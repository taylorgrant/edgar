

get_form4_disclosures <- function(date) {
  
  pacman::p_load(tidyverse, edgar, here, glue, janitor, qdapRegex, progress)
  source(here('R', "getFilingsLimited.R"))
  # load contextual data ----------------------------------------------------
  
  trade_table <- tibble(code = c("P", "S", "A", "D", "G",
                                 "F", "M", "X", "C", "W", "J"),
                        full = c("P - Purchase", "S - Sale", "A - Grant",
                                 "D - Sale to Iss", "G - Gift", "F - Tax",
                                 'M - Option Ex', "X - Option Ex", "C - Cnv Deriv",
                                 "W - Inherited", 
                                 "J - Other"))
  
  sic_codes <- readRDS(here('data', "sic_codes.RDS"))
  
  # get master index and filings --------------------------------------------
  
  # date of interest
  filing_date <- date
  
  # master data
  tmp <- getDailyMaster(filing_date)
  
  # check to make sure didn't return null for weekend or holiday
  if (is.null(tmp) || str_detect(tmp$company.name, "<Error>")) {
    cat(paste0(filing_date, " is either a holiday or weekend. Adding 1 day...\n"))
    filing_date <- filing_date + 1
    tmp <- getDailyMaster(filing_date)
  }
  
  # keep only Form 4 filings
  tmp4 <- tmp %>% filter(form.type == 4)
  
  # pass these values through the function
  year <- lubridate::year(filing_date) %>% as.numeric()
  qrtr <- lubridate::quarter(filing_date) %>% as.numeric()
  form_type <- "4"
  
  # pull the CIK (Central Index Key) from filings
  cik <- tmp4 %>% distinct(cik) %>% pull %>% as.character()
  
  # download the filings (using LIMITED pull)  
  output <- getFilingsLimited(cik.no = cik, form.type = form_type, 
                       filing.date = filing_date,
                       filing.year = year, quarter = c(qrtr), downl.permit = "y") %>% 
    mutate_if(is.factor, as.character)
  
  # read the filings and summarise ------------------------------------------
  
  form4_tabulate <- function(cik, accession.number, date.filed) {
    
    ## progress bar ##
    pb$tick()
    
    filename <- paste0("Edgar filings_full text/Form ", 
                       form_type, "/", cik, "/", cik, "_", 
                       form_type, "_", date.filed, "_", accession.number, 
                       ".txt")
    tmpfile <- readLines(filename)
    tmpfile <- str_trim(tmpfile)
    
    ## filing date ## 
    filing_date <- str_replace_all(tmpfile[which(str_detect(tmpfile, "ACCEPTANCE-DATETIME"))],
                                   "<ACCEPTANCE-DATETIME>", "")
    filing_date <- lubridate::as_datetime(filing_date)
    
    ## non-derivative table ## 
    nd_loc <- which(str_detect(tmpfile, "nonDerivativeTable"))
    nd <- tmpfile[nd_loc[1]:nd_loc[2]]
    
    ## transaction date ## 
    date <- which(str_detect(nd, "transactionDate"))
    trade_date <- str_replace_all(nd[date[c(TRUE,FALSE)] + 1], 
                                  "<value>|</value>", "") 
    
    ## stock ticker ## 
    stock_ticker <- str_replace_all(tmpfile[which(str_detect(tmpfile, "issuerTradingSymbol"))],
                                    "<issuerTradingSymbol>|</issuerTradingSymbol>" ,"")
    
    ## company name ## 
    names <- tmpfile[grep("COMPANY CONFORMED NAME", 
                          tmpfile, ignore.case = T)]
    names <- gsub("COMPANY CONFORMED NAME:|\t", 
                  "", names, ignore.case = T)
    company_name <- names[2]
    
    ## insider name ## 
    insider_name <- names[1]
    
    ## insider position ## 
    reporting_loc <- which(str_detect(tmpfile, "reportingOwnerRelationship"))
    ro <- tmpfile[reporting_loc[1]:reporting_loc[2]]
    ro <- ro[grep("1", ro, ignore.case = T)] %>%
      str_trim()
    ro <- rm_between(ro, '<', '>', extract=TRUE)[[1]][1]
    ro <- str_replace_all(ro, "is", "")  
    
    ## officer title 
    if (ro != "Officer") {
      officer_title <- ro
    } else {
      officer_title <- tmpfile[grep("officerTitle", tmpfile, ignore.case = T)]
      officer_title <- rm_between(officer_title, '>', '<', extract=TRUE)[[1]]
      officer_title <- str_replace_all(officer_title, "amp|,|;", "")
    }
    
    ## trade type ##
    trade_type <- str_replace_all(nd[grep("transactionCode", 
                                          nd, ignore.case = T)], "<transactionCode>|</transactionCode>", "")
    
    ## price ## 
    price_loc <- which(str_detect(nd, "transactionPricePerShare"))
    price <- str_replace_all(nd[price_loc[c(TRUE,FALSE)] + 1], 
                             "<value>|</value>", "") 
    
    ## quantity ## 
    quantity_loc <- which(str_detect(nd, "transactionShares"))
    quantity <- str_replace_all(nd[quantity_loc[c(TRUE,FALSE)] + 1], 
                                "<value>|</value>", "")
    
    ## owned ## 
    owned_loc <- which(str_detect(nd, "sharesOwned"))
    owned <-  str_replace_all(nd[owned_loc[c(TRUE,FALSE)] + 1], 
                              "<value>|</value>", "")
    
    if (length(owned) > length(trade_date)) {
      l <- last(owned)
      owned <- owned[1:length(trade_date)]
      # owned[length(trade_date)] <- l
    }
    
    ## bought or sold ## 
    type_loc <- which(str_detect(nd, "transactionAcquiredDisposedCode"))
    type <-  str_replace_all(nd[type_loc[c(TRUE,FALSE)] + 1], 
                             "<value>|</value>", "")
    
    ## SIC ## 
    sic <- tmpfile[grep("STANDARD INDUSTRIAL CLASSIFICATION", 
                        tmpfile, ignore.case = T)]
    sic <- rm_between(sic, '[', ']', extract=TRUE)[[1]]
    
    ## put it together ## 
    tmp_df <- tibble(cik = rep(cik, length(trade_date)),
                     filing_date = rep(filing_date, length(trade_date)), 
                     trade_date, 
                     stock_ticker = rep(stock_ticker, length(trade_date)), 
                     company_name = rep(company_name, length(trade_date)),
                     insider_name = rep(insider_name, length(trade_date)), 
                     insider_title = rep(officer_title, length(trade_date)),
                     trade_type, price, quantity, owned, type, sic = rep(sic, length(trade_date)),
                     access_number = rep(accession.number, length(trade_date))) %>%
      mutate(across(.cols = c(price:owned), .fns = as.numeric),
             delta_owned = ifelse(type == "A", quantity/(owned-quantity),
                                  (owned - (quantity+owned))/(quantity+owned)),
             quantity = ifelse(type == "D", quantity*-1, quantity),
             value = quantity*price) %>% 
      left_join(select(sic_codes, sic, industry, division), by = c("sic" = "sic")) %>% 
      left_join(trade_table, by = c("trade_type" = "code")) %>% 
      select(cik, filing_date, trade_date, stock_ticker, company_name, insider_name, insider_title,
             trade_type = full, price, quantity, owned, value, type, sic, 
             delta_owned, industry, division, access_number) %>%
      mutate(price = ifelse(is.na(price), 0, price),
             value = ifelse(is.na(value), 0, value)) %>%
      mutate(tmpid = ifelse(trade_type %in% c('M - Option Ex', 'X - Option Ex'), 
                            1, NA)) %>% 
      group_by(access_number) %>% 
      fill(tmpid) %>% 
      mutate(tmpid = ifelse(is.na(tmpid), 0, tmpid)) %>% 
      mutate(trade_type = ifelse(tmpid == 1 & trade_type == "S - Sale", 'S - Sale+OE', trade_type)) %>%
      ungroup %>% 
      select(-tmpid)
    
  }
  
  # wrap function in possibly for safety 
  try_form4_tabulate <- possibly(form4_tabulate, otherwise = NA_character_)
  
  cat("Parsing Form 4 filings now ...\n")
  pb <- progress_bar$new(total =  nrow(output))
  disclosures <- pmap(list(output$cik, output$accession.number, output$date.filed), try_form4_tabulate)
  
  # now put it all together 
  disclosures <- disclosures[!is.na(disclosures)] # drop NA
  disclosures <- Filter(nrow, disclosures) # filter empty tibbles
  disclosures <- disclosures %>% 
    reduce(bind_rows)
  
  saveRDS(disclosures, glue(here("data", "disclosures","form4_disclosure_{date}.RDS")))
  
  ## if want to delete files... ## 
  # unlink(here("Daily Indexes"), recursive = TRUE)
  # unlink(here("Master Indexes"), recursive = TRUE)
  # unlink(here("Edgar filings_full text"), recursive = TRUE)
  
}


# example  ----------------------------------------------------------------

# library(tidyverse)  

# date <- seq.Date(as.Date("2020-06-13"), as.Date("2020-06-20"), "days")
# dates <- tibble(date = date) %>%
#   mutate(dow = lubridate::wday(date, label = TRUE)) %>%
#   filter(!dow %in% c("Sat", "Sun")) %>%
#   pull(date)
# 
# dates %>%
#   walk(get_form4_disclosures)




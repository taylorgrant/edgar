# cleaning up data and duplicates  ----------------------------------------

clean_form4 <- function(dat) {
  
  ## function to de-duplicate the reports 
  deduplicate <- function(dat) {
    dat <- dat %>% 
      group_by(access_number) %>%
      mutate(var1 = cumsum(!duplicated(cik))) %>% 
      filter(var1 == 1) %>% 
      ungroup() %>% 
      select(-var1)
  } 
  
  ## function to clean up the tickers that are pulled in ##
  clean_ticker <- function(dat) {
    dat <- dat %>%
      mutate(stock_ticker = gsub(".*\\: ", "", stock_ticker),
             stock_ticker = gsub("\\-.*", "", stock_ticker),
             stock_ticker = gsub("\\ .*", "", stock_ticker),
             stock_ticker = gsub("\\,.*", "", stock_ticker),
             stock_ticker = gsub("\\_.*", "", stock_ticker),
             stock_ticker = str_replace_all(stock_ticker, "\\(|\\)", ""),
             stock_ticker = toupper(stock_ticker))
  }
  
  ## function to clean insider titles 
  inside_titles <- function(dat) {
    dat <- dat %>% 
      mutate(insider_title = str_replace_all(insider_title, "\\*", ""),
             insider_title = str_replace_all(insider_title, fixed(" and ", ignore_case = TRUE), ", "),
             insider_title = str_replace_all(insider_title, fixed(" & ", ignore_case = TRUE), ", "),
             insider_title = str_replace_all(insider_title, fixed("& ", ignore_case = TRUE), ", "),
             insider_title = str_replace_all(insider_title, fixed("executive vice chairman", ignore_case = TRUE), "CBO"),
             insider_title = str_replace_all(insider_title, fixed("vice chairman", ignore_case = TRUE), "CBO"),
             insider_title = str_replace_all(insider_title, fixed("chairman", ignore_case = TRUE), "CBO"),
             insider_title = str_replace_all(insider_title, fixed("chair", ignore_case = TRUE), "CBO"),
             insider_title = str_replace_all(insider_title, fixed("vice president", ignore_case = TRUE), "VP"),
             insider_title = str_replace_all(insider_title, fixed("vice pres.", ignore_case = TRUE), "VP"),
             insider_title = str_replace_all(insider_title, fixed("vice pres", ignore_case = TRUE), "VP"),
             insider_title = str_replace_all(insider_title, fixed("executive vp", ignore_case = TRUE), "EVP"),
             insider_title = str_replace_all(insider_title, fixed("executive vp", ignore_case = TRUE), "EVP"),
             insider_title = str_replace_all(insider_title, fixed("exec. vp", ignore_case = TRUE), "EVP"),
             insider_title = str_replace_all(insider_title, fixed("exec vp", ignore_case = TRUE), "EVP"),
             insider_title = str_replace_all(insider_title, fixed("ex vp", ignore_case = TRUE), "EVP"),
             insider_title = str_replace_all(insider_title, fixed("senior vp", ignore_case = TRUE), "SVP"),
             insider_title = str_replace_all(insider_title, fixed("sr. vp", ignore_case = TRUE), "SVP"),
             insider_title = str_replace_all(insider_title, fixed("senior vp", ignore_case = TRUE), "SVP"),
             insider_title = str_replace_all(insider_title, fixed("sr vp", ignore_case = TRUE), "SVP"),
             insider_title = str_replace_all(insider_title, fixed("president", ignore_case = TRUE), "Pres"),
             insider_title = str_replace_all(insider_title, fixed("chief executive officer", ignore_case = TRUE), "CEO"),
             insider_title = str_replace_all(insider_title, fixed("chief financial officer", ignore_case = TRUE), "CFO"),
             insider_title = str_replace_all(insider_title, fixed("chief operating officer", ignore_case = TRUE), "COO"),
             insider_title = str_replace_all(insider_title, fixed("general counsel", ignore_case = TRUE), "GC")) 
  }
  tmp <- deduplicate(dat)
  tmp <- clean_ticker(tmp)
  tmp <- inside_titles(tmp)
}
## read and compile ## 

compile_form4 <- function() {
  pacman::p_load(tidyverse, here)
  
  data_path <- here('data', 'disclosures')
  files <- dir(data_path, pattern = "*.RDS") # get file names
  
  df <- files %>%
    # read in all the files, appending the path before the filename
    map(~ readRDS(file.path(data_path, .))) %>% 
    reduce(bind_rows) %>% 
    mutate(trade_date = as.Date(trade_date))

  tmp_dat <- readRDS(here('data', 'edgar_form4.RDS'))
  
  tmp_dat <- tmp_dat %>% 
    bind_rows(df) %>% 
    saveRDS(here('data', 'edgar_form4.RDS'))
}


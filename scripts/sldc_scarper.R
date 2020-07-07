pacman::p_load(data.table, tabulizer, lubridate, RCurl)
setwd("C:/Users/tbowen/Desktop")

dir_base <- "temp_sldc_pdfs"
url_base <- "https://sldc.rajasthan.gov.in/mis/getReDsmReportForTitle?reportReDsmTitles=%s-%s_RE+CURTAILMENT+FOR+DT.+%s&rep_name=%s-%s_RE+CURTAILMENT+FOR+DT.+%s&rep_type=monthly"

source("temp_sldc_pdfs/scripts/url_check.R")
source("temp_sldc_pdfs/scripts/logger.R")
source("temp_sldc_pdfs/scripts/scraper_function.R")
source("temp_sldc_pdfs/scripts/data_check.R")

dates_test <- seq(as.Date("2020-05-01"), as.Date("2020-05-31"), by="days")

# ------------------------------------------------------------ %
# ~~~~~~~~~ Check urls are valid ~~~~~~~~~~~~~
# ------------------------------------------------------------ %

urls_dates <- lapply(dates_test, url_selector)
urls_dates <- setNames(urls_dates, dates_test)

# ------------------------------------------------------------ %
# ~~~~~~~~~ Scrape tables for vlaid urls ~~~~~~~~~~~~~
# ------------------------------------------------------------ %
urls_results <- lapply(names(urls_dates), function(x){urls_scraper(url_day = x, urls_list = urls_dates)})

# ------------------------------------------------------------ %
# ~~~~~~~~~ Collate data ~~~~~~~~~~~~~
# ------------------------------------------------------------ %
# use regex to find exact files to pull together
files_collate <- list.files(file.path(dir_base, 'outputs'), pattern = "^temp_sldc_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv$")

#combine csv's into single data.table
curtail_data <- rbindlist(lapply(files_collate, function(file_day, db = dir_base){

    return(fread(file.path(db, 'outputs', file_day)))  
  
}),use.names = TRUE, fill = FALSE)

# ------------------------------------------------------------ %
# ~~~~~~~~~ run checks ~~~~~~~~~~~~~
# ------------------------------------------------------------ %

check_time_periods()
check_data_format()

# ------------------------------------------------------------ %
# ~~~~~~~~~ save collated data ~~~~~~~~~~~~~
# ------------------------------------------------------------ %
fwrite(curtail_data, file = file.path(dir_base, "Collated_Curtailment_Data.csv"))

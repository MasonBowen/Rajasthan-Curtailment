pacman::p_load(data.table, tabulizer, lubridate, RCurl)
setwd("C:/Users/cwayner/Documents/")

# Error 1 - no plus sign after DT (5/30/20, 5/6/20, 5/18/20, 5/31/20), corrected
# Error 2 - 5/10/20 stored at 5/8/20 link, 5/8/20 link missing 3rd and 6th dates - MANUAL
# Error 3 - 5/13/20 has 13-04-20 stored as its 3rd and 6th dates, corrected  
# Error 4 - no data for 5/28/20
# Error 5 - Y instead of y for 3rd and 6th dates, corrected
# Error 6 - FOOR instead of FOR, 4/13/20, corrected
# Error 7 - FOR is missing from URL, 4/22/20, corrected
# Error 8 - 3/9/20 has O instead of 0 - MANUAL
# Error 9 - 3/21/20 has 3/20/20 as 3rd and 6th dates, corrected

## Renewable curtailment data goes back to August 2019, don't check beyond there.

dir_base <- "Rajasthan-Curtailment"
url_base <- "https://sldc.rajasthan.gov.in/mis/getReDsmReportForTitle?%s=%s-%s_RE+CURTAILMENT+%s%s&rep_name=%s-%s_RE+CURTAILMENT+%s%s&rep_type=monthly"

source("Rajasthan-Curtailment/scripts/url_check.R")
source("Rajasthan-Curtailment/scripts/logger.R")
source("Rajasthan-Curtailment/scripts/scraper_function.R")
source("Rajasthan-Curtailment/scripts/data_check.R")

dates_test <- seq(as.Date("2019-12-01"), as.Date("2019-12-31"), by="days")

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

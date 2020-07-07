# Logging files
#create logging file with timestamp to append errors/warning messages to
systime <- Sys.time()

# for errors with checking urls
log_file_path <-  file.path(dir_base, "log", sprintf("logging_urls_%s.txt", format(systime, "%d%m%Y-%H%M%S")))
fileConn<-file(log_file_path)
writeLines(c("File created to capture warning/error messages with teting urls", 
             paste0("File created on: ", systime)), fileConn)
close(fileConn)

# for tracking missing urls
emptydata_file_path <-  file.path(dir_base, "log", sprintf("missingdata_%s.txt", format(systime, "%d%m%Y-%H%M%S")))
fileConn<-file(emptydata_file_path)
writeLines(c("File created to capture missing data", 
             paste0("File created on: ", systime),
             "Dates with missing data: "), fileConn)
close(fileConn)

# for errors with actual scraping
log_tablescrape_file_path <-  file.path(dir_base, "log", sprintf("logging_tablescrape_%s.txt", format(systime, "%d%m%Y-%H%M%S")))
fileConn<-file(log_tablescrape_file_path)
writeLines(c("File created to capture warning/error messages with scraping tables", 
             paste0("File created on: ", systime)), fileConn)
close(fileConn)
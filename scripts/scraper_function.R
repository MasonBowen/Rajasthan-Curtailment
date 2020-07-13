# actually scraps data from tables with legitimate urls

urls_scraper <- function(url_day, urls_list = urls_dates, ubase = url_base, db = dir_base, lfp = log_tablescrape_file_path){
  
  #format original url with dates, build save path for pdf
  destfile <- file.path(db, 'outputs', sprintf("temp_sldc_%s.csv", url_day))
  
  # see here for trycatch in R: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  
  out <- tryCatch(
    expr = {
      
      x = tabulizer::extract_tables(urls_list[[url_day]])[[1]]
      
      xsub <- x[(nrow(x)-96):(nrow(x)-1),]
      xsub <- setDT(data.frame(xsub))
      
      # 2020, else 2019
      if (ncol(x) == 13) {
        xsub[, c("Block No.", "Time-Block", "Schedule before Curtailment", "Actual SCADA", "Wind Curtailment Max MW", "Wind Curtailment Energy") := tstrsplit(X1, " ")]
        xsub[, c("Solar Curtailment Max MW", "Solar Curtailment Energy") := tstrsplit(X2, " ")]
        xsub[, c("Backing down of own thermal generation (MW)", "Reduction in drwal from Centeral Sector") := tstrsplit(X4, " ")]
        
        xsub[, `:=` (X1 = NULL, X2 = NULL, X4 = NULL)]
        setnames(xsub, old = c("X3", "X5", "X6"), new = c("Reason for curtailment", "Frequency", "Demand"))
        
        xsub[, Date := url_day, ]
        
        setcolorder(xsub, c("Date", "Block No.", "Time-Block", "Schedule before Curtailment", "Actual SCADA",
                            "Wind Curtailment Max MW", "Wind Curtailment Energy", "Solar Curtailment Max MW", 
                            "Solar Curtailment Energy", "Reason for curtailment", "Backing down of own thermal generation (MW)", 
                            "Reduction in drwal from Centeral Sector", "Frequency", "Demand"))
        
      } else if (ncol(x) == 9) {
        xsub[, c("Block No.", "Time-Block") := tstrsplit(X1, " ")]
        xsub[, c("Actual SCADA", "Wind Curtailment Max MW", "Wind Curtailment Energy", 
                 "Solar Curtailment Max MW", "Solar Curtailment Energy") := tstrsplit(X3, " ")]
        
        xsub[, `:=` (X1 = NULL, X3 = NULL)]
        setnames(xsub, old = c("X2", "X4"), new = c("Schedule before Curtailment", "Reason for curtailment"))
        
        xsub[, Date := url_day, ]
        
        setcolorder(xsub, c("Date", "Block No.", "Time-Block", "Schedule before Curtailment", "Actual SCADA",
                            "Wind Curtailment Max MW", "Wind Curtailment Energy", "Solar Curtailment Max MW", 
                            "Solar Curtailment Energy", "Reason for curtailment"))
      } else {
        setnames(xsub, old = c("X1", "X2", "X3", "X4", "X5", "X6"), new = c("Block No.", "Time-Block", "Schedule before Curtailment", 
                                                                            "Actual SCADA", "Curtailment MW", "Reason for curtailment"))
        
        xsub[, Date := url_day, ]
        
        setcolorder(xsub, c("Date", "Block No.", "Time-Block", "Schedule before Curtailment", "Actual SCADA",
                            "Curtailment MW", "Reason for curtailment"))
      }
      
      fwrite(xsub, file = destfile, sep = ",", na = "")
      "Successfully saved csv"
      
    },
    error = function(e){ 
      # (Optional)
      # Do this if an error is caught...
      message(paste("Issue scraping table for:", url_day, " , perhaps url does not exist"))
      message("Here's the original error message:")
      message(e)
      
      return(as.character(e))
    },
    # other main date format I saw
    warning = function(w){
      # (Optional)
      # Do this if an warning is caught...
      message(paste("Table scraping produced warning:", url_day))
      message("Here's the original warning message:")
      message(w)
      
      return(as.character(w))
    },
    
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
      message(paste("Finished with orig. run for :", url_day))
    }
  )
  
  write(sprintf("\nResults for pdf file '%s':", url_day), lfp, append=TRUE)
  write(out, lfp, append=TRUE)
  
  return(out)
  
}

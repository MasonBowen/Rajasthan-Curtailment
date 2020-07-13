url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need these two functions if you're already using `purrr`
  # but `purrr` is a heavyweight compiled package that introduces
  # many other "tidyverse" dependencies and this doesn't
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(x)
    
  } else {
    return(x)
  }
  
}

url_selector <- function(date_str, ub = url_base, lfp = log_file_path, edfp = emptydata_file_path){
  
  d1 <- format(date_str, format = "%d%m%Y")
  d2 <- format(date_str, format = "%d-%m-%y")
  d3 <- format(date_str, format = "%d.%m.%y")
  d4 <- format(date_str, format = "%d-%m-2%y")
  d5 <- format(date_str, format = "%d.%m-%y")
  d6 <- format(date_str, format = "%d-%m-%Y")
  d7 <- format(date_str - 1, format = "%d-%m-%y")
  d8 <- format(date_str %m-% months(1), format = "%d-%m-%y")
  d9 <- format(date_str, format = "%d-%m-%y+")
  
  plus <- "_RE+CURTAILMENT+FOR+DT.+"
  noplus <- "_RE+CURTAILMENT+FOR+DT."
  nofor <- "_RE+CURTAILMENT+DT.+"
  nodot <- "_RE+CURTAILMENT+FOR+DT+"
  foor <- "_RE+CURTAILMENT+FOOR+DT.+"
  doubleplus <- "_RE+CURTAILMENT+FOR++DT.+"
  nodt <- "_RE+CURTAILMENT+FOR+"
  empty <- ""
  dated <- "_RE+CURTAILMENT+for+dated+"
  DATED <- "_RE+CURTAILMENT+FOR+DATED+"
  
  
  # use newpre (prefix) for 2020-04-01 and after, otherwise use oldpre
  newpre <- "reportReDsmTitles"
  oldpre <- "weeklydsm"


  if (date_str > as.Date("2020-03-31")) {
    url_list = list(t1 = sprintf(ub, newpre, d1, d1, plus, d2, d1, d1, plus, d2),
                    t2 = sprintf(ub, newpre, d1, d1, plus, d3, d1, d1, plus, d3),
                    t3 = sprintf(ub, newpre, d1, d1, plus, d4, d1, d1, plus, d4),
                    t4 = sprintf(ub, newpre, d1, d1, plus, d5, d1, d1, plus, d5),
                    t5 = sprintf(ub, newpre, d1, d1, plus, d6, d1, d1, plus, d6),
                    t6 = sprintf(ub, newpre, d1, d1, noplus, d2, d1, d1, noplus, d2),
                    t7 = sprintf(ub, newpre, d1, d1, nofor, d2, d1, d1, nofor, d2),
                    t8 = sprintf(ub, newpre, d1, d1, foor, d2, d1, d1, foor, d2),
                    t9 = sprintf(ub, newpre, d1, d1, plus, d7, d1, d1, plus, d7),
                    t10 = sprintf(ub, newpre, d1, d1, plus, d8, d1, d1, plus, d8),
                    t11 = sprintf(ub, newpre, d1, d1, doubleplus, d2, d1, d1, doubleplus, d2),
                    t12 = sprintf(ub, newpre, d1, d1, nodot, d2, d1, d1, nodot, d2),
                    t13 = sprintf(ub, newpre, d1, d1, nodt, d9, d1, d1, nodt, d9),
                    t14 = sprintf(ub, newpre, d1, d1, empty, empty, d1, d1, empty, empty),
                    t15 = sprintf(ub, newpre, d1, d1, dated, d2, d1, d1, dated, d2),
                    t16 = sprintf(ub, newpre, d1, d1, DATED, d2, d1, d1, DATED, d2)) 
  } else {
    url_list = list(t1 = sprintf(ub, oldpre, d1, d1, plus, d2, d1, d1, plus, d2),
                    t2 = sprintf(ub, oldpre, d1, d1, plus, d3, d1, d1, plus, d3),
                    t3 = sprintf(ub, oldpre, d1, d1, plus, d4, d1, d1, plus, d4),
                    t4 = sprintf(ub, oldpre, d1, d1, plus, d5, d1, d1, plus, d5),
                    t5 = sprintf(ub, oldpre, d1, d1, plus, d6, d1, d1, plus, d6),
                    t6 = sprintf(ub, oldpre, d1, d1, noplus, d2, d1, d1, noplus, d2),
                    t7 = sprintf(ub, oldpre, d1, d1, nofor, d2, d1, d1, nofor, d2),
                    t8 = sprintf(ub, oldpre, d1, d1, foor, d2, d1, d1, foor, d2),
                    t9 = sprintf(ub, oldpre, d1, d1, plus, d7, d1, d1, plus, d7),
                    t10 = sprintf(ub, oldpre, d1, d1, plus, d8, d1, d1, plus, d8),
                    t11 = sprintf(ub, oldpre, d1, d1, doubleplus, d2, d1, d1, doubleplus, d2),
                    t12 = sprintf(ub, oldpre, d1, d1, nodot, d2, d1, d1, nodot, d2),
                    t13 = sprintf(ub, oldpre, d1, d1, nodt, d9, d1, d1, nodt, d9),
                    t14 = sprintf(ub, oldpre, d1, d1, empty, empty, d1, d1, empty, empty),
                    t15 = sprintf(ub, oldpre, d1, d1, dated, d2, d1, d1, dated, d2),
                    t16 = sprintf(ub, oldpre, d1, d1, DATED, d2, d1, d1, DATED, d2))
  }
  
  #most common, check first before iterating since it is cumbersome/slow
  result = url_exists(url_list[['t1']],non_2xx_return_value = NA, quiet = TRUE)  
  
  if(is.na(result)){
    
    result = tryCatch({
      lapply(url_list[c("t2","t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16")], 
             function(x){url_exists(x,non_2xx_return_value = NA, quiet = TRUE)})
    }, warning = function(w2) {
      message(sprintf("URLs for %s produced warning!", d1))
      message("Here's the original warning message:")
      message(w2)
      return(w2)
    }, error = function(e2) {
      message(sprintf("URLs produced error!", d1))
      message("Here's the original error message:")
      message(e2)
      return(e2)
    })
    
  }
  
  write(sprintf("\nResults for urls for '%s':", d1), lfp, append=TRUE)
  write(as.character(result[!is.na(result)]), lfp, append=TRUE)
  
  # result_reduced = tryCatch({
  #   as.character(result[!is.na(result)])
  # }, warning = function(w2) {
  #   message(sprintf("Something wrong with 'results' for ", d1))
  #   message("Here's the original warning message:")
  #   message(w2)
  # }, error = function(e2) {
  #   message(sprintf("Something wrong with 'results' for ", d1))
  #   message("Here's the original error message:")
  #   message(e2)
  # })
  
  if(length(as.character(result[!is.na(result)])) ==0){
    write(d1, edfp, append=TRUE)
    }
  
  return(as.character(result[!is.na(result)]))
  
}

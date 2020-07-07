url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
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

  
  url_list = list(t1 = sprintf(ub, d1, d1, d2, d1, d1, d2),
                  t2 = sprintf(ub, d1, d1, d3, d1, d1, d3),
                  t3 = sprintf(ub, d1, d1, d4, d1, d1, d4),
                  t4 = sprintf(ub, d1, d1, d5, d1, d1, d5))
  
  #most common, check first before iterating since it is cumbersome/slow
  result = url_exists(url_list[['t1']],non_2xx_return_value = NA, quiet = TRUE)
  
  if(is.na(result)){
    
    result = tryCatch({
      lapply(url_list[c("t2","t3", "t4")], function(x){url_exists(x,non_2xx_return_value = NA, quiet = TRUE)})
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

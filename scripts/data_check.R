# this is just to give you an idea of the issues you might run into, not at all exhaustive

check_time_periods <- function(dt = curtail_data){
  
  check_dt = dt[, .(Entries = .N), by = .(Date)]
  
  if(!all(check_dt$Entries == 96)){
    return(warning(sprintf("See following dates without 96 entries:\n %s", 
                    paste(check_dt[Entries != 96, Date], collapse = "; "))))
  }else{
    return("All dates have valid number of entries")
  }
  
}

check_data_format <- function(dt = curtail_data){
  
  data_type_ref = list(
    "Date" = "character",
    "Block No." = "integer",
    "Time-Block" = "character",
    "Schedule before Curtailment" = "numeric",
    "Actual SCADA" = "integer",
    "Wind Curtailment Max MW" = "integer",
    "Wind Curtailment Energy" = "integer",
    "Solar Curtailment Max MW" = "integer",
    "Solar Curtailment Energy" = "integer",
    "Reason for curtailment" = "logical",
    "Backing down of own thermal generation (MW)" = "numeric",
    "Reduction in drwal from Centeral Sector" = "numeric",
    "Frequency" = "numeric",
    "Demand" = "integer"
  )
  
  data_types <- lapply(dt, class)
  
  checks = setNames(unlist(lapply(names(data_type_ref), function(nm, ref = data_type_ref, test = data_types){
    
    return(ref[[nm]] == test[[nm]])
    
  })), nm = names(data_type_ref))
  
  if(!all(checks)){
    return(warning(sprintf("See following columns with innapropriate data types:\n %s", 
                           paste(unlist(lapply(names(checks), function(nm, lst = checks, ref = data_type_ref, test = data_types){
                             if(!lst[[nm]]){
                               return(sprintf("Column '%s' should be '%s' but is actually '%s'", nm, ref[[nm]], test[[nm]]))
                             }
                           })), collapse = '\n'))))
  }else{
    return("All data types appear correct")
  }
  
  
}

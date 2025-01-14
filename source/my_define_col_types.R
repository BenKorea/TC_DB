my_define_col_types <- function(total, date, numeric) {
  
  col_types <- rep("text", total)
  col_types[date] <- "date"
  col_types[numeric] <- "numeric"
  
  return(col_types)
}
################################################################################
## my_functions_for_response
################################################################################
my_parsing_response_line <- function(dt) {
  
  dt[, response_date := str_extract(특기사항, "\\d{4}-\\d{2}-\\d{2}")]
  dt[, response_line := str_remove(특기사항, "\\d{4}-\\d{2}-\\d{2}")]
  error <- dt[is.na(response_date)]
  error[,response_date := str_extract(response_line, "\\d{4}-\\w{2}-dd")]
  error[,response_line := str_remove(response_line, "\\d{4}-\\w{2}-dd")]
  error[,response_date := str_replace(response_date, "mm","06")]
  error[,response_date := str_replace(response_date, "dd","15")]
  error[,response_date_imputed := TRUE]
  dt <- dt[!is.na(response_date)]
  dt[,response_date_imputed := FALSE]
  dt<-rbind(dt,error)
  dt[, response_date := as.IDate(response_date)]
  error_response_date <<- dt[is.na(response_date)]
  dt <- dt[!is.na(response_date)]

  return(dt)
}

my_mutate_recur_date <- function(dt) {
  
  dt$Recur<- ifelse(dt$Response=="Structural","Y","N")
  
  return(dt)
}

my_parsing_response_data<-function(dt) {
  
  followup_data <- dt[분류명 %in% c("Excellent","Indeterminate","Biochemical","Structural")]
  setnames(followup_data, "분류명", "Response")
  followup_data[, 등록일 := as.Date(등록일, format="%Y-%m-%d")]

  followup_data <- my_parsing_response_line (followup_data)
  
  
  return(followup_data)
  
}

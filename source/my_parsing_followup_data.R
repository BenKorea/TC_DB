################################################################################
## my_functions_for_followup
################################################################################
my_parsing_response_line <- function(dt) {
  
  dt$response_date <- tstrsplit(dt$특기사항, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[1]]
  
  return(dt)
}

my_mutate_recur_date <- function(dt) {
  
  dt$Recur<- ifelse(dt$Response=="Structural","Y","N")
  
  return(dt)
}

my_parsing_followup_data<-function(dt) {
  
  followup_data <- dt[분류명 %in% c("Transfer","follow up loss","expire","요양병원","호스피스")]
  setnames(followup_data, "분류명", "FollowUp")
  followup_data[, 등록일 := as.Date(등록일, format="%Y-%m-%d")]
  
  
  return(followup_data)
  
}

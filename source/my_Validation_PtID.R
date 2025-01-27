my_Validation_PtID <- function(dt) {

  result <- list()
  
  invalid_data <- dt[!(nchar(PtID) >= 5 & nchar(PtID) <= 8 & grepl("^[0-9]+$", PtID))]
  valid_data <- dt[nchar(PtID) >= 5 & nchar(PtID) <= 8 & grepl("^[0-9]+$", PtID)]

  valid_data[nchar(PtID) == 5, PtID := paste0("000", PtID)]
  valid_data[nchar(PtID) == 6, PtID := paste0("00", PtID)]
  valid_data[nchar(PtID) == 7, PtID := paste0("0", PtID)]
    
  invalid_data_PtID_NA <<- invalid_data[is.na(PtID)]
  invalid_data <- invalid_data[!is.na(PtID)]
    
  setcolorder(dt, c("PtID", setdiff(names(valid_data), "PtID")))
    
  result$valid_data <- valid_data
  result$invalid_data <- invalid_data
    
  return(result)
}


################################################################################
## 수기로 입력된 환자정보의 경우 숫자로 구성된 8자리가 지켜지지 않는 경우가 있다.
## 입력오류를 검증하는 것과 오류데이터를 어떻게 반환할지가 고민이다.


my_validation_pt_id <- function(dt) {

  result <- list()
  
  invalid_data <- dt[!(nchar(pt_id) >= 5 & nchar(pt_id) <= 8 & grepl("^[0-9]+$", pt_id))]
  valid_data <- dt[nchar(pt_id) >= 5 & nchar(pt_id) <= 8 & grepl("^[0-9]+$", pt_id)]

  valid_data[nchar(pt_id) == 5, pt_id := paste0("000", pt_id)]
  valid_data[nchar(pt_id) == 6, pt_id := paste0("00", pt_id)]
  valid_data[nchar(pt_id) == 7, pt_id := paste0("0", pt_id)]
    
  invalid_data_pt_id_is_na <<- invalid_data[is.na(pt_id)]
  invalid_data <- invalid_data[!is.na(pt_id)]
    
  setcolorder(dt, c("pt_id", setdiff(names(valid_data), "pt_id")))
    
  result$valid_data <- valid_data
  result$invalid_data <- invalid_data
    
  return(result)
}


################################################################################
## 수기로 입력된 환자정보의 경우 숫자로 구성된 8자리가 지켜지지 않는 경우가 있다.
## 입력오류를 검증하는 것과 오류데이터를 어떻게 반환할지가 고민이다.


my_PtID <- function(dt) {
  # 결과 저장을 위한 리스트
  result <- list()
  
  # "등록번호"를 "PtID"로 변경
  if ("등록번호" %in% colnames(dt)) {
    setnames(dt, old = "등록번호", new = "PtID")
  }
  
  # PtID 유효성 점검: 5자에서 8자 사이의 숫자로 구성된 값 필터링
  invalid_data <- dt[!(nchar(PtID) >= 5 & nchar(PtID) <= 8 & grepl("^[0-9]+$", PtID))]
  
  # 유효한 데이터만 유지
  valid_data <- dt[nchar(PtID) >= 5 & nchar(PtID) <= 8 & grepl("^[0-9]+$", PtID)]
  
  # 유효한 데이터에 대해 PtID 길이에 따른 처리
  valid_data[nchar(PtID) == 5, PtID := paste0("000", PtID)]
  valid_data[nchar(PtID) == 6, PtID := paste0("00", PtID)]
  valid_data[nchar(PtID) == 7, PtID := paste0("0", PtID)]
  
  # PtID를 첫 번째 열로 이동
  setcolorder(valid_data, c("PtID", setdiff(names(valid_data), "PtID")))
  
  # 결과 저장
  result$valid_data <- valid_data
  result$invalid_data <- invalid_data
  
  return(result)
}


################################################################################
## 수기로 입력된 환자정보의 경우 숫자로 구성된 8자리가 지켜지지 않는 경우가 있다.
## 입력오류를 검증하는 것과 오류데이터를 어떻게 반환할지가 고민이다.


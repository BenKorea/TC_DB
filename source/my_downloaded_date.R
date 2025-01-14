my_downloaded_date <- function(directory, pattern) {
  
  file_name <- list.files(directory, pattern = pattern)
  # 다운로드 날자를 추출하여 birthday input error를 확인하기 위해 사용할 예정
  date_string <- substr(file_name, 6, 13)
  downloaded_date <- as.Date(date_string, format = "%Y%m%d")
  
  return(downloaded_date)
}
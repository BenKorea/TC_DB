load_levothyroxine_data <- function(folder_path, pattern) {
  
  excel_file_names <- list.files(folder_path, pattern = pattern)
  file_data <- NULL
  
  # 파일 하나씩 읽기
  for (file_name in excel_file_names) {
    file_path <- file.path(raw_data_path, file_name)  # 경로 수정
    
    # 파일 읽기 및 데이터 결합
    temp <- read_excel(file_path, col_types = "text") %>% as.data.table()
    setnames(temp, old = colnames(temp)[4], new = "처방일자")
    # 
    #  if (is.null(file_data)) {
    #   file_data <- temp  # 첫 파일의 데이터 초기화
    # } else {
    file_data <- rbind(file_data, temp, use.names = TRUE, fill = TRUE)
    # }
  }  
  return(file_data)
  
}
my_read_excels <- function(path, pattern) {
  # 폴더 경로 생성
  folder_path <- file.path(project_path, path)
  
  # 패턴에 해당하는 엑셀 파일 리스트 가져오기
  excel_file_names <- list.files(folder_path, pattern = pattern, full.names = TRUE)
  
  # 엑셀 파일이 없는 경우 에러 반환
  if (length(excel_file_names) == 0) {
    stop("패턴에 해당하는 엑셀 파일이 없습니다.")
  }
  
  col_types <- rep("text", 30)
  col_types[c(6)]<-"date"
  # 모든 엑셀 파일을 읽어서 병합
  data_list <- lapply(excel_file_names, function(file) {
    as.data.table(read_xls(file), col_types = col_types)
  })
  
  # 리스트를 하나의 data.table로 병합
  merged_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)
  
  return(merged_data)
}

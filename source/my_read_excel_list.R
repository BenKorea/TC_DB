my_read_excel_list<- function(directory, pattern) {

  file_path <- list.files(directory, pattern, full.names = TRUE) #파일경로를 이름에 붙여서
  data_list <- lapply(file_path, function(file) {
    as.data.table(read_excel(file, col_types = "text"))
  })
  data<-rbindlist(data_list, use.names = TRUE, fill = TRUE) #컴럼명으로, 없다면 NA로

  return(data)
  
}
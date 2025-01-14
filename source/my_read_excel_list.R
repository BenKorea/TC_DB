my_read_excel_list<- function(directory, pattern) {

  file_names <- list.files(directory, pattern, full.names = TRUE)
  data_list <- lapply(file_names, function(file) {
    as.data.table(read_excel(file, col_types = "text"))
  })
  data<-rbindlist(data_list, use.names = TRUE, fill = TRUE)
  data <-my_PtID(data)

  return(data)
  
}
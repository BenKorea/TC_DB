my_read_excel<- function(directory, file_name, col_types) {

  file_path <- file.path(getwd(), directory, file_name)
  data <- as.data.table(read_xlsx(file_path, col_types = col_types))
  data <-my_PtID(data)

  return(data)
  
}
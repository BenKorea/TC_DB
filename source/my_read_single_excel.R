my_read_single_excel <- function(path, pattern) {
  
  folder_path <- file.path(project_path, path)
  excel_file_name <- list.files(folder_path, pattern = pattern)
  file_path <- file.path(folder_path, excel_file_name[1])
  data <- as.data.table(read_xlsx(file_path), col_types = "text")
  
  return(data)
}
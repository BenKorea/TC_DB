my_deidentify_PatientID <- function(df, col_name) {
  # 열을 정렬하고 고유한 값들을 추출합니다.
  unique_values <- unique(df[[col_name]])
  sorted_values <- sort(unique_values)
  
  # 고유한 값들에 대해 순서를 부여합니다.
  value_to_index <- match(sorted_values, unique_values)
  
  # 해당 열을 고유한 값에 대응하는 숫자로 대체합니다.
  df[[col_name]] <- as.character(value_to_index[match(df[[col_name]], unique_values)])
  
  return(df)
}

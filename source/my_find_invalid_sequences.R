my_find_invalid_sequences <- function(data) {
  invalid_data <- data[
    , {
      # 투여일 순서로 정렬
      sorted_data <- .SD[order(투여일)]
      
      # 회차의 정렬 여부 확인
      correct_order <- all(diff(sorted_data$회차) == 1)  # 회차가 1씩 증가하는지 확인
      
      # 오류가 있는 경우 전체 데이터를 반환
      if (!correct_order) .SD else NULL
    },
    by = PtID
  ]
  
  # PtID별로 회차 순서로 정렬
  if (nrow(invalid_data) > 0) {
    invalid_data <- invalid_data[order(PtID, 회차)]
  }
  
  return(invalid_data)
}
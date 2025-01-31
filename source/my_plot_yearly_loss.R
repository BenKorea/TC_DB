my_plot_yearly_loss <- function(data) {
  
  
  data[, 처방연도 := format(last_prescription, "%Y")]
  data[, 처방연도 := ifelse(is.na(처방연도), format(last_tx, "%Y"), 처방연도)]
  
  # 연도별 처방 횟수 집계
  yearly_summary <- data[, .N, by = 처방연도]
  
  # 연도 데이터를 정렬
  yearly_summary <- yearly_summary[order(처방연도)]
  
  # 그래프 생성
  plot <-ggplot(yearly_summary, aes(x = as.integer(처방연도), y = N)) +
    geom_bar(stat = "identity")
    
    return(plot)
}
  

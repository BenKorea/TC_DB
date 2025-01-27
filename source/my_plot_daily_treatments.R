my_plot_daily_treatments <- function(data) {
  
  data[, 실시연도 := format(TxDate, "%Y")]
  unique_years <- unique(data[, 실시연도])
  
  plots <- lapply(split(data, by = "실시연도"), function(year_data) {
    ggplot(year_data, aes(x = TxDate, y = n)) +
      geom_bar(stat = "identity", fill = "lightblue", color = "black") +
      scale_x_date(
        date_breaks = "1 month",  # 한 달 단위로 x축 표시
        date_labels = "%Y-%m"    # YYYY-MM 형식으로 라벨 표시
      ) +
      labs(
        title = paste("(연도:", unique(year_data$실시연도), ")"),
        x = "TxDate",
        y = "일일치료건수"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)  # x축 라벨 기울이기
      )
  })
  
  return(plots)
  
}

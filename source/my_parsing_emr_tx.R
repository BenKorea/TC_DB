################################################################################
## my_functions_for_risk
################################################################################
my_splilt_lines_for_emr_tx <- function(dt) {
  
  dt[, line_count := str_count(특기사항, "\n") + 1]
  if (input_error_checking_mode == "Y") {
    emr_tx_line_count_errors <<- dt[line_count >= 3]
  }
  dt <- dt[line_count == 2 | line_count == 1]
  dt[line_count == 2, c("Tg_line", "tx_line") := tstrsplit(특기사항, split = "\n", fixed = TRUE, fill = NA)]
  dt[line_count == 1, c("tx_line") := 특기사항]
  
  dt[, Tg_line := my_clean_string_edges(Tg_line)]
  dt[, tx_line := my_clean_string_edges(tx_line)]

  if (input_error_checking_mode == "Y") {
    emr_tx_line_error <<- dt[!grepl("^[0-9]", dt$tx_line), ]
  }
  dt <- dt[grepl("^[0-9]", dt$tx_line), ]
  
  return(dt)
}

my_parsing_emr_tx<-function(dt) {
  
  emr_tx <- dt[분류명 %in% c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")]
  setnames(emr_tx, "분류명", "Therapy")
  emr_tx[, 시행일 := as.IDate(등록일, format="%Y-%m-%d")]
  
  emr_tx <- my_splilt_lines_for_emr_tx(emr_tx)

  return(emr_tx)
  
}
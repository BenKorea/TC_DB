################################################################################
## my_functions_for_Tx_EMR
################################################################################
my_splilt_lines_for_Tx_EMR <- function(dt) {
  
  dt[, line_count := str_count(특기사항, "\n") + 1]
  if (input_error_checking_mode == "Y") {
    Tx_EMR_error_line_count <<- dt[line_count >= 3]
  }
  dt <- dt[line_count == 2 | line_count == 1]
  dt[line_count == 2, c("Tg_line", "tx_line") := tstrsplit(특기사항, split = "\n", fixed = TRUE, fill = NA)]
  dt[line_count == 1, c("tx_line") := 특기사항]
  
  dt[, Tg_line := my_clean_string_edges(Tg_line)]
  dt[, tx_line := my_clean_string_edges(tx_line)]

  if (input_error_checking_mode == "Y") {
    Tx_EMR_error_line <<- dt[!grepl("^[0-9]", dt$tx_line), ]
  }
  dt <- dt[grepl("^[0-9]", dt$tx_line), ]
  
  return(dt)
}

my_parsing_Tx_EMR_tx_line <- function(dt) {
  
  dt[, TxDate := str_extract(tx_line, "\\d{4}-\\d{2}-\\d{2}")]
  dt[, tx_line := str_remove(tx_line, "\\d{4}-\\d{2}-\\d{2}")]
  error <- dt[is.na(TxDate)]
  error[,TxDate := str_extract(tx_line, "\\d{4}-\\w{2}-dd")]
  error[, tx_line := str_remove(tx_line, "\\d{4}-\\w{2}-dd")]
  error[,TxDate := str_replace(TxDate, "mm","06")]
  error[,TxDate := str_replace(TxDate, "dd","15")]
  error[,TxDate_imputed := TRUE]
  dt <- dt[!is.na(TxDate)]
  dt[,TxDate_imputed := FALSE]
  dt<-rbind(dt,error)
  dt[, TxDate := as.IDate(TxDate)]
  Tx_EMR_error_TxDate <<- dt[is.na(TxDate)]
  dt <- dt[!is.na(TxDate)]
  
  dt[, Tx_rhTSH := str_detect(tx_line, "\\brhTSH\\b")]
  dt[, MIBG := str_detect(tx_line, "\\bMIBG\\b")]
  dt[, tx_line := str_remove(tx_line, "\\brhTSH\\b|\\bMIBG\\b")]
  
  dt[, combined_agent := str_extract(tx_line, "\\s\\+.*?(?=\\()")]
  dt[, tx_line := str_remove(tx_line, "\\s\\+.*?(?=\\s*\\()")]
  
  dt[, TxType := fifelse(
    MIBG == TRUE, 
    "I-131 MIBG",  # MIBG가 TRUE인 경우
    fifelse(
      !is.na(combined_agent), 
      str_c("I-131", combined_agent, sep = ""),  # combined_agent가 NA가 아니면 결합
      "I-131"  # 나머지 경우
    )
  )]
  dt[, c("MIBG", "combined_agent") := NULL]
  Tx_EMR_error_TxType <<- dt[is.na(TxType)]
  dt <- dt[!is.na(TxType)]
  
  dt[, TxDose := str_extract(tx_line, "\\d+\\s*mCi")]
  dt[, tx_line := str_remove(tx_line, "\\d+\\s*mCi")]
  dt[, TxDose := as.numeric(str_remove(TxDose, " mCi"))]
  Tx_EMR_error_TxDose <<- dt[is.na(TxDose)]
  # dt <- dt[!is.na(TxDose)] # 분석에 포함시키기 위해 주석 처리석
  
  dt[, tx_details := str_extract(tx_line, "\\(.*?\\)")]
  dt[, tx_line := str_remove(tx_line, "\\(.*?\\)")] 
  
  dt[, outsideH := str_extract(tx_line, "@\\s*.*$")]
  dt[, tx_line := str_remove(tx_line, "@\\s*.*$")]
  
  
  dt[, c("TxNumber", "TxWBS", "Tg_TFT", "TxTgAb") := tstrsplit(tx_details, ",", fixed = TRUE)]
  
  dt[, TxNumber := as.numeric(str_extract(TxNumber, "\\d"))]
  
  dt[is.na(Tg_TFT), TxTg_binary := "NA"]
  dt[str_detect(Tg_TFT, "Tg\\s*NA"), TxTg_binary := "NA"]
  dt[, Tg_TFT := str_remove(Tg_TFT, "Tg\\s*NA")]
  
  dt[str_detect(Tg_TFT, "Tg\\s*-"), TxTg_binary := "negative"]
  dt[, Tg_TFT := str_remove(Tg_TFT, "Tg\\s*-")]
  
  dt[str_detect(Tg_TFT, "Tg\\s*\\+"), TxTg_binary := "positive"]
  dt[, Tg_TFT := str_remove(Tg_TFT, "Tg\\s*\\+")]
  
  dt[, Tg1 := str_extract(Tg_TFT, "(?<=Tg )\\d+\\.?\\d*")]
  dt[, Tg_TFT := str_remove(Tg_TFT, "(?<=Tg )\\d+\\.?\\d*")]
  
  dt[, TFT := str_extract(Tg_TFT, "(?<=Tg/TgAb/TSH ).*?(?=\\))")]
  dt[, c("Tg2", "TgAb", "TSH") := tstrsplit(TFT, "/", fixed = TRUE)]
  
  dt[str_detect(Tg2, "ND|NA"), TxTg_binary := "NA"]
  
  dt[str_detect(Tg2, "-"), TxTg_binary := "negative"]
  dt[str_detect(Tg2, "\\+"), TxTg_binary := "positive"]
  
  dt[, TxTg := as.numeric(Tg1)]
  dt[, Tg2 := as.numeric(Tg2)]
  dt[, TxTg := ifelse(is.na(TxTg), Tg2, TxTg)]
  
  dt[!is.na(TxTg), TxTg_binary := "positive"]
  
  Tx_EMR_error_TxTg_binary <- dt[is.na(TxTg_binary)]
  
  return(dt)
}


my_parsing_Tx_EMR<-function(dt) {
  
  dt <- dt[분류명 %in% c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")]
  setnames(dt, "분류명", "Therapy")
  dt[, 시행일 := as.IDate(등록일, format="%Y-%m-%d")]
  
  dt <- my_splilt_lines_for_Tx_EMR(dt)
  dt <- my_parsing_Tx_EMR_tx_line(dt)

  return(dt)
  
}
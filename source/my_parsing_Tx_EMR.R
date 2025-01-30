################################################################################
## my_functions_for_tx_emr
################################################################################
my_splilt_lines_for_tx_emr <- function(dt) {
  
  dt[, line_count := str_count(특기사항, "\n") + 1]
  if (ERROR_CKECKING_MODE == "Y") {
    tx_EMR_error_line_count <<- dt[line_count >= 3]
  }
  dt <- dt[line_count == 2 | line_count == 1]
  dt[line_count == 2, c("tg_line", "tx_line") := tstrsplit(특기사항, split = "\n", fixed = TRUE, fill = NA)]
  dt[line_count == 1, c("tx_line") := 특기사항]
  
  dt[, tg_line := my_clean_string_edges(tg_line)]
  dt[, tx_line := my_clean_string_edges(tx_line)]

  if (ERROR_CKECKING_MODE == "Y") {
    tx_emr_error_line <<- dt[!grepl("^[0-9]", dt$tx_line), ]
  }
  dt <- dt[grepl("^[0-9]", dt$tx_line), ]
  
  return(dt)
}

my_parsing_tx_emr_tx_line <- function(dt) {
  
  dt[, tx_date := str_extract(tx_line, "\\d{4}-\\d{2}-\\d{2}")]
  dt[, tx_line := str_remove(tx_line, "\\d{4}-\\d{2}-\\d{2}")]
  error <- dt[is.na(tx_date)]
  error[,tx_date := str_extract(tx_line, "\\d{4}-\\w{2}-dd")]
  error[,tx_line := str_remove(tx_line, "\\d{4}-\\w{2}-dd")]
  error[,tx_date := str_replace(tx_date, "mm","06")]
  error[,tx_date := str_replace(tx_date, "dd","15")]
  error[,tx_date_imputed := TRUE]
  dt <- dt[!is.na(tx_date)]
  dt[,tx_date_imputed := FALSE]
  dt<-rbind(dt,error)
  dt[, tx_date := as.IDate(tx_date)]
  tx_emr_error_tx_date <<- dt[is.na(tx_date)]
  dt <- dt[!is.na(tx_date)]
  
  dt[, tx_rhtsh := str_detect(tx_line, "\\brhTSH\\b")]
  dt[, mibg := str_detect(tx_line, "\\bMIBG\\b")]
  dt[, tx_line := str_remove(tx_line, "\\brhTSH\\b|\\bMIBG\\b")]
  
  dt[, combined_agent := str_extract(tx_line, "\\s\\+.*?(?=\\()")]
  dt[, tx_line := str_remove(tx_line, "\\s\\+.*?(?=\\s*\\()")]
  
  dt[, tx_type := fifelse(
    mibg == TRUE, 
    "I-131 MIBG",  # MIBG가 TRUE인 경우
    fifelse(
      !is.na(combined_agent), 
      str_c("I-131", combined_agent, sep = ""),  # combined_agent가 NA가 아니면 결합
      "I-131"  # 나머지 경우
    )
  )]
  dt[, c("mibg", "combined_agent") := NULL]
  tx_emr_error_is_na_tx_type <<- dt[is.na(tx_type)]
  dt <- dt[!is.na(tx_type)]
  
  dt[, tx_dose := str_extract(tx_line, "\\d+\\s*mCi")]
  dt[, tx_line := str_remove(tx_line, "\\d+\\s*mCi")]
  dt[, tx_dose := as.numeric(str_remove(tx_dose, " mCi"))]
  tx_emr_error_is_na_tx_dose <<- dt[is.na(tx_dose)]
  # dt <- dt[!is.na(tx_dose)] # 분석에 포함시키기 위해 주석 처리석
  
  dt[, tx_details := str_extract(tx_line, "\\(.*?\\)")]
  dt[, tx_line := str_remove(tx_line, "\\(.*?\\)")] 
  
  dt[, outside_hosp := str_extract(tx_line, "@\\s*.*$")]
  dt[, tx_line := str_remove(tx_line, "@\\s*.*$")]
  
  
  dt[, c("tx_number", "tx_wbs", "tg_tft", "tx_tgab") := tstrsplit(tx_details, ",", fixed = TRUE)]
  
  dt[, tx_number := as.numeric(str_extract(tx_number, "\\d"))]
  
  dt[is.na(tg_tft), tx_tg_binary := "NA"]
  dt[str_detect(tg_tft, "Tg\\s*NA"), tx_tg_binary := "NA"]
  dt[, tg_tft := str_remove(tg_tft, "Tg\\s*NA")]
  
  dt[str_detect(tg_tft, "Tg\\s*-"), tx_tg_binary := "negative"]
  dt[, tg_tft := str_remove(tg_tft, "Tg\\s*-")]
  
  dt[str_detect(tg_tft, "Tg\\s*\\+"), tx_tg_binary := "positive"]
  dt[, tg_tft := str_remove(tg_tft, "Tg\\s*\\+")]
  
  dt[, tg1 := str_extract(tg_tft, "(?<=Tg )\\d+\\.?\\d*")]
  dt[, tg_tft := str_remove(tg_tft, "(?<=Tg )\\d+\\.?\\d*")]
  
  dt[, tft := str_extract(tg_tft, "(?<=Tg/TgAb/TSH ).*?(?=\\))")]
  dt[, c("tg2", "tgab", "tsh") := tstrsplit(tft, "/", fixed = TRUE)]
  
  dt[str_detect(tg2, "ND|NA"), tx_tg_binary := "NA"]
  
  dt[str_detect(tg2, "-"), tx_tg_binary := "negative"]
  dt[str_detect(tg2, "\\+"), tx_tg_binary := "positive"]
  
  dt[, tx_tg := as.numeric(tg1)]
  dt[, tg2 := as.numeric(tg2)]
  dt[, tx_tg := ifelse(is.na(tx_tg), tg2, tx_tg)]
  
  dt[!is.na(tx_tg), tx_tg_binary := "positive"]
  
  tx_emr_error_tx_tg_binary <- dt[is.na(tx_tg_binary)]
  
  return(dt)
}


my_parsing_tx_emr<-function(dt) {
  
  dt <- dt[분류명 %in% c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")]
  setnames(dt, "분류명", "Therapy")
  dt[, 시행일 := as.IDate(등록일, format="%Y-%m-%d")]
  
  dt <- my_splilt_lines_for_tx_emr(dt)
  dt <- my_parsing_tx_emr_tx_line(dt)

  return(dt)
  
}
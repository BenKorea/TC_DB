################################################################################
## my_functions_for_emr_tx
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

my_parsing_tx_line <- function(dt) {
  
  dt[, c("tx_date", "remained") := tstrsplit(tx_line, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE, keep = 2)]
  
  dt$tx_type <- tstrsplit(dt$tx_line, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[2]]
  dt$tx_dose <- tstrsplit(dt$tx_line, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[3]]
  # "PCNA (+)"가 있으면 dt$PCNA를 "Positiv"로 파생  
  dt$PCNA <- ifelse(grepl("PCNA \\(\\+\\)", dt$cN_line), "Positive", NA)
  # &가 있으면 이후를 PETCT_part로 파생
  dt$PETCT_part <- ifelse(grepl("&", dt$cN_line), sub(".*&\\s*", "", dt$cN_line), NA)
  dt$PETCT_date <- tstrsplit(dt$PETCT_part, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[1]]  
  dt$Thyroid_SUVmax <- tstrsplit(dt$PETCT_part, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[3]]
  dt$Thyroid_SUVmax <- sub("Thyroid=", "", dt$Thyroid_SUVmax)
  dt$Thyroid_SUVmax <- sub(",", "", dt$Thyroid_SUVmax)
  dt$PETCT_Metastasis <- tstrsplit(dt$PETCT_part, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[4]]
  dt$PETCT_Metastasis_SUVmax <- tstrsplit(dt$PETCT_part, "\\s+", fixed = FALSE, fill = NA, type.convert = TRUE)[[5]]
  dt$PETCT_Metastasis<-paste0(dt$PETCT_Metastasis,".",dt$PETCT_Metastasis_SUVmax)
  dt$PETCT_Metastasis_SUVmax <- ifelse(grepl("=", dt$PETCT_Metastasis), sub(".*=\\s*", "", dt$PETCT_Metastasis), NA)
  dt$PETCT_Metastasis <- ifelse(grepl("=", dt$PETCT_Metastasis), sub("\\s*=.*", "", dt$PETCT_Metastasis), NA)
  
  dt[, cN_date := as.Date(cN_date, format="%Y-%m-%d", try = TRUE)]
  if (input_error_checking_mode == "Y") {
    risk_data_cN_date_error <<- dt[is.na(cN_date) & line_count==3]
  }
  dt <- dt[!(is.na(cN_date) & line_count==3)]
  
  if (input_error_checking_mode == "Y") {
    risk_data_cN_error <<- dt[!cN %in% c("cN0", "cN1a", "cN1b", "cN1", NA,"pN0", "pN1a", "pN1b", "pN1")]
  }
  dt <- dt[cN %in% c("cN0", "cN1a", "cN1b", "cN1", NA,"pN0", "pN1a", "pN1b", "pN1")]
  
  return(dt)
}


my_parsing_Tx_EMR<-function(dt) {
  
  dt <- dt[분류명 %in% c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th")]
  setnames(dt, "분류명", "Therapy")
  dt[, 시행일 := as.IDate(등록일, format="%Y-%m-%d")]
  
  dt <- my_splilt_lines_for_Tx_EMR(dt)

  return(dt)
  
}
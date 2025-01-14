my_parsing_birthday_data<-function(dt) {
  
  birthday_data <- dt[분류명 == "Birthday_Registration"]
  birthday_data[, birthday := as.Date(특기사항, format="%Y-%m-%d")]
  if (input_error_checking_mode == "Y") {
    birthday_input_error <<- birthday_data[is.na(birthday), ]
  }
  birthday_data <- birthday_data[!is.na(birthday), ]
  downloaded_date <- my_downloaded_date("raw_data","관심환자")
  birthday_data[, 다운로드기준나이 := as.integer(difftime(downloaded_date, birthday, units = "weeks") / 52.25)]
  birthday_data[, validation := 나이-다운로드기준나이]
  if (input_error_checking_mode == "Y") {
    birthday_data_validation_error <<- birthday_data[validation > 1 | validation < -1]
  }
  birthday_data <- birthday_data[!(validation > 1 | validation < -1)]
  birthday_data <- birthday_data[, c('PtID', '성별', 'birthday')]
  return(birthday_data)
  
}
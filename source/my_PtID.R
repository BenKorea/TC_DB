my_PtID <- function(dt) {

  if ("등록번호" %in% colnames(dt)) {
    setnames(dt, old = "등록번호", new = "PtID")
  }
  dt <- dt[nchar(PtID) >= 5 & nchar(PtID) <= 8]
  dt[nchar(PtID) == 5, PtID := paste0("000", PtID)]
  dt[nchar(PtID) == 6, PtID := paste0("00", PtID)]
  dt[nchar(PtID) == 7, PtID := paste0("0", PtID)]
  
  # PtID를 첫 번째 열로 이동
  setcolorder(dt, c("PtID", setdiff(names(dt), "PtID")))
  
  return(dt)
}
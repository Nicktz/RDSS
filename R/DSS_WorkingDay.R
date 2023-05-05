#' @title DSS_WorkingDay
#' @description Checks if a day is a working day.
#' @param EvaluationDate The date for which the information is required.
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' DSS_WorkingDay(Code = c("NPN", "CPI"), DataType = c("name", "decalogaatype"), EvaluationDate = lubridate::today()-1)
#'
#' }
#' @export

DSS_WorkingDay <- function(EvaluationDate = lubridate::today()-1) {

  dssClient <- DSS_Con()

      Comdat <- new("COMDate", as.numeric(EvaluationDate - lubridate::ymd(18991230)))
      params <- list(Comdat)
      result <- dssClient$DSSGetData_2("DSSIsBusinessDay", params, "DSSIsBusinessDay")
result

}

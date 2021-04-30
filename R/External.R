
#' @title External
#' @description  returns an external series
#' @param Code Ticker provided. E.g. VIX
#' @param StartDate Start Date, e.g. lubridate::ymd(20100101)
#' @param EndDate lubridate::today()
#' @param PeriodCode Defaults to 'td'
#' @param HideMissing Defaults to 0 (ie include all dates)
#' @param HideDates Always default to 0
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS);library(lubridate)
#' DSS_Con()
#' Code = c("VIX", "USDZAR")
#' StartDate = today() %m-% months(1); EndDate = lubridate::today(); PeriodCode = "td"; HideMissing = 0; HideDates = 0
#' External(Code, StartDate, EndDate = lubridate::today(), PeriodCode = "td", HideMissing = 0, HideDates = 0)
#'
#' }
#' @export

External <- function(Code, StartDate, EndDate = lubridate::today(), PeriodCode = "td", HideMissing = 0, HideDates = 0) {

  # Convert to base 1900 to work:
  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))
  dssClient <- DSS_Con()

  df_creator <- function(dssClient, Code, ComdatStart, ComdatEnd, PeriodCode, HideMissing, HideDates){

    # EQ <- Code[[1]]
    EQ <- Code
    Tempres <-
      dssClient$DSSGetData_2("DSSExternalTimeSeries", list( EQ, ComdatStart, ComdatEnd, PeriodCode, as.integer(HideMissing), as.integer(HideDates) ), "DSS_Ext_Fetch")

    Tempres %>%
      map(set_names, c("date", "Value")) %>%
      map_df(~bind_rows(.)) %>% mutate(date = as.Date(date, origin = "1899-12-30")) %>%
      mutate(Tickers = EQ)

  }

  result <-
    Code %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., ComdatStart, ComdatEnd, PeriodCode, HideMissing, HideDates))

  if(nrow(result) == 0) result <- NULL

  result

}

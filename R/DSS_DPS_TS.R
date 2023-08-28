#' @title DSS_DPS_TS
#' @description This function returns a time series of Dividends Per Share (DPS) market data between two given dates.
#' @param Code Ticker codes
#' @param Src Source
#' @param Broker CON or SIM (Using forecast Divs), or XXX meaning no forecast divs.
#' @param StartDate Start Date
#' @param EndDate End date
#' @param IAD Information Association Date, associates the data to be brought back with a specific date field.
#' @param InfoAsAtDate The data returned is comprised of information available on or prior to specified date.
#' @param Periodicity Period code
#' @param Interpolation Leave to default
#' @param HideDates Leave to default
#' @param InclusionLevel Leave to default
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' DSS_DPS_TS(Code = c("NPN", "CPI"), Src = "PUB", Broker = "CON", StartDate = lubridate::today()-1, EndDate = lubridate::today()-1,
#' IAD = 0, InfoAsAtDate = lubridate::today()-1, Periodicity = "d", Interpolation = 1, HideDates = 0, InclusionLevel = 1)
#'
#' }
#' @export

DSS_DPS_TS <- function(Code, Src = "PUB", Broker = "CON", StartDate = lubridate::today()-1, EndDate = lubridate::today()-1,
                          IAD = 2, InfoAsAtDate = lubridate::today()-1, Periodicity = "d", Interpolation = 1, HideDates = 0, InclusionLevel = 1 ) {


  dssClient <- DSS_Con()

  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))
  InfoAsAtDate <- new("COMDate", as.numeric(InfoAsAtDate - lubridate::ymd(18991230)))


Collate_Types <- function(dssClient, Code_use, Src, Broker, ComdatStart, ComdatEnd, IAD, InfoAsAtDate, Periodicity, Interpolation, HideDates, InclusionLevel){
  # Code_use = Code[1]
  Tempres <-
    dssClient$DSSGetData_2("DSSDividendTimeSeries", list( Code_use, Src, Broker,  ComdatStart, ComdatEnd, as.integer(IAD), InfoAsAtDate,
                                                          Periodicity, as.integer(Interpolation), as.integer(HideDates), as.integer(InclusionLevel)),
                           "DSS_DivFetch") %>%
    map(set_names, c("date", "Div")) %>%
    map_df(~bind_rows(.)) %>% mutate(date = as.Date(date, origin = "1899-12-30")) %>%
    mutate(Tickers = Code_use)

}

Result <-
  Code %>% as.list() %>%
  map(~Collate_Types(dssClient, ., Src, Broker, ComdatStart, ComdatEnd, IAD, InfoAsAtDate, Periodicity, Interpolation, HideDates, InclusionLevel))

Result <- Result %>% bind_rows()

Result

}

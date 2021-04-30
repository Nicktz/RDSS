#' @title ForexTS
#' @description This function fetches the time series for a given code and input over time. Translation of DSSTimeSeries
#' @param SpotCodes Spot to fetch. E.g. SpotCodes = "$ZAR.USD"
#' @param DataType What to fetch. If multiple, it is full_joined by ticker and date. E.g. c("close", "MStotalReturn")
#' @param EvaluationDate e.g. lubridate::ymd(20200930)
#' @param Interpolation The interpolation mechanism used to get the value requested. defaults to 1.
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr

#' @examples
#' \dontrun{
#' rm(list=ls());library(RDSS);library(lubridate)
#' SpotCodes = c("$ZAR.USD", "$ZAR.EUR")
#' DataType = "spot"
#' StartDate = lubridate::today() %m-% months(6)
#' EndDate = lubridate::today()
#' HideDates = 0
#' Periodicity = 'm'
#' Orientation	= "H"
#' ForexTS(SpotCodes = c("$ZAR.USD", "$ZAR.EUR"), DataType, StartDate = lubridate::today() %m-% months(6), EndDate = lubridate::today(), Periodicity = 'm', Orientation = Orientation)
#'
#' }
#' @export

ForexTS  <- function(SpotCodes = "$ZAR.USD", DataType = "spot", StartDate = lubridate::today() %m-% months(6), EndDate = lubridate::today(), HideDates = 0, Periodicity = "m", Interpolation = 1, Orientation	= "H") {

  # Convert to base 1900 to work:
  dssClient <- RDSS::DSS_Con()
  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))


  Collation_Foo <- function(dssClient, SpotCodes, DataType, ComdatStart, ComdatEnd,
                            Periodicity, HideDates, Orientation){

    # DataType_Use <- DataType[[1]]
    DataType_Use <- DataType

    df_creator <- function(dssClient, SpotCodes, DataType_Use, ComdatStart, ComdatEnd,
                           Periodicity, HideDates, Orientation){
      # EQ <- SpotCodes[[1]]
      EQ <- SpotCodes
      Tempres <-
        dssClient$DSSGetData_2("DSSForexTimeSeries",
                               list( EQ, DataType_Use, ComdatStart, ComdatEnd,
                                     Periodicity, as.integer(HideDates), Orientation), "DSS_forex_TS_Fetch")

      Tempres <-
        Tempres %>% map(function(x) map(x, function(y) ifelse(is.null(y), NA, y))) %>% reduce(cbind) %>% data.frame() %>%
        purrr::set_names(c("Date", "value") ) %>%
        mutate(Date = as.numeric(Date)) %>%
        mutate(Date = as.Date(Date, origin = "1899-12-30")) %>% rename(date = Date) %>% mutate(Tickers = EQ)

    }

    result <-
      SpotCodes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., DataType_Use, ComdatStart, ComdatEnd,
                                                            Periodicity, HideDates, Orientation))

    result

  }

  Collated <-
    DataType %>% as.list() %>%
    map(~Collation_Foo(dssClient, SpotCodes, ., ComdatStart, ComdatEnd,
                       Periodicity, HideDates, Orientation))  %>%
    reduce(., full_join, by = c("date", "Tickers")) %>% select(date, Tickers, everything()) %>% tibble::as_tibble() %>% mutate(value = unlist(value))

  Collated <-
    Collated %>% mutate(value = ifelse(value == "", NA_real_, value))

  if(nrow(Collated) == 0) Collated <- NULL

  Collated
}

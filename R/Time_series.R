#' @title Time_series
#' @description This function fetches the time series for a given code and input over time. Translation of DSSTimeSeries
#' @param EquityCodes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for.
#' @param DataType What to fetch. If multiple, it is full_joined by ticker and date. E.g. c("close", "MStotalReturn")
#' @param StartDate e.g. lubridate::ymd(20200930)
#' @param EndDate defaults to lubridate::today()
#' @param Periodicity interval at which data should be returned, i.e. d=daily, td = trading daily, w=weekly, m=monthly, etc.
#' @param CurrencyCode Defaults to "ZAR"
#' @param Orientation Keep at "V" to make function work
#' @param HideDates Set to 0 (false) by default.
#' @param Interpolation The interpolation mechanism used to get the value requested. defaults to 1.
#' @param Extrapolation The Extrapolation mechanism used to get the value requested. defaults to 1.
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr

#' @examples
#' \dontrun{
#' library(RDSS)
#' EquityCodes = "STBI"
#' DataType = c("close", "MStotalReturn")
#' StartDate = lubridate::ymd(20200131)
#' EndDate = lubridate::today()-1
#' Periodicity = "d"
#' CurrencyCode = "ZAR"
#' Orientation = "V"
#' Interpolation = 1
#' HideDates = 0
#' Extrapolation = 0
#' Time_series (EquityCodes, DataType, StartDate, Periodicity = "m")
#'
#' }
#' @export

Time_series  <- function(EquityCodes, DataType, StartDate, EndDate = lubridate::today()-1, Periodicity = "td",
                         CurrencyCode = "ZAR", HideDates = 0, Orientation = "V", Interpolation = 1, Extrapolation = 0
                         ) {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()
  ReturnPeriodCode <- Periodicity

  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))


  Collation_Foo <- function(dssClient, EquityCodes, DataType, ComdatStart, ComdatEnd, Periodicity,
                            CurrencyCode, HideDates, Orientation, Interpolation, Extrapolation){

    # DataType_Use <- DataType[[1]]
    DataType_Use <- DataType

    df_creator <- function(dssClient, EquityCodes, DataType_Use, ComdatStart, ComdatEnd, Periodicity,
                           CurrencyCode, HideDates, Orientation, Interpolation, Extrapolation){
      # EQ <- EquityCodes[[1]]
      EQ <- EquityCodes
      Tempres <-
        dssClient$DSSGetData_2("DSSTimeSeries", list( EQ, DataType_Use, ComdatStart, ComdatEnd, Periodicity,
                                                      CurrencyCode, as.integer(HideDates), Orientation, as.integer(Interpolation), as.integer(Extrapolation )), "DSS_TS_Fetch")
      Tempres %>%
        map(set_names, c("date", DataType_Use)) %>%
        map_df(~bind_rows(.)) %>% mutate(date = as.Date(date, origin = "1899-12-30")) %>%
        mutate(Tickers = EQ)

}

  result <-
    EquityCodes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., DataType_Use, ComdatStart, ComdatEnd, Periodicity,
                                                            CurrencyCode, HideDates, Orientation, Interpolation, Extrapolation))

  result

  }

  Collated <-
  DataType %>% as.list() %>%
    map(~Collation_Foo(dssClient, EquityCodes, ., ComdatStart, ComdatEnd, Periodicity,
                       CurrencyCode, HideDates, Orientation, Interpolation, Extrapolation))  %>%
    reduce(., full_join, by = c("date", "Tickers")) %>% select(date, Tickers, everything())

  Collated

}

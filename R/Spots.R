#' @title Spots
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
#' library(RDSS)
#' SpotCodes = c("$ZAR.USD", "$ZAR.EUR")
#' DataType = c("Decalogspot")
#' EvaluationDate = lubridate::today()
#' Interpolation = 1
#' Spots(SpotCodes, DataType, EvaluationDate)
#'
#' }
#' @export

Spots  <- function(SpotCodes = "$ZAR.USD", DataType = "Decalogspot", EvaluationDate = lubridate::today(), Interpolation = 1, Value_Type = TRUE) {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()

  ComdatStart <- new("COMDate", as.numeric(EvaluationDate - lubridate::ymd(18991230)))


  Collation_Foo <- function(dssClient, SpotCodes, DataType, ComdatStart, Interpolation){

    # DataType_Use <- DataType[[1]]
    DataType_Use <- DataType

    df_creator <- function(dssClient, SpotCodes, DataType_Use, ComdatStart, Interpolation){
      # EQ <- SpotCodes[[2]]
      EQ <- SpotCodes
      Tempres <-
        dssClient$DSSGetData_2("DSSForexData", list( EQ, DataType_Use, ComdatStart, as.integer(Interpolation)), "DSS_forex_Fetch")

        tibble::as_tibble(Tempres) %>% mutate(date = as.Date(EvaluationDate), Tickers = EQ)

    }

    result <-
      SpotCodes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., DataType_Use, ComdatStart, Interpolation))

    result

  }

  Collated <-
    DataType %>% as.list() %>%
    map(~Collation_Foo(dssClient, SpotCodes, ., ComdatStart, Interpolation))  %>%
    reduce(., full_join, by = c("date", "Tickers")) %>% select(date, Tickers, everything())

    Collated <-
      Collated %>% mutate(value = ifelse(value == "", NA_real_, value))

    if(nrow(Collated) == 0) Collated <- NULL

    Collated
}

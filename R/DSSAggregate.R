#' @title DSSAggregate
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
#' Code = c("ABG")
#' DataType = "value"
#' AggregateType = "avg"
#' Enddate = lubridate::today() - 1
#' Periodicity = "3m"
#' CompletePeriod = 1
#' DSSAggregate(Codes, DataType, AggregateType, Enddate, Periodicity, CompletePeriod)
#'
#' }
#' @export

DSSAggregate  <- function(Codes = "ABG", DataType = "value", AggregateType = "avg",
                          Enddate = lubridate::today(), Periodicity = "3m", CompletePeriod = 1) {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()

  ComdatStart <- new("COMDate", as.numeric(Enddate - lubridate::ymd(18991230)))

  Collation_Foo <- function(dssClient, Codes, DataType, AggregateType, Enddate, Periodicity, CompletePeriod){

    # DataType_Use <- DataType[[1]]
    DataType_Use <- DataType

    df_creator <- function(dssClient, Codes, DataType_Use, AggregateType, Enddate, Periodicity, CompletePeriod){
      # EQ <- Codes[[1]]
      EQ <- Codes
      Tempres <-
        dssClient$DSSGetData_2("DSSAggregate", list( EQ, DataType_Use, AggregateType,
                                                     ComdatStart, Periodicity, as.integer(CompletePeriod)), "DSS_forex_Fetch")
if(is.null(Tempres)){
  res <- data.frame(value = NA_real_)%>% mutate(date = as.Date(Enddate), Tickers = EQ)
} else {
  res <- tibble::as_tibble(Tempres) %>% mutate(date = as.Date(Enddate), Tickers = EQ)
}

    }

    result <-
      Codes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., DataType_Use, AggregateType, Enddate, Periodicity, CompletePeriod))

    result

  }

  Collated <-
    DataType %>% as.list() %>%
    map(~Collation_Foo(dssClient, Codes, ., AggregateType, Enddate, Periodicity, CompletePeriod))  %>%
    reduce(., full_join, by = c("date", "Tickers")) %>% select(date, Tickers, everything())

  Collated <-
    Collated %>% mutate(value = ifelse(value == "", NA_real_, value))

  if(nrow(Collated) == 0) Collated <- NULL

  Collated
}

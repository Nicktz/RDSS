#' #' @title DSSPortfolioMValue
#' #' @description This function fetches the time series for a given code and input over time. Translation of DSSTimeSeries
#' #' @param SpotCodes Spot to fetch. E.g. SpotCodes = "$ZAR.USD"
#' #' @param DataType What to fetch. If multiple, it is full_joined by ticker and date. E.g. c("close", "MStotalReturn")
#' #' @param EvaluationDate e.g. lubridate::ymd(20200930)
#' #' @param Interpolation The interpolation mechanism used to get the value requested. defaults to 1.
#' #' @import RDCOMClient
#' #' @import lubridate
#' #' @import dplyr
#' #' @import purrr
#'
#' #' @examples
#' #' \dontrun{
#' #' library(RDSS)
#' #' PortfolioCode = c("STXBAL")
#' #' Date = lubridate::today() - 1
#' #' InstrumentType = 0
#' #' IncludeCash = 1
#' #' InternationalEquities = 1
#' #' SeeThrough = 1
#' #' DSSPortfolioMValue(Codes, DataType, AggregateType, Date, Periodicity, YesNo)
#' #'
#' #' }
#' #' @export
#'
#' DSSPortfolioMValue  <- function(PortfolioCode, Date, InstrumentType, IncludeCash, InternationalEquities, SeeThrough) {
#'
#'   # Convert to base 1900 to work:
#'   dssClient <- DSS_Con()
#'
#'   ComdatStart <- new("COMDate", as.numeric(Date - lubridate::ymd(18991230)))
#'
#'   Collation_Foo <- function(PortfolioCode, Date, InstrumentType, IncludeCash, InternationalEquities, ){
#'
#'     # DataType_Use <- InstrumentType[[1]]
#'     DataType_Use <- InstrumentType
#'
#'     df_creator <- function(dssClient, PortfolioCode, ComdatStart, InstrumentType, IncludeCash, InternationalEquities, SeeThrough){
#'       # EQ <- PortfolioCode[[1]]
#'       EQ <- PortfolioCode
#'       Tempres <-
#'         dssClient$DSSGetData_2("DSSPortfolioMValue", list( PortfolioCode, ComdatStart, as.integer(InstrumentType), as.integer(IncludeCash),
#'                                                            as.integer(InternationalEquities), as.integer(SeeThrough), "DSS_Portvalm"))
#'
#'       tibble::as_tibble(Tempres) %>% mutate(date = as.Date(Date), Tickers = EQ)
#'
#'     }
#'
#'     result <-
#'       Codes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., ComdatStart, InstrumentType, IncludeCash, InternationalEquities, SeeThrough))
#'
#'     result
#'
#'   }
#'
#'   Collated <-
#'     DataType %>% as.list() %>%
#'     map(~Collation_Foo(dssClient, Codes, ., AggregateType, Date, Periodicity, YesNo))  %>%
#'     reduce(., full_join, by = c("date", "Tickers")) %>% select(date, Tickers, everything())
#'
#'   Collated <-
#'     Collated %>% mutate(value = ifelse(value == "", NA_real_, value))
#'
#'   if(nrow(Collated) == 0) Collated <- NULL
#'
#'   Collated
#' }

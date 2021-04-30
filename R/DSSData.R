#' @title DSSData
#' @description This function fetches a single data point from DSS
#' @param Code The Securitycode for the Instrument, Index, portfolio, etc. to provide data for.
#' @param DataType The market data type to retrieve, e.g. Close, Volume, etc. Default: 'CLOSE'.
#' @param EvaluationDate The date for which the information is required.
#' @param Interpolation The interpolation mechanism used to get the value requested.
#' @param DCurrency The Currency Code in which to Display Data. Default: 'ZAR'
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' DSSData(Code = "R186", DataType = c("YTM", "CLOSE"), EvaluationDate = lubridate::today()-1)
#' DSSData(Code = c("SOL", "NPN"), DataType = c("CLOSE", "TRI"), EvaluationDate = lubridate::today()-1)
#' # Weekend date requires interpolation set to 1. I also set below to USD
#' DSSData(Code = "NPN", DataType = "OPEN", EvaluationDate = lubridate::today()-5, Interpolation = 1, DCurrency = "USD")
#'
#' }
#' @export

DSSData <- function(Code, DataType = "CLOSE", EvaluationDate = lubridate::today()-1, Interpolation = 1, DCurrency = "ZAR") {

# Convert to base 1900 to work:
  dssClient <- DSS_Con()

  Collation_codes <- function(dssClient, Code, DataType, EvaluationDate, Interpolation, DCurrency){

    # Code_use <- Code[[1]]
    Code_use <- Code

    Collate_Types <- function(dssClient, Code, DataType, EvaluationDate, Interpolation, DCurrency){

    # DataType_Use <- DataType[[1]]
    DataType_Use <- DataType

    Comdat <- new("COMDate", as.numeric(EvaluationDate - lubridate::ymd(18991230)))

    params <- list(Code_use, DataType_Use, Comdat, as.integer(Interpolation), DCurrency)
    result <- dssClient$DSSGetData_2("DSSData", params, "DSS_Dat_Fetch")
    result <- ifelse(is.null(result), NA_real_, result)

    tibble::as_tibble(result) %>% mutate(date = as.Date(EvaluationDate), Tickers = Code_use) %>%
      purrr::set_names(all_of(DataType_Use), "date", "Tickers") %>% select(date, Tickers, all_of(DataType_Use))

  }

    Result <-
    DataType %>% as.list() %>%
      map(~Collate_Types(dssClient, Code_use, ., EvaluationDate, Interpolation, DCurrency))

    Result %>% reduce(full_join, by = c("date", "Tickers"))

}

  Collation <-
    Code %>% as.list() %>%
    map_df(~Collation_codes(dssClient, ., DataType, EvaluationDate, Interpolation, DCurrency))

  if(nrow(Collation) == 0) Collation <- NULL

  Collation
}

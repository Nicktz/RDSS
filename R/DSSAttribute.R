#' @title DSSAttribute
#' @description This function fetches a simple attribute from DSS
#' @param Code The Securitycode for the Instrument, Index, portfolio, etc. to provide data for.
#' @param DataType The market data type to retrieve, e.g. Close, Volume, etc. Default: 'CLOSE'.
#' @param EvaluationDate The date for which the information is required.
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' DSSAttribute(Code = c("NPN", "CPI"), DataType = c("name", "decalogaatype"), EvaluationDate = lubridate::today()-1)
#'
#' }
#' @export

DSSAttribute <- function(Code, DataType = "decalogaatype", EvaluationDate = lubridate::today()-1) {

  dssClient <- DSS_Con()

  Collation_codes <- function(dssClient, Code, DataType, EvaluationDate){

    # Code_use <- Code[[1]]
    Code_use <- Code

    Collate_Types <- function(dssClient, Code, DataType, EvaluationDate){

      # DataType_Use <- DataType[[1]]
      DataType_Use <- DataType

      Comdat <- new("COMDate", as.numeric(EvaluationDate - lubridate::ymd(18991230)))

      params <- list(Code_use, DataType_Use, Comdat)
      result <- dssClient$DSSGetData_2("DSSAttribute", params, "DSS_Dat_Fetch")
      result <- ifelse(is.null(result), NA_real_, result)

      tibble::as_tibble(result) %>% mutate(date = as.Date(EvaluationDate), Tickers = Code_use) %>%
        purrr::set_names(all_of(DataType_Use), "date", "Tickers") %>% select(date, Tickers, all_of(DataType_Use))

    }

    Result <-
      DataType %>% as.list() %>%
      map(~Collate_Types(dssClient, Code_use, ., EvaluationDate))

    Result %>% reduce(full_join, by = c("date", "Tickers"))

  }

  Collation <-
    Code %>% as.list() %>%
    map_df(~Collation_codes(dssClient, ., DataType, EvaluationDate))

  if(nrow(Collation) == 0) Collation <- NULL

  Collation

}

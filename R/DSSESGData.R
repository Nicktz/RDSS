#' @title DSSESGData
#' @description This function fetches the ESG Information as compiled by Sustainalytics. See ?DSSESGData for examples
#' @param EquityCodes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for. Only one index input
#' @param Fields Could be multiple inputs that will be left joined. e.g. DataType <- c("weight", "close")
#' @param Date e.g. lubridate::ymd(20200130)
#' @param Provider SUST for sustainalytics
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @examples
#' \dontrun{
#' EquityCodes = c("NONSENSE", "ABG", "SOL", "NONSENSE2")
#' DataTypes = c("WRONG_SCORE", "ESG Risk Score", "ESG Risk Score-Momentum", "RUBBISH")
#' Date = lubridate::ymd(20210531)
#' DSSESGData(EquityCodes, DataTypes, Date)
#' }
#' @export

DSSESGData  <- function(EquityCodes, DataTypes, Date, Provider = "SUST"){

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()
  Comdat <- new("COMDate", as.numeric(Date - lubridate::ymd(18991230)))

  EQL <- as.list(EquityCodes)

  EQ_map <- function(EQL, DataTypes, Comdat, Provider){

  # EQChk <- EQL[[1]]
  EQChk <- EQL

  Tempres <-
    DataTypes %>% as.list() %>% map(~dssClient$DSSGetData_2("DSSESGData", list( EQChk, ., Comdat, Provider ), "DSSESGData") )

  if( class(Tempres) == "character") {
    message(print(Tempres))
    return(NULL)
  }

  result <-
    Tempres %>%
    map(function(x) map(x, function(y) ifelse(is.null(y), NA, y))) %>%
    map(function(x) map(x, function(y) ifelse(grepl("Invalid ", y), NA, y))) %>%
    map(function(x) map(x, function(y) ifelse(grepl("Object reference", y), NA, y))) %>%
    reduce(cbind) %>% data.frame() %>%
    mutate(Tickers = EQChk) %>%
    mutate(date = as.numeric(Comdat)) %>%
    mutate(date = as.Date(date, origin = "1899-12-30")) %>%
    purrr::set_names(DataTypes, "Tickers", "date") %>%
    tibble::as_tibble() %>% mutate_at( vars(-date, -Tickers), ~unlist(.))

  if(nrow(result) == 0) result <- NULL
  result
  }

  rest <-
    EQL %>% map_df(~EQ_map(., DataTypes, Comdat, Provider))

  rest
}

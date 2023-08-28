#' @title DSS_NextDivDate
#' @description This function fetches dates for Div payouts for all weekdays from start to end date given...
#' @param Code The Securitycode for the Instrument, Index, portfolio, etc. to provide data for.
#' @param DataType means LDR Date; 1 means payment Date
#' @param StartDate The date for which the information is required.
#' @param EndDate The date for which the information is required.
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' D_Check <- c("SBK", "CPI") %>% map_df(~DSS_NextDivDate(Code = ., DataType = 0, StartDate = lubridate::today()-10, EndDate = lubridate::today()-1))
#' D_Check <- DSS_NextDivDate(Code = c("CPI"), DataType = 1, StartDate = ymd(20200101), EndDate = lubridate::today()-1)
#'
#' }
#' @export

DSS_NextDivDate <- function(Code = c("NPN", "CPI"), DataType = 1, StartDate = lubridate::today()-10, EndDate = lubridate::today()-1) {

  if(length(Code) > 1) stop("Loop across stocks outside this function please...")

  dssClient <- DSS_Con()

  datloop <- function(Code, EvaluationDate, DataType){
  Comdat <- new("COMDate", as.numeric(EvaluationDate - lubridate::ymd(18991230)))
# print(EvaluationDate)
  result <- dssClient$DSSGetData_2("DSSNextDividendDate", list(Code, Comdat, as.integer(DataType)), "DSS_Dat_Fetch")

  if(result == "") return(NULL)
  data.frame(date = EvaluationDate, Next_Div = result)
  }

result <-
rmsfuns::dateconverter(StartDate, EndDate, Transform = "weekdays") %>% as.list() %>%
  map(~datloop(Code, ., DataType))

if( result %>% compact() %>% length() == 0) return(NULL)

rst <-
  result %>%
  bind_rows(.) %>% mutate(date = as.Date(date, origin = "1899-12-30"), Next_Div = as.Date(Next_Div, origin = "1899-12-30")) %>%
  mutate(Tickers = Code) %>% as_tibble()

if(DataType == 0) rst <- rst %>% mutate(Type = "LDR")
if(DataType == 1) rst <- rst %>% mutate(Type = "Payment Date")

rst

}

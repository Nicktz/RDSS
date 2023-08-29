#' @title DSSDivReport
#' @description Gives a historic view of Divs paid dates and announcements, etc.
#' @param Code Ticker code (singular)
#' @param Src Source
#' @param InclusionLevel Leave to default#' @import RDCOMClient
#' @param StartDate Start Date
#' @param EndDate End date
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' DSS_Con()
#' DSSDivReport(Code = c("NPN"), Src = "PUB", InclusionLevel = 1, StartDate = ymd(20020101), EndDate = lubridate::today()-1)
#'
#' }
#' @export

DSSDivReport <- function(Code = c("NPN"), Src = "PUB", InclusionLevel = 1, StartDate = lubridate::today()-100, EndDate = lubridate::today()-1) {

  if(length(Code) > 1) stop("Loop across stocks outside this function please...")

  dssClient <- DSS_Con()
    Comdat1 <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
    Comdat2 <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))
    # print(EvaluationDate)
    result <- dssClient$DSSGetData_2("DSSDividendsReport", list(Code, Src, Comdat1, Comdat2, as.integer(InclusionLevel)), "DSS_Dat_Fetch")

  if( result %>% compact() %>% length() == 0 | result %>% length() == 1 ) {

    rst <-
      as_tibble(c("Period",	"Type",	"Dividend", "Financial_YE",	"Date_Dec", "LDR",	"Date_Paid",	"Adjusted_DPS")) %>% mutate(V = NA) %>% spread(value, V) %>%
      mutate(across(c("Period",	"Type"), ~as.character(.))) %>%
      mutate(across(c("Dividend", "Adjusted_DPS"), ~as.numeric(.))) %>%
      mutate(across(c("Financial_YE",	"Date_Dec", "LDR",	"Date_Paid"), ~as.Date(.))) %>% mutate(Tickers = Code)

  } else {

    rst <-
      result[-1] %>%
      map(set_names, c("Period",	"Type",	"Dividend", "Financial_YE",	"Date_Dec", "LDR",	"Date_Paid",	"Adjusted_DPS")) %>%
      map_df(~bind_rows(.)) %>%
      mutate(across(c("Period",	"Type"), ~as.character(.))) %>%
      mutate(across(c("Dividend", "Adjusted_DPS"), ~as.numeric(.))) %>%
      mutate(across(c("Financial_YE",	"Date_Dec", "LDR",	"Date_Paid"), ~lubridate::dmy(.))) %>%
      mutate(Tickers = Code)


  }

    rst

}

#' @title DSSTotalReturn
#' @description This function fetches the time series for a given code over time
#' @param EquityCodes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for.
#' @param StartDate e.g. lubridate::ymd(20200930)
#' @param EndDate defaults to lubridate::today()
#' @param Interpolation The interpolation mechanism used to get the value requested. defaults to 1.
#' @param DividendSource FTSE chosen as default
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr

#' @examples
#' \dontrun{
#' library(RDSS)
#' EquityCodes <- c("NPN", "CPI", "nonsense")
#'  # Total return for NPN and CPI, with a nonsense showing its output:
#' StartDate = ymd(20220101)
#' DSSTotalReturn (EquityCodes, StartDate)
#'
#' }
#' @export

DSSTotalReturn  <- function(EquityCodes, StartDate, EndDate = lubridate::today()-1,
                            Interpolation = 1, DividendSource=c("PUB", "FTSE")[1]) {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()

  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))


  df_creator <- function(dssClient, EquityCodes, ComdatStart, ComdatEnd, Interpolation,  DividendSource){
    # EQ <- EquityCodes[[2]]
    EQ <- EquityCodes
    Tempres <-
      dssClient$DSSGetData_2("DSSTotalReturn", list( EQ, ComdatStart, ComdatEnd, as.integer(Interpolation),DividendSource ), "DSS_TR_Fetch")

    tibble(Tickers = EQ, TRI = Tempres, startDate = StartDate, endDate = EndDate)

  }


  result <-
    EquityCodes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., ComdatStart, ComdatEnd, Interpolation,  DividendSource))

  if(nrow(result) == 0) result <- NULL

  result

}

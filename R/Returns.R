#' @title Returns
#' @description This function fetches the time series for a given code over time
#' @param EquityCodes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for.
#' @param StartDate e.g. lubridate::ymd(20200930)
#' @param EndDate defaults to lubridate::today()
#' @param Periodicity interval at which data should be returned, i.e. d=daily, td = trading daily, w=weekly, m=monthly, etc.
#' @param ReturnPeriodCode Defaults to NULL - which is then the same as periodicity. This is the output periodicity
#' @param Interpolation The interpolation mechanism used to get the value requested. defaults to 1.
#' @param HideDates Set to 0 (false) by default.
#' @param Orientation Keep at "V" to make function work
#' @param Type Default = "TRI". Also available "Price"
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr

#' @examples
#' \dontrun{
#' library(RDSS)
#' EquityCodes <- c("NPN", "CPI")
#'  # Daily time series for NPN and CPI:
#'  StartDate = ymd(20200101)
#' Returns (EquityCodes, StartDate)
#' # Monthly time series for NPN and CPI:
#' Returns (EquityCodes, StartDate, Periodicity = "m")
#'
#' }
#' @export

Returns  <- function(EquityCodes, StartDate, EndDate = lubridate::today()-1, Periodicity = "td",
                                      ReturnPeriodCode = NULL,
                                      Interpolation = 1, HideDates = 0,
                                      Orientation = "V", DividendSource=c("PUB", "FTSE")[1],
                     Type = "TRI") {

  # Convert to base 1900 to work:
   dssClient <- DSS_Con()
   ReturnPeriodCode <- Periodicity

    ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
    ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))

    if(Type == "TRI"){

      df_creator <- function(dssClient, EquityCodes, ComdatStart, ComdatEnd, Periodicity, ReturnPeriodCode, Interpolation, HideDates, Orientation, DividendSource){
        # EQ <- EquityCodes[[2]]
        EQ <- EquityCodes
        Tempres <-
          dssClient$DSSGetData_2("DSSTotalReturnTimeSeries", list( EQ, ComdatStart, ComdatEnd, Periodicity,
                                                                   ReturnPeriodCode,as.integer(Interpolation),
                                                                   as.integer(HideDates), Orientation, DividendSource ), "DSS_TSTRI_Fetch")
        Tempres %>%
          map(set_names, c("date", "Returns")) %>%
          map_df(~bind_rows(.)) %>% mutate(date = as.Date(date, origin = "1899-12-30")) %>%
          mutate(Tickers = EQ)

      }

    } else{

      df_creator <- function(dssClient, EquityCodes, ComdatStart, ComdatEnd, Periodicity, ReturnPeriodCode, Interpolation, HideDates, Orientation, DividendSource){
        # EQ <- EquityCodes[[2]]
        EQ <- EquityCodes
        Tempres <-
          dssClient$DSSGetData_2("DssPriceReturnTimeSeries", list( EQ, ComdatStart, ComdatEnd, Periodicity,
                                                                   ReturnPeriodCode,as.integer(Interpolation),
                                                                   as.integer(HideDates), Orientation ), "DSS_TSPX_Fetch")
        Tempres %>%
          map(set_names, c("date", "Returns")) %>%
          map_df(~bind_rows(.)) %>% mutate(date = as.Date(date, origin = "1899-12-30")) %>%
          mutate(Tickers = EQ)

      }

    }


  result <-
    EquityCodes %>% as.list() %>% purrr::map_df(~df_creator(dssClient, ., ComdatStart, ComdatEnd, Periodicity, ReturnPeriodCode, Interpolation, HideDates, Orientation, DividendSource))

  if(nrow(result) == 0) result <- NULL

  result

}

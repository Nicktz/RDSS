#' @title Constituent_matrix_TS
#' @description This function fetches the time series for a given index's values over time.
#' @param EquityCodes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for. Only one index input
#' @param DataType Could be multiple inputs that will be left joined. e.g. DataType <- c("weight", "close")
#' @param PeriodCode periodicity of fetch (default = "m")
#' @param StartDate e.g. lubridate::ymd(20200130)
#' @param EndDate E.g. lubridate::today()
#' @param Periodicity interval at which data should be returned, i.e. d=daily, td = trading daily, w=weekly, m=monthly, etc.
#' @param SeeThrough is option indicates wether drilldown into the holdings of subportfolios need to be done.
#' The instruments of the subportfolio will appear in the list as if it is held by the parent portfolio. Defaults: 0
#' @param InstrumentType Instrument to be viewed. 0 by default (all). See here: file:///H:/_DSS/Help/NewHelp/Definitions/InstrumentType.html
#' @param AliasIndicator Set to 0 (false) by default.
#' @param IncludeCash Set to 0 (false) by default.
#' @param International 0 exclude int, 1 include, 2 only int, 3 SA, Nam, Swazi
#' @param Exclusions String for stocks to exclude
#' @param Interpolation The Interpolation mechanism used to get the value requested (default = 1)
#' @param Extrapolation The extrapolation mechanism used to get the value requested (default = 1)
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' EquityCodes = "J200"
#' DataTypes = c("Weight", "Volume", "MarketCap")
#' StartDate = ymd(20200130);EndDate = ymd(20200331);
#' PeriodCode = "m"; SeeThrough = 0; InstrumentType = 1; AliasIndicator = 0; IncludeCash = 0;
#' International = 1; Exclusions = ""; Interpolation = 1; Extrapolation = 1
#' Constituent_matrix_TS(EquityCodes, DataTypes, StartDate, EndDate)
#' }
#' @export

Constituent_matrix_TS  <- function(EquityCodes, DataTypes = "weight", StartDate = lubridate::today()-1, EndDate = lubridate::today()-1, PeriodCode = "d",
                                   SeeThrough = 0, InstrumentType = 0, AliasIndicator = 0, IncludeCash = 0,
                                   International = 1, Exclusions = "", Interpolation = 1, Extrapolation = 1){

  # Convert to base 1900 to work:
  dssClient <- RDSS::DSS_Con()
  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))

    Tempres <-
      dssClient$DSSGetData_2("DSSConstituentDataQuery", list( EquityCodes, DataTypes, ComdatStart, ComdatEnd, PeriodCode, as.integer(SeeThrough), as.integer(InstrumentType),
                                                           as.integer(AliasIndicator), as.integer(IncludeCash),
                                                           as.integer(International), Exclusions, as.integer(Interpolation), as.integer(Extrapolation) ),
                             "DSS_Const_Mat_Fetch")

    if(class(Tempres) == "character") {
      message(print(Tempres))
      return(NULL)
    }

  result <-
    Tempres %>% map(function(x) map(x, function(y) ifelse(is.null(y), NA, y))) %>% reduce(rbind) %>% data.frame() %>% slice(-1) %>%
    purrr::set_names(Tempres %>% reduce(rbind) %>% data.frame() %>% slice(1) %>% c(., recursive=TRUE) %>% unname()) %>%
    mutate(Date = as.numeric(Date)) %>%
    mutate(Date = as.Date(Date, origin = "1899-12-30")) %>% rename(date = Date) %>% # simply preferred aesthetics.
    mutate_at(vars(Basket, Instrument), ~as.character(.)) %>% tibble::as_tibble() %>%
    mutate_at(vars(-Basket, -Instrument, -date), ~unlist(.)) %>% rename(Tickers = Instrument, Index = Basket) %>% tibble::as_tibble() %>%
    select(date, Tickers, Index, everything())

  if(nrow(result) == 0) result <- NULL

  result
}

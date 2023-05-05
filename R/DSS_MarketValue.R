
#' @title DSS_MarketValue
#' @description This function fetches the Market Value of a Portfolio
#' @param Portfolios The Portfolio's code.
#' @param Evaldate e.g. lubridate::ymd(20200930)
#' @param InstrumentType 0 by default
#' The instruments of the subportfolio will appear in the list as if it is held by the parent portfolio. Defaults: 0
#' @param InstrumentType Instrument to be viewed. 0 by default (all). See here: "file:///H:/_DSS/Help/NewHelp/Definitions/InstrumentType.html"
#' @param AliasIndicator Set to 0 (false) by default.
#' @param IncludeCash Set to 0 (false) by default.
#' @param International 0 exclude int, 1 include, 2 only int, 3 SA, Nam, Swazi
#' @param SeeThrough Set to 0 by default
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' Portfolios <- c("J200", "J300", "STXMOMBENSP")
#' Evaldate = lubridate::today()
#' InstrumentType	= 0
#' IncludeCash	= 1
#' International	= 1
#' SeeThrough	= 1
#'
#' DSS_MarketValue(Portfolios = c("J200", "J300", "STXMOMBENSP"), Evaldate = lubridate::today(), InstrumentType	= 0, IncludeCash	= 1, International	= 1, SeeThrough	= 1)
#' }
#' @export

DSS_MarketValue  <- function(Portfolios, Evaldate = lubridate::today(), InstrumentType	= 0, IncludeCash	= 1, International	= 1, SeeThrough	= 0) {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()
  ComdatStart <- new("COMDate", as.numeric(Evaldate - lubridate::ymd(18991230)))

  df_creator <- function(dssClient, Portfolios, Evaldate, ComdatStart, InstrumentType, IncludeCash, International , SeeThrough){

    # EQ <- Portfolios[[2]]
    EQ <- Portfolios

      Tempres <-
        dssClient$DSSGetData_2("DSSPortfolioMValue", list( EQ, ComdatStart, as.integer(InstrumentType), as.integer(IncludeCash), as.integer(International) , as.integer(SeeThrough)),
                               "DSS_PortVal")


      if( class(Tempres) == "character") {

        df <- data.frame(date = Evaldate, Market_Value = NA_real_) %>% mutate(Tickers = EQ) %>% tibble::as_tibble()

      } else {

        df <- data.frame(date = Evaldate, Market_Value = Tempres) %>% mutate(Tickers = EQ) %>% tibble::as_tibble()
      }


    }

    result <-
      Portfolios %>% as.list() %>%
      purrr::map(~df_creator(dssClient, ., Evaldate, ComdatStart, InstrumentType, IncludeCash, International , SeeThrough))


    result %>% reduce(bind_rows)

  }


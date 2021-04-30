
#' @title Constituent_TS
#' @description This function fetches the time series for a given constituent information set over time
#' @param Codes The Securitycodes for the Instruments, Indexes, portfolios, etc. to provide data for. Only one index input
#' @param DataType Could be multiple inputs that will be left joined. e.g. DataType <- c("weight", "close")
#' @param StartDate e.g. lubridate::ymd(20200930)
#' @param EndDate E.g. lubridate::today()
#' @param Periodicity interval at which data should be returned, i.e. d=daily, td = trading daily, w=weekly, m=monthly, etc.
#' @param SeeThrough is option indicates wether drilldown into the holdings of subportfolios need to be done.
#' The instruments of the subportfolio will appear in the list as if it is held by the parent portfolio. Defaults: 0
#' @param InstrumentType Instrument to be viewed. 0 by default (all). See here: "file:///H:/_DSS/Help/NewHelp/Definitions/InstrumentType.html"
#' @param AliasIndicator Set to 0 (false) by default.
#' @param IncludeCash Set to 0 (false) by default.
#' @param International 0 exclude int, 1 include, 2 only int, 3 SA, Nam, Swazi
#' @param Orientation Keep at "H" to make function work
#' @import RDCOMClient
#' @import lubridate
#' @import dplyr
#' @import purrr
#' @examples
#' \dontrun{
#' library(RDSS)
#' EquityCodes <- "J200"
#' DataType <- c("weight", "close", "name")
#' StartDate = lubridate::ymd(20200630)
#' EndDate = lubridate::today()
#' Periodicity	= "m"
#' SeeThrough	= 0
#' InstrumentType	= 0
#' AliasIndicator	= 0
#' IncludeCash	= 0
#' International	= 1
#' Orientation	= "H"
#' Constituent_TS(EquityCodes, StartDate, EndDate, DataType, Periodicity = "tm")
#' }
#' @export

Constituent_TS  <- function(EquityCodes, StartDate, EndDate = lubridate::today(),
                         DataType = "weight", Periodicity = "td", SeeThrough = 0, InstrumentType = 0, AliasIndicator = 0 , IncludeCash = 0, International = 1, Orientation = "H") {

  # Convert to base 1900 to work:
  dssClient <- DSS_Con()
  ComdatStart <- new("COMDate", as.numeric(StartDate - lubridate::ymd(18991230)))
  ComdatEnd <- new("COMDate", as.numeric(EndDate - lubridate::ymd(18991230)))

  df_creator <- function(dssClient, EquityCodes, DataType, ComdatStart, ComdatEnd, Periodicity, SeeThrough, InstrumentType, AliasIndicator , IncludeCash, International, Orientation){

    # EQ <- EquityCodes[[2]]
    EQ <- EquityCodes

    datatype_foo <- function(dssClient, EQ, DataType, ComdatStart, ComdatEnd, Periodicity, SeeThrough, InstrumentType, AliasIndicator , IncludeCash, International, Orientation){

      # DTChoice <- DataType[[1]]
      DTChoice <- DataType

      Tempres <-
        dssClient$DSSGetData_2("DSSConstituentTimeSeries", list( EQ, DTChoice, ComdatStart, ComdatEnd, Periodicity, as.integer(SeeThrough), as.integer(InstrumentType),
                                                                 as.integer(AliasIndicator) , as.integer(IncludeCash), as.integer(International), Orientation ),
                               "DSS_TS_Const_Fetch")


      if( class(Tempres) == "character") {
        message(print(Tempres))
        return(NULL)
      }

      df <-
        Tempres %>% map(function(x) map(x, function(y) ifelse(is.null(y), NA, y))) %>% reduce(rbind) %>% data.frame() %>% slice(-1) %>%
        purrr::set_names(c("date", Tempres %>% reduce(rbind) %>% data.frame() %>% slice(1) %>% c(., recursive=TRUE) %>% unname())) %>%
        mutate_at(vars(date), as.numeric) %>%
        mutate(date = as.Date(date, origin = "1899-12-30")) %>% tibble::as_tibble()

      if( length(colnames(df)) == 1 ) return(df %>% mutate(Tickers = DTChoice, Col = NA) %>% rename(!!all_of(DTChoice) := Col))
      df <-
        df %>%
        gather(Tickers, Col, -date) %>% rename(!!all_of(DTChoice) := Col) %>%
        unnest(all_of(DTChoice))

    }

    result <-
      DataType %>% as.list() %>%
      purrr::map(~datatype_foo(dssClient, EQ, ., ComdatStart, ComdatEnd, Periodicity, SeeThrough, InstrumentType, AliasIndicator , IncludeCash, International, Orientation)) %>%
      reduce(left_join, c("date", "Tickers")) %>%
      mutate(Index = EQ)

  }

  Collated <-
    EquityCodes %>% as.list() %>%
    purrr::map(~df_creator(dssClient, ., DataType, ComdatStart, ComdatEnd, Periodicity, SeeThrough, InstrumentType, AliasIndicator , IncludeCash, International, Orientation))


  if(nrow(Collated) == 0) Collated <- NULL

  Collated

}

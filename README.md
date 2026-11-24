## Purpose

Connect to DSS databases for Sanlam employees - you will need to be
connected to SI network for RDSS to work.

> This package allows connnections to DSS through R.

> This will remain a work in progress as DSS itself develops - for
> assistance, contact me directly at <nicok@satrix.co.za>

## Steps:

> You need to have the following packages installed first, then
> RDCOMClient must be installed”

``` r
if(!require("pacman")) install.packages("pacman")
if(!require("devtools")) install.packages("devtools")
if(!require("remotes")) install.packages("remotes")
```

Next install RDCOmclient from my forked github version (ask me if this
does not work please) - which corrects some offending elements of the
updated package:

``` r
remotes::install_github("jkylearmstrong-temple/RDCOMClient")
```

Now install RDSS from source here:

``` r
remotes::install_github("Nicktz/RDSS")
```

## Functions

### Grab a single point: **BDP**

#### DSS Call: DSSData

``` r
pacman::p_load(tidyverse, lubridate, RDSS)
DSSData(Code = "R186", DataType = "YTM", EvaluationDate = lubridate::today()-1)
DSSData(Code = "SOL", DataType = "CLOSE", EvaluationDate = lubridate::today()-1)
```

### TRI timeseries returns: **Returns**

#### DSS Call: DssPriceReturnTimeSeries / DSSTotalReturnTimeSeries

``` r
pacman::p_load(tidyverse, lubridate, RDSS)

# Get Timeseries for stocks, funds, etc.
Tickers <- c("DOESNTEXIST", "SOL", "CPI", "STRUST", "SP500")
# Daily:
df <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(3)) %>% arrange(date)
#Monthly - last 6 months

  alibtr <- Returns(EquityCodes = "ALBITR", StartDate = today() %m-% months(3), Periodicity = "tm") %>% arrange(date)
  
  # Or alternatively:
  
  albi <- RDSS::Time_series(c("ALBITR"), "close", StartDate = ymd(20260101), EndDate = today(), Periodicity = "tm", CurrencyCode = "ZAR") %>% arrange(date) %>% group_by(Tickers) %>% mutate(Returns = close / lag(close)-1) %>% ungroup() %>% filter(!is.na(Returns)) %>% select(-close)
  

library(lubridate)
dfweekly <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(1), Periodicity = "w", Type = "TRI") %>% arrange(date)

# Monthly: last, 6 months: Trading days
dfweekly_TRI <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(24), Periodicity = "tw", Type = "TRI") %>% arrange(date)

dfweekly_PX <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(24), Periodicity = "tw", Type = "PX") %>% arrange(date)
```

### Constituent time series: **Constituent_TS**

#### DSS Call: DSSConstituentTimeSeries

``` r
pacman::p_load(tidyverse, lubridate, RDSS)
EquityCodes <- "J300"
DataType <- c("weight", "close", "totalreturn", "PE") # PE not valid call here - show that it won't break function
StartDate = lubridate::today() %m-% months(6)
EndDate = lubridate::today()
Periodicity = "m"
Ex_df <- Constituent_TS(EquityCodes, StartDate, EndDate, DataType, Periodicity = "tm")
```

### Constituent Matrix timeseries

#### DSS Call: DSSConstituentDataQuery

``` r
rm(list=ls())
pacman::p_load(tidyverse, lubridate, RDSS)
# Index fetch:
EquityCodes = "J200"
DataTypes = c("Weight", "Volume", "MarketCap", "EPS", "DY")
StartDate = lubridate::today() %m-% months(6);EndDate = lubridate::today()
# SeeThrough = 0; InstrumentType = 1; AliasIndicator = 0; IncludeCash = 0;
# International = 1; Exclusions = ""; Interpolation = 1; Extrapolation = 1
df_TS <- Constituent_matrix_TS(EquityCodes, DataTypes, StartDate, EndDate, PeriodCode = "m")

# Funds fetch
EquityCodes = c("STRUST", "SVOORS")
DataTypes <- c("holdings",  "close","marketvalue", "weight",    "name", "decalogtype", "hiportsector1", "hiportsector3", "hiportsector5")
# StartDate = lubridate::today() %m-% months(6);EndDate = lubridate::today()

df_TS <- 
  EquityCodes %>% as.list() %>% 
  map_df(~Constituent_matrix_TS(., DataTypes, StartDate, EndDate, IncludeCash = 1, PeriodCode = "m")) %>% 
  rename(Locality = hiportsector5)
df_TS %>% arrange(date)


# MSCI constituent data: Use the DSSConstituentData function as opposed to the time-series function (doesn't square e.g. with DSSMSCIEMERG when using TS.):
EquityCodes = c("DSSMSCIEMERG", "DSSMSCIWRLD")
DataTypes <- c("MSCIInstrumentName","MSCIBBTicker","MSCIRICTicker","weight", "close","MarketCap","mscisectorname","mscicountryname")
# StartDate = ymd(20200831);EndDate = ymd(20200930) 

df_TS <- 
  EquityCodes %>% as.list() %>% 
  map_df(~Constituent_matrix_TS(., DataTypes, StartDate, EndDate, IncludeCash = 1, PeriodCode = "tm")) 

df_TS %>% arrange(date)


# Japan funds:
EquityCodes = c("DSSMSCIJPA", "DSSMSCIEDMJ")
df_TS <- 
  EquityCodes %>% as.list() %>% 
  map_df(~Constituent_matrix_TS(., DataTypes, StartDate, EndDate, IncludeCash = 1, PeriodCode = "tm")) 
```

Time series

``` r
# E.g. time series of PE of J200
library(tidyverse)
 library(RDSS)
 EquityCodes = c("J400", "J203")
 DataType = c("close", "MStotalReturn", "ey")
 StartDate = lubridate::today() %m-% months(6)
 EndDate = lubridate::today()-1
 Periodicity = "tm"
 dff <- Time_series (EquityCodes, DataType, StartDate, Periodicity = Periodicity)
 dff %>% group_by(Tickers) %>% mutate(PE = 1/ey)
```

### External Data Fetch

#### DSS Call: DSSExternalTimeSeries

``` r
pacman::p_load(tidyverse, lubridate, RDSS)

Code = c("VIX", "USDZAR")
StartDate = today() %m-% months(1)
EndDate = lubridate::today()
PeriodCode = "td"

External(Code, StartDate, EndDate, PeriodCode) %>% arrange(date) %>% 
  ggplot() + geom_line(aes(date, Value, color = Tickers)) + facet_wrap(~Tickers) + theme_bw()


# Fetching the ZAR:
zar <- RDSS::ForexTS(SpotCodes = "$ZAR.USD", "decalogspot", StartDate = today()%m-%months(3), Periodicity = "td")

### Spot at specific point in time:
EquityCodes <- "NDDUWI"
dff <- Time_series(EquityCodes, DataType = "close", StartDate, Periodicity = "td")
ZAR <- unique(dff$date) %>% as.list() %>% map_df(~RDSS::Spots(SpotCodes = "$ZAR.USD", DataType = "DecalogSpot", EvaluationDate = .))
```

Example of various global indices fetched:

``` r
  Aliases <-
    c("NDDUWI"="MSCI World",
      "SXXGV"="MSCI Europe 600",
      "NDUEEGF"="MSCI Emerging Markets",
      "NDUEACWF"="MSCI ACWI",
      "DSSMSCIJAPAN"="MSCI Japan",
      "LEGATRUU"="GlobalAgg Unhedged USD",
      "FGCIITU" = "FTSE Global Core Infrastructure Net Return",
      # "US0003M" = "US 3 Month Libor Rate",
      # "TENGDNU" = "FTSE EPRA/NAREIT Developed Dividend+ Index",
      "TRNGLU" = "FTSE EPRA/NAREIT Developed Index",
      "GOLDS" = "Gold Spot $/Oz",
      # "MXEF" = "MSCI EM",
      "BCIT1T" = "US Inflation Linkers",
      "NNDX" = "NASDAQ 100",
      "M1WO000G" = "MSCI Growth",
      "M1WO000V" = "MSCI Value",
      "M1WOQU" = "MSCI Quality",
      "M1WOMOM" = "MSCI Momentum",
      "VIX" = "VIX",
      "SPXT" = "SP 500",
      "UKXDUK" = "FTSE 100",
      "NKYTR" = "Nikkei",
      "DSSMSCIAFXSA" = "AfricaXSA",
      "SOFRRATE" = "US_CASH",
      "JALSHTR" = "JALSHTR",
      "BRENT" = "BRENT",
      "India" = "India",
      "China" = "China",
      "IBXXSIG1" = "Iboxx Short Duration Credit",
      "JPEICORE" = "EM_Debt",
      "LGCPTRUU" = "Gl_Credit"
    )

  
  tsdf <- RDSS::Time_series(c(  "NDDUWI","NDUEEGF","NDUEACWF","LEGATRUU","SXXGV", "DSSMSCIJAPAN","FGCIITU","TRNGLU","GOLDS","BCIT1T","NNDX","M1WO000G","M1WO000V",
                                "M1WOQU","M1WOMOM","VIX","SPXT","UKXDUK","NKYTR","SPXT","BRENT",
                                "DSSMSCIAFXSA","JALSHTR","SOFRRATE","MXEF", "IBXXSIG1", "LGCPTRUU", "JPEICORE"), "close",
                            StartDate = today() %m-%months(12), EndDate = today(), Periodicity = "m", CurrencyCode = "USD")
  
  tsdf <-
    tsdf %>%
    mutate(Name = Tickers) %>% RA::Rename_Column(., "Name", Aliases)
  
  india <- RDSS::Time_series("DSSMSCIINDIA", "MSCIUSDTR", StartDate = today() %m-%months(12), EndDate = today(), Periodicity = "tm", CurrencyCode = "USD") %>% purrr::set_names(c("date", "Tickers", "close"))
  china <- RDSS::Time_series("DSSMSCICHINA", "MSCIUSDTR", StartDate = today() %m-%months(12), EndDate = today(), Periodicity = "tm", CurrencyCode = "USD") %>% purrr::set_names(c("date", "Tickers", "close"))


tsdf <- 
  bind_rows(
    tsdf,
    india %>% mutate(Name = "MSCI India"),
    china %>% mutate(Name = "MSCI China")
  )
```

Example of fetching back index holdings:

``` r
dates <- rmsfuns::dateconverter(today() %m-% months(6), EndDate = floor_date(today(), unit = "months")-1, Transform = "calendarEOM")

DataTypes <- c("holdings",  "close","marketvalue", "weight",    "name", "decalogtype", "hiportsector1", "hiportsector3", "hiportsector5")

df_H <- 
dates %>% as.list() %>% 
map_df(~Constituent_matrix_TS(EquityCodes = "J200", DataTypes, ., ., IncludeCash = 1, PeriodCode = "m")) %>% 
    rename(Locality = hiportsector5)

# Let's say you now want to append ISINs:
isins <- unique(df_H$Tickers) %>% as.list() %>% map_df(~RDSS::DSSAttribute(., EvaluationDate = today(), DataType = "ISIN")) %>% select(-date)

df_H <- 
  left_join(df_H, 
          isins, by = "Tickers"
)
```

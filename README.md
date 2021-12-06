## Purpose

Connect to DSS databases for Sanlam employees.

> Big thanks to the mvp Francois Botha who assisted with API connections
> and understanding the dark art of working with the DSS api…

Still a work in Progress.

## Steps:

First copy the files in the server\_files folder in the RDSS folder, to
the Rstudio directory as follows:

  - In R, run:

> system.file(‘server\_files’, package = ‘RDSS’)

  - Copy these files to wherever your Rstudio is installed, e.g.:

> “C:/Program Files/RStudio/bin”

Next, run the following from cmd (admin):

``` 
    cd "c:\Program Files\SIM\DSSServerAPI\"
    regasm64\RegAsm.exe SIM.ServerInterface.dll /codebase /tlb
```

Documentation for functions reside here:

> system.file(‘bin’, package = ‘RDSS’)

Now also install **pacman**:

``` r
if(!require("pacman")) install.packages("pacman")
```

You might also need to install RDCOmclient from the source to make it
work (as I discussed
[here](https://stackoverflow.com/a/51132513/4198868)):

``` r
install.packages("RDCOMClient", repos="http://www.omegahat.net/R").
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
library(lubridate)
dfweekly <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(1), Periodicity = "w", Type = "TRI") %>% arrange(date)
# Monthly: last, 6 months: Trading days
dfweekly_TRI <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(24), Periodicity = "tw", Type = "TRI") %>% arrange(date)
dfweekly_PX <- Returns(EquityCodes = Tickers, StartDate = today() %m-% months(24), Periodicity = "tw", Type = "PX") %>% arrange(date)
```

### Constituent time series: **Constituent\_TS**

#### DSS Call: DSSConstituentTimeSeries

``` r
pacman::p_load(tidyverse, lubridate, RDSS)
EquityCodes <- "J300"
DataType <- c("weight", "close", "totalreturn", "PE") # PE not valid call here - show that it won't break function
StartDate = lubridate::ymd(20200101)
EndDate = lubridate::today()
Periodicity = "m"
Ex_df <- Constituent_TS(EquityCodes, StartDate, EndDate, DataType, Periodicity = "tm")
```

### Constituent Matrix timeseries

#### DSS Call: DSSConstituentDataQuery

``` r
pacman::p_load(tidyverse, lubridate, RDSS)
rm(list=ls())
# Index fetch:
EquityCodes = "J200"
DataTypes = c("Weight", "Volume", "MarketCap", "EPS", "DY")
StartDate = ymd(20200130);EndDate = ymd(20200930) 
# SeeThrough = 0; InstrumentType = 1; AliasIndicator = 0; IncludeCash = 0;
# International = 1; Exclusions = ""; Interpolation = 1; Extrapolation = 1
df_TS <- Constituent_matrix_TS(EquityCodes, DataTypes, StartDate, EndDate)

# Funds fetch
EquityCodes = c("STRUST", "SVOORS")
DataTypes <- c("holdings",  "close","marketvalue", "weight",    "name", "decalogtype", "hiportsector1", "hiportsector3", "hiportsector5")
StartDate = ymd(20200130);EndDate = ymd(20200930) 

df_TS <- 
  EquityCodes %>% as.list() %>% 
  map_df(~Constituent_matrix_TS(., DataTypes, StartDate, EndDate, IncludeCash = 1)) %>% 
  rename(Locality = hiportsector5)
df_TS %>% arrange(date)


# MSCI constituent data: Use the DSSConstituentData function as opposed to the time-series function (doesn't square e.g. with DSSMSCIEMERG when using TS.):
EquityCodes = c("DSSMSCIEMERG", "DSSMSCIWRLD")
DataTypes <- c("MSCIInstrumentName","MSCIBBTicker","MSCIRICTicker","weight", "close","MarketCap","mscisectorname","mscicountryname")
StartDate = ymd(20200831);EndDate = ymd(20200930) 

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
 StartDate = lubridate::ymd(20200131)
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

### Spot at specific point in time:

MSCI_W <- "NDDUWI"
dff <- Time_series(EquityCodes, DataType = "close", StartDate, Periodicity = Periodicity)
ZAR <- unique(dff$date) %>% as.list() %>% map_df(~RDSS::Spots(SpotCodes = "$ZAR.USD", DataType = "DecalogSpot", EvaluationDate = .))
```

#Data Download file for Precious Metals Prediction - ST 558 Final Project
#Author: Carlos Petzold
#Date: 11/21/2026

#We'll need 3 packages to download data from Yahoo Finance and FRED (Federal Reserve Economic Data)

library(tidyverse)
library(tidyquant) #brings quantitative financial analysis into the tidyverse
library(lubridate) #makes working with dates and times easier


#Let's use data from the last 20 years (Jan 1 2006) and end at the latest completed month (Oct 2026)

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2026-11-01")

################################################################################

#Now let's download S&P 500, Gold, and Silver ETF data across those 20 years:
etf_symbols <- c("SPY", "GLD", "SLV")
etf_data <- tq_get( #get ETF symbol data in tibble format
  etf_symbols, get = "stock.prices", from = start_date, to = end_date
  ) |>
  filter(date < end_date)

#Quick check to make sure that the desired data was downloaded:
if (!setequal(unique(etf_data$symbol), etf_symbols)) {
  stop("One or more ETF series were not downloaded")
}

glimpse(etf_data)

#save data to files
write_csv(etf_data, "data/raw/etf_daily.csv")

################################################################################

#Now let's download economic data from FRED:

fred_symbols <- c(
  "DFF",    #Effective Federal Funds Rate
  "DGS2",   #2-year Treasury Yield
  "DGS10",  #10-year Treasury Yield
  "DFII10", #10-year Real Treasury Yield
  "VIXCLS"  #Market Volatility Index
  )

fred_data <- tq_get(
  fred_symbols, get = "economic.data", from = start_date, to = end_date
  ) |>
  filter(date < end_date)

#Quick check to make sure that the desired data was downloaded:
if (!setequal(unique(fred_data$symbol), fred_symbols)) {
  stop("One or more FRED series were not downloaded.")
}

glimpse(fred_data)

# Save data to files
write_csv(fred_data, "data/raw/fred_daily.csv")

################################################################################

#Now lets do a quick check of our downloaded data:

# Check ETF date coverage
etf_data |>
  group_by(symbol) |>
  summarise(
    start = min(date),
    end = max(date), 
    observations = n(),
    missing_values = sum(is.na(adjusted)),
    .groups = "drop"
  )

# Check FRED date coverage
fred_data |>
  group_by(symbol) |>
  summarise(
    start = min(date),
    end = max(date),
    observations = n(),
    missing_values = sum(is.na(price)),
    .groups = "drop"
  )

head(etf_data)
head(fred_data)


etf_data |> count(symbol)
fred_data |> count(symbol)

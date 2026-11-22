#Data Download file for Precious Metals Prediction - ST 558 Final Project
#Author: Carlos Petzold
#Date: 11/21/2026

#We'll need 3 packages to download data from Yahoo Finance and FRED (Federal Reserve Economic Data)

library(tidyverse)
library(tidyquant) #brings quantitative financial analysis into the tidyverse
library(lubridate) #makes working with dates and times intuitive


#Let's use data from the last 20 years (Jan 1 2006) and end at the latest completed month (Oct 2026)

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2026-11-01")


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
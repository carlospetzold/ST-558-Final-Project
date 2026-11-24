#Data Preparation file for Precious Metals Prediction - ST 558 Final Project
#Author: Carlos Petzold
#Date: 11/23/2026


library(tidyverse)
library(lubridate)

#Read in our raw data
eft_data <- read_csv("data/raw/etf_daily.csv", show_col_types = FALSE)
fred_data <- read_csv("data/raw/fred_daily.csv", show_col_types = FALSE)

#We need to create a variable to calculate our monthly ETF returns. We'll do this by obtaining the last adjusted closing price at each month
etf_monthly <- etf_data |>
  filter(!is.na(adjusted)) |>
  mutate(month = floor_date(date, "month")) |>
  group_by(symbol, month) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(month, symbol, adjusted) |>
  pivot_wider(
    names_from = symbol,
    values_from = adjusted
  )

#create a monthly calendar
months <- tibble(
  month = seq(
    as.Date("2006-01-01"),
    as.Date("2026-10-01"),
    by = "month"
  )
)

# Calculate percentage returns as decimals
etf_monthly <- months |>
  left_join(etf_monthly, by = "month") |>
  arrange(month) |>
  mutate(
    ret_SPY = SPY / lag(SPY) - 1,
    ret_GLD = GLD / lag(GLD) - 1,
    ret_SLV = SLV / lag(SLV) - 1
  )
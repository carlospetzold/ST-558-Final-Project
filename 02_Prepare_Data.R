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

# Calculate percentage returns as decimals. The return is equal to the ratio of one months value to the month prior, subtracted by 1 to standardize
etf_monthly <- months |>
  left_join(etf_monthly, by = "month") |>
  arrange(month) |>
  mutate(
    ret_SPY = SPY / lag(SPY) - 1,
    ret_GLD = GLD / lag(GLD) - 1,
    ret_SLV = SLV / lag(SLV) - 1
  )


#Now we'll similarly set up our FRED variables
fred_monthly <- fred_data |>
  filter(!is.na(price)) |>
  mutate(month = floor_date(date, "month")) |>
  group_by(symbol, month) |>
  slice_max(date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(month, symbol, price) |>
  pivot_wider(
    names_from = symbol,
    values_from = price
  ) |>
  mutate(
    yield_spread = DGS10 - DGS2
  )


#Now combine the datasets
market_data <- etf_monthly |>
  left_join(fred_monthly, by = "month") |>
  arrange(month) |>
  mutate(
    next_spy_return = lead(ret_SPY),
    next_ret_GLD = lead(ret_GLD),
    next_ret_SLV = lead(ret_SLV)
  )

#Now we clean up our data to prepare it for EDA and model building
model_data <- market_data |>
  pivot_longer(
    cols = c(
      ret_GLD, ret_SLV,
      next_ret_GLD, next_ret_SLV
    ),
    names_to = c(".value", "metal"),
    names_pattern = "(ret|next_ret)_(GLD|SLV)"
  ) |>
  mutate(
    metal = recode(
      metal,
      GLD = "Gold",
      SLV = "Silver"
    ),
    outperform_next_month = case_when(
      is.na(next_ret) |
        is.na(next_spy_return) ~ NA_character_,
      next_ret > next_spy_return ~ "Yes",
      TRUE ~ "No"
    )
  ) |>
  transmute(
    month,
    metal = factor(metal),
    fed_funds_rate = DFF,
    treasury_2y = DGS2,
    treasury_10y = DGS10,
    real_yield_10y = DFII10,
    yield_spread,
    vix = VIXCLS,
    spy_return = ret_SPY,
    metal_return = ret,
    outperform_next_month = factor(
      outperform_next_month,
      levels = c("Yes", "No")
    )
  ) |>
  filter(
    month >= as.Date("2007-01-01"),
    month <= as.Date("2026-09-01")
  )


# Number of observations for each metal
model_data |>
  count(metal)

# Distribution of our response
model_data |>
  count(metal, outperform_next_month)

# Summary statistics for predictors
model_data |>
  select(where(is.numeric)) |>
  summary()

# Check and handle missing observations
model_data |>
  summarise(
    across(everything(), ~ sum(is.na(.x)))
  )

# Retain complete observations
model_data <- model_data |>
  drop_na() |>
  group_by(month) |>
  filter(n() == 2) |>
  ungroup()


# Save the processed data
dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  model_data,
  "data/processed/precious_metals.csv"
)

# Examine the completed dataset
glimpse(model_data)

model_data |>
  count(metal, outperform_next_month)

model_data |>
  summarise(
    first_month = min(month),
    last_month = max(month),
    observations = n(),
    number_of_months = n_distinct(month)
  )
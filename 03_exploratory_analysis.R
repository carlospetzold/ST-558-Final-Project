#EDA file for Precious Metals Prediction - ST 558 Final Project
#Author: Carlos Petzold
#Date: 11/23/2026

library(tidyverse)
library(lubridate)
library(scales)


# Read in and inspect the data:

model_data <- read_csv(
  "data/processed/precious_metals.csv",
  show_col_types = FALSE
) |>
  mutate(
    month = as.Date(month),
    metal = factor(metal, levels = c("Gold", "Silver")),
    outperform_next_month = factor(
      outperform_next_month,
      levels = c("Yes", "No")
    )
  ) |>
  arrange(month, metal)

# Create directory for figures
dir.create("figures", showWarnings = FALSE)

# Split data into training and testing:

# Split by unique months rather than individual rows
all_months <- sort(unique(model_data$month))

n_train_months <- floor(length(all_months) * 0.70)

training_months <- all_months[
  seq_len(n_train_months)
]

train_data <- model_data |>
  filter(month %in% training_months)

test_data <- model_data |>
  filter(!month %in% training_months)

# Verify that months do not overlap
stopifnot(
  !any(train_data$month %in% test_data$month)
)

# Inspect split
tibble(
  dataset = c("Training", "Testing"),
  observations = c(nrow(train_data), nrow(test_data)),
  months = c(
    n_distinct(train_data$month),
    n_distinct(test_data$month)
  ),
  start = c(
    min(train_data$month),
    min(test_data$month)
  ),
  end = c(
    max(train_data$month),
    max(test_data$month)
  )
)

#Distribution of Outcome by Metal:
outcome_summary <- train_data |>
  group_by(metal, outperform_next_month) |>
  summarise(
    n = n(),
    .groups = "drop"
  ) |>
  group_by(metal) |>
  mutate(
    proportion = n / sum(n)
  ) |>
  ungroup()

print(outcome_summary)

# Visualize
p1 <- ggplot(
  outcome_summary,
  aes(
    x = metal,
    y = proportion,
    fill = outperform_next_month
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = percent(proportion, accuracy = 0.1)),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = label_percent(),
    limits = c(0, 0.75)
  ) +
  labs(
    title = "Precious Metal Outperformance",
    subtitle = "Proportion of training months by metal",
    x = NULL,
    y = "Proportion of months",
    fill = "Outperformed SPY\nnext month"
  ) +
  theme_minimal(base_size = 12)

print(p1)

ggsave(
  "figures/01_outperformance_distribution.png",
  plot = p1,
  width = 8,
  height = 5,
  dpi = 300
)


# Calculate cumulative returns

cumulative_returns <- train_data |>
  select(
    month,
    metal,
    metal_return,
    spy_return
  ) |>
  pivot_wider(
    names_from = metal,
    values_from = metal_return
  ) |>
  rename(
    SPY = spy_return
  ) |>
  pivot_longer(
    cols = c(SPY, Gold, Silver),
    names_to = "asset",
    values_to = "monthly_return"
  ) |>
  arrange(asset, month) |>
  group_by(asset) |>
  mutate(
    growth_of_dollar = cumprod(1 + monthly_return)
  ) |>
  ungroup()

# Visualize cumulative investment growth
p2 <- ggplot(
  cumulative_returns,
  aes(
    x = month,
    y = growth_of_dollar,
    color = asset
  )
) +
  geom_line(linewidth = 1) +
  scale_y_continuous(
    labels = label_dollar()
  ) +
  labs(
    title = "Historical Performance of Gold, Silver, and SPY",
    subtitle = "Growth of $1 invested at the beginning of the training period",
    x = "Year",
    y = "Investment value",
    color = "Asset"
  ) +
  theme_minimal(base_size = 12)

print(p2)

ggsave(
  "figures/02_cumulative_returns.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)


# Look at interest rates and trasury yields

interest_rate_data <- train_data |>
  distinct(
    month,
    fed_funds_rate,
    treasury_10y,
    real_yield_10y
  ) |>
  pivot_longer(
    cols = c(
      fed_funds_rate,
      treasury_10y,
      real_yield_10y
    ),
    names_to = "interest_rate_type",
    values_to = "rate"
  ) |>
  mutate(
    interest_rate_type = recode(
      interest_rate_type,
      fed_funds_rate = "Federal Funds Rate",
      treasury_10y = "10-Year Treasury Yield",
      real_yield_10y = "10-Year Real Treasury Yield"
    )
  )

p3 <- ggplot(
  interest_rate_data,
  aes(
    x = month,
    y = rate,
    color = interest_rate_type
  )
) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Interest Rates and Treasury Yields",
    subtitle = "Monthly observations during training period",
    x = "Year",
    y = "Annualized rate (%)",
    color = "Interest Rate"
  ) +
  theme_minimal(base_size = 12)

print(p3)

ggsave(
  "figures/03_interest_rates.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

# Yield Curve (10 year minus 2 year)

yield_curve_summary <- train_data |>
  mutate(
    yield_curve = if_else(
      yield_spread < 0,
      "Inverted",
      "Non-Inverted"
    )
  ) |>
  group_by(metal, yield_curve) |>
  summarise(
    n_months = n(),
    n_outperform = sum(
      outperform_next_month == "Yes"
    ),
    outperform_rate = mean(
      outperform_next_month == "Yes"
    ),
    .groups = "drop"
  )

print(yield_curve_summary)

p4 <- ggplot(
  yield_curve_summary,
  aes(
    x = yield_curve,
    y = outperform_rate,
    fill = metal
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    labels = label_percent(),
    limits = c(0, 1)
  ) +
  labs(
    title = "Yield Curve Inversion and Precious Metal Performance",
    subtitle = "Training-period next-month outperformance rates",
    x = "Yield Curve Condition",
    y = "Probability of Outperformance (Observed)",
    fill = "Metal"
  ) +
  theme_minimal(base_size = 12)

print(p4)

ggsave(
  "figures/04_yield_curve_outperformance.png",
  plot = p4,
  width = 8,
  height = 5,
  dpi = 300
)
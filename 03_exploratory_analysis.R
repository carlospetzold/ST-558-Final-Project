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
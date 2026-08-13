#=========================================================
# 07_exploratory_analysis.R
#=========================================================

library(tidyverse)
library(janitor)
library(lubridate)

#=========================================================
# Load analytic dataset
#=========================================================

analytic_data <- readRDS(
  "analytic_dataset.rds"
)

analytic_data <- analytic_data %>%
  
  select(
    -doses.x,
    -age_group.x,
    -age_target.x
  ) %>%
  
  rename(
    doses = doses.y,
    age_group = age_group.y,
    age_target = age_target.y
  )
#=========================================================
# Basic structure
#=========================================================

cat("\nRows:\n")
print(nrow(analytic_data))

cat("\nColumns:\n")
print(ncol(analytic_data))

glimpse(analytic_data)

#=========================================================
# Missingness assessment
#=========================================================

missingness <- analytic_data %>%
  
  summarise(
    across(
      everything(),
      ~mean(is.na(.))*100
    )
  ) %>%
  
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_pct"
  ) %>%
  
  arrange(
    desc(missing_pct)
  )

print(missingness)

#=========================================================
# Outcome distributions
#=========================================================

cat("\nOutbreak within 12 months:\n")

print(
  table(
    analytic_data$outbreak_within_12m
  )
)

cat("\nTime to next outbreak:\n")

print(
  summary(
    analytic_data$time_to_next_outbreak_days
  )
)

cat("\nTime to next outbreak (years):\n")

print(
  summary(
    analytic_data$time_to_next_outbreak_days /
      365.25
  )
)

#=========================================================
# Vaccination variables
#=========================================================

vaccination_vars <- c(
  "mcv1",
  "mcv2"
)

analytic_data %>%
  
  select(
    all_of(vaccination_vars)
  ) %>%
  
  summary()

#=========================================================
# Births
#=========================================================

cat("\nBirths summary:\n")

print(
  summary(
    analytic_data$births
  )
)

#=========================================================
# Campaign characteristics
#=========================================================

cat("\nCampaign duration:\n")

print(
  summary(
    analytic_data$campaign_duration_days
  )
)

cat("\nDoses delivered:\n")

print(
  summary(
    analytic_data$doses
  )
)

cat("\nAge target:\n")

print(
  table(
    analytic_data$age_target
  )
)

#=========================================================
# Historical outbreak burden
#=========================================================

historical_vars <- c(
  "previous_outbreaks_12m",
  "previous_cases_12m",
  "previous_outbreak_size",
  "previous_outbreak_duration"
)

analytic_data %>%
  
  select(
    all_of(historical_vars)
  ) %>%
  
  summary()

#=========================================================
# Region characteristics
#=========================================================

cat("\nRegion type:\n")

print(
  table(
    analytic_data$region_type
  )
)

cat("\nAdmin1:\n")

print(
  table(
    analytic_data$admin1
  )
)

#=========================================================
# Correlation assessment
#=========================================================

numeric_vars <- analytic_data %>%
  
  select(
    
    mcv1,
    mcv2,
    
    births,
    
    doses,
    
    campaign_duration_days,
    
    previous_outbreaks_12m,
    previous_cases_12m,
    
    previous_outbreak_size,
    previous_outbreak_duration,
    
    time_to_next_outbreak_days
    
  )

cor_matrix <- cor(
  numeric_vars,
  use = "pairwise.complete.obs"
)

print(
  round(
    cor_matrix,
    2
  )
)

#=========================================================
# Highly correlated predictors
#=========================================================

cor_df <- as.data.frame(
  as.table(cor_matrix)
)

high_corr <- cor_df %>%
  
  filter(
    Var1 != Var2,
    abs(Freq) >= 0.70
  ) %>%
  
  arrange(
    desc(abs(Freq))
  )

print(high_corr)

#=========================================================
# Potential outliers
#=========================================================

analytic_data %>%
  
  arrange(
    desc(previous_cases_12m)
  ) %>%
  
  select(
    admin1,
    sia_year,
    previous_cases_12m
  ) %>%
  
  head(10)

analytic_data %>%
  
  arrange(
    desc(doses)
  ) %>%
  
  select(
    admin1,
    sia_year,
    doses
  ) %>%
  
  head(10)

analytic_data %>%
  
  arrange(
    desc(births)
  ) %>%
  
  select(
    admin1,
    sia_year,
    births
  ) %>%
  
  head(10)

#=========================================================
# Candidate outcome:
# outbreak within 6 months
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    outbreak_within_6m =
      ifelse(
        time_to_next_outbreak_days <= 183,
        1,
        0
      )
    
  )

cat("\nOutbreak within 6 months:\n")

print(
  table(
    analytic_data$outbreak_within_6m,
    useNA = "ifany"
  )
)

#=========================================================
# Save updated dataset
#=========================================================

write_csv(
  analytic_data,
  "analytic_dataset.csv"
)

saveRDS(
  analytic_data,
  "analytic_dataset.rds"
)

#=========================================================
# 03_SIA_cleaning.R
# Supplementary Immunization Activities (SIA)
#=========================================================

library(tidyverse)
library(lubridate)
library(janitor)

#=========================================================
# Load data
#=========================================================

sia <- read_csv(
  "SIA_calendar_by_state.csv")

#=========================================================
# Initial inspection
#=========================================================

glimpse(sia)

names(sia)

#=========================================================
# Clean names
#=========================================================

sia <- sia %>%
  clean_names()

#=========================================================
# Rename state to admin1
#=========================================================

sia <- sia %>%
  rename(admin1 = state)

#=========================================================
# Standardize region names
#=========================================================

sia <- sia %>%
  mutate(
    admin1 = str_to_title(admin1)
  )

sia <- sia %>%
  mutate(
    admin1 = case_when(
      admin1 == "Benishangul" ~ "Benishangul Gumuz",
      admin1 == "Gambella" ~ "Gambela",
      TRUE ~ admin1
    )
  )

#=========================================================
# Convert dates
#=========================================================

sia <- sia %>%
  mutate(
    start_date = mdy(start_date),
    end_date   = mdy(end_date)
  )

#=========================================================
# Campaign duration
#=========================================================

sia <- sia %>%
  mutate(
    campaign_duration_days =
      as.numeric(end_date - start_date) + 1
  )

summary(sia$campaign_duration_days)

#=========================================================
# Extract year
#=========================================================

sia <- sia %>%
  mutate(
    sia_year = year(start_date)
  )

#=========================================================
# Missingness
#=========================================================

overall_missing <- sia %>%
  summarise(across(
    everything(),
    ~mean(is.na(.))*100
  )) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_pct"
  ) %>%
  arrange(desc(missing_pct))

print(overall_missing)

#=========================================================
# Region frequencies
#=========================================================

table(sia$admin1)

#=========================================================
# SIA frequency by year
#=========================================================

sia_by_year <- sia %>%
  count(sia_year)

print(sia_by_year)

#=========================================================
# Check age groups
#=========================================================

sort(unique(sia$age_group))

#=========================================================
# Create age-group categories
#=========================================================

sia <- sia %>%
  mutate(
    age_target = case_when(
      
      age_group %in% c("6-59M", "9-47M", "9-59M") ~
        "Under5",
      
      age_group == "6M-15Y" ~
        "Children",
      
      TRUE ~ "Other"
    )
  )

table(sia$age_target)

#Numberic age range
sia <- sia %>%
  mutate(
    
    target_upper_age_years = case_when(
      age_group %in% c("6-59M", "9-47M", "9-59M") ~ 5,
      age_group == "6M-15Y" ~ 15,
      TRUE ~ NA_real_
    )
    
  )

#=========================================================
# Doses summary
#=========================================================

summary(sia$doses)

sia %>%
  group_by(admin1) %>%
  summarise(
    total_doses =
      sum(doses, na.rm = TRUE),
    campaigns =
      n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_doses))

#=========================================================
# Inter-campaign intervals
#=========================================================

sia_intervals <- sia %>%
  arrange(admin1, start_date) %>%
  group_by(admin1) %>%
  mutate(
    days_since_previous_sia =
      as.numeric(
        start_date - lag(end_date)
      )
  )

summary(
  sia_intervals$days_since_previous_sia
)

#=========================================================
# Harmonization check
#=========================================================

setdiff(
  unique(sia$admin1),
  unique(measles_data$admin1)
)

setdiff(
  unique(measles_data$admin1),
  unique(sia$admin1)
)

#=========================================================
# Save
#=========================================================

saveRDS(
  sia,
  "sia_clean.rds"
)

write_csv(
  sia,
  "sia_clean.csv"
)

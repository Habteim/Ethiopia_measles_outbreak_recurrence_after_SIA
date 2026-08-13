#=========================================================
# 02_births_RI_cleaning.R
# Births and Routine Immunization Data Cleaning
#=========================================================

# Load libraries
#=========================================================
library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)
library(here)

#=========================================================
# Load data
#=========================================================
setwd("/Users/apple/Desktop/SKKU postdoc fellowship/SKKU research")
birth_ri <- read_csv(here(
  "Time to measles recurrence after SIA", "combined_epi_time_series_complete.csv"))

#=========================================================
# Initial inspection
#=========================================================

glimpse(birth_ri)

names(birth_ri)

#=========================================================
# Clean variable names
#=========================================================

birth_ri <- birth_ri %>%
  clean_names()

#=========================================================
# Convert date
#=========================================================

birth_ri <- birth_ri %>%
  mutate(
    date = mdy(time),
    year = year(date),
    month = month(date),
    epiweek = isoweek(date)
  )

summary(birth_ri$date)

#=========================================================
# Overall missingness
#=========================================================

overall_missing <- birth_ri %>%
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

write_csv(
  overall_missing,
  "overall_missingness_birth_RI.csv"
)

#=========================================================
# Missingness by year
#=========================================================

missing_by_year <- birth_ri %>%
  group_by(year) %>%
  summarise(
    total_records = n(),
    
    births_missing_pct =
      round(mean(is.na(births))*100,1),
    
    mcv1_missing_pct =
      round(mean(is.na(mcv1))*100,1),
    
    mcv2_missing_pct =
      round(mean(is.na(mcv2))*100,1),
    
    .groups = "drop"
  )

print(missing_by_year)

write_csv(
  missing_by_year,
  "missingness_by_year_birth_RI.csv"
)

#=========================================================
# Missingness by region
#=========================================================

missing_by_region <- birth_ri %>%
  group_by(state) %>%
  summarise(
    total_records = n(),
    
    births_missing_pct =
      round(mean(is.na(births))*100,1),
    
    mcv1_missing_pct =
      round(mean(is.na(mcv1))*100,1),
    
    mcv2_missing_pct =
      round(mean(is.na(mcv2))*100,1),
    
    .groups = "drop"
  )

print(missing_by_region)

write_csv(
  missing_by_region,
  "missingness_by_region_birth_RI.csv"
)

#=========================================================
# Structural missingness assessment for MCV2
#=========================================================

mcv2_pre2018 <- birth_ri %>%
  filter(year < 2018) %>%
  summarise(
    
    total_days = n(),
    
    missing_mcv2 =
      sum(is.na(mcv2)),
    
    zero_mcv2 =
      sum(mcv2 == 0,
          na.rm = TRUE)
    
  )

print(mcv2_pre2018)

#=========================================================
# Region harmonization
#=========================================================

sort(unique(birth_ri$state))

birth_ri <- birth_ri %>%
  mutate(
    state = str_trim(state),
    state = str_to_title(state)
  )


birth_ri <- birth_ri %>%
  mutate(
    state = case_when(
      
      state == "Addis Ababa" ~ "Addis Ababa",
      
      state == "Afar" ~ "Afar",
      
      state == "Amhara" ~ "Amhara",
      
      state %in% c(
        "Benishangul",
        "Benishangul Gumuz"
      ) ~ "Benishangul Gumuz",
      
      state %in% c(
        "Gambella",
        "Gambela"
      ) ~ "Gambela",
      
      state %in% c(
        "Hareri",
        "Harar",
        "Harari"
      ) ~ "Harari",
      
      state == "Dire Dawa" ~ "Dire Dawa",
      
      state == "Oromia" ~ "Oromia",
      
      state == "Somali" ~ "Somali",
      
      state == "Tigray" ~ "Tigray",
      
      TRUE ~ state
    )
  )


sort(unique(birth_ri$state))

#Rename state to admin1 and ensure title case for consistency with other datasets
birth_ri <- birth_ri %>%
  rename(admin1 = state) %>%
  mutate(
    admin1 = str_to_title(admin1)
  )

#Furthe harmonize Benishangul Gumuz naming
birth_ri <- birth_ri %>%
  mutate(
    admin1 = case_when(
      admin1 == "Benishangul" ~ "Benishangul Gumuz",
      TRUE ~ admin1
    )
  )

#=========================================================
# Birth variable checks
#=========================================================

summary(birth_ri$births)

birth_ri %>%
  filter(births < 0)

birth_ri %>%
  filter(is.na(births))

#=========================================================
# Coverage checks
#=========================================================

summary(birth_ri$mcv1)

summary(birth_ri$mcv2)

sum(birth_ri$mcv1 < 0,
    na.rm = TRUE)

sum(birth_ri$mcv1 > 1,
    na.rm = TRUE)

sum(birth_ri$mcv2 < 0,
    na.rm = TRUE)

sum(birth_ri$mcv2 > 1,
    na.rm = TRUE)

#=========================================================
# Coverage trends by region
#=========================================================

coverage_summary <- birth_ri %>%
  group_by(admin1) %>%
  summarise(
    
    mean_births =
      mean(births,
           na.rm = TRUE),
    
    mean_mcv1 =
      mean(mcv1,
           na.rm = TRUE),
    
    mean_mcv2 =
      mean(mcv2,
           na.rm = TRUE),
    
    .groups = "drop"
  )

print(coverage_summary)

write_csv(
  coverage_summary,
  "coverage_summary_region.csv"
)

#=========================================================
# Birth trends
#=========================================================

ggplot(
  birth_ri %>%
    filter(admin1 == "Oromia"),
  aes(date, births)
) +
  geom_line()

ggplot(
  birth_ri %>%
    filter(admin1 == "Amhara"),
  aes(date, births)
) +
  geom_line()

#=========================================================
# National coverage trends
#=========================================================

national_trend <- birth_ri %>%
  group_by(year) %>%
  summarise(
    
    births =
      sum(births,
          na.rm = TRUE),
    
    mcv1 =
      mean(mcv1,
           na.rm = TRUE),
    
    mcv2 =
      mean(mcv2,
           na.rm = TRUE),
    
    .groups = "drop"
  )

print(national_trend)

#=========================================================
# Weekly aggregation
#=========================================================

birth_ri_weekly <- birth_ri %>%
  mutate(
    week_start =
      floor_date(date,
                 unit = "week")
  ) %>%
  group_by(
    admin1,
    week_start
  ) %>%
  summarise(
    
    births =
      sum(births,
          na.rm = TRUE),
    
    mcv1 =
      mean(mcv1,
           na.rm = TRUE),
    
    mcv2 =
      mean(mcv2,
           na.rm = TRUE),
    
    .groups = "drop"
  )

glimpse(birth_ri_weekly)

#=========================================================
# Export cleaned datasets
#=========================================================

saveRDS(
  birth_ri,
  "birth_RI_clean_daily.rds"
)

saveRDS(
  birth_ri_weekly,
  "birth_RI_clean_weekly.rds"
)

write_csv(
  birth_ri_weekly,
  "birth_RI_clean_weekly.csv"
)

#=========================================================
# Final checks
#=========================================================

cat(
  "\nDaily dataset rows:",
  nrow(birth_ri),
  "\n"
)

cat(
  "\nWeekly dataset rows:",
  nrow(birth_ri_weekly),
  "\n"
)

table(birth_ri$admin1)

# =========================================================
# 01_data_cleaning_missingness_harmonization.R
# Measles surveillance data cleaning and harmonization
# Ethiopia, 2005–2025
# =========================================================

# =========================================================
# 1. LOAD LIBRARIES
# =========================================================

library(readr)
library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(naniar)
library(gtsummary)
library(stringr)
library(scales)

# =========================================================
# 2. IMPORT DATA
# =========================================================

#setwd("~/Desktop/SKKU postdoc fellowship/SKKU research/Time to measles recurrence after SIA/")

measles_raw <- read_csv(
  "/Users/apple/Desktop/SKKU_postdoc_fellowship/SKKU_research/Time to measles recurrence after SIA/ETH_MR_linelist_SACEMA_export_2025_mod.csv")

# Preserve untouched raw dataset
measles_data <- measles_raw

# =========================================================
# 3. INITIAL DATA EXPLORATION
# =========================================================

# Data set dimensions
dim(measles_data)

# Variable names
names(measles_data)

# Structure
glimpse(measles_data)

# Overall missingness
pct_miss(measles_data)

# Missingness by variable
colSums(is.na(measles_data))

# =========================================================
# 4. STANDARDIZE VARIABLE NAMES
# =========================================================

measles_data <- measles_data %>%
  clean_names()

# =========================================================
# 5. PRESERVE RAW VARIABLES
# =========================================================

measles_data <- measles_data %>%
  mutate(
    admin1_raw = admin1,
    admin2_raw = admin2,
    sex_raw = sex,
    age_months_raw = age,
    final_diagnosis_raw = final_diagnosis
  )

# =========================================================
# 6. STANDARDIZE TEXT VARIABLES
# =========================================================

measles_data <- measles_data %>%
  mutate(
    across(
      c(admin1, admin2),
      ~ str_squish(str_to_title(.))
    )
  )

# =========================================================
# 7. DATE CONVERSION
# =========================================================

date_vars <- c(
  "date_onset",
  "date_rec_vax",
  "date_lab_recived",
  "date_spe_recived",
  "date_res_recived"
)

measles_data <- measles_data %>%
  mutate(
    across(
      all_of(date_vars),
      ~ suppressWarnings(mdy(.))
    )
  )

# =========================================================
# 8. DERIVE TEMPORAL VARIABLES
# =========================================================

measles_data <- measles_data %>%
  mutate(
    onset_year = year(date_onset),
    onset_month = month(date_onset),
    onset_week = isoweek(date_onset),
    onset_epiweek = epiweek(date_onset)
  )

# =========================================================
# 9. MISSINGNESS ANALYSIS
# =========================================================

# ---------------------------------------------------------
# 9A. Variable-level missingness
# ---------------------------------------------------------

variable_missingness <- measles_data %>%
  summarise(
    across(
      everything(),
      ~ mean(is.na(.)) * 100
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_pct"
  ) %>%
  arrange(desc(missing_pct))

print(variable_missingness)

# ---------------------------------------------------------
# 9B. Temporal missingness
# ---------------------------------------------------------

missing_by_year <- measles_data %>%
  group_by(onset_year) %>%
  summarise(
    total_records = n(),
    
    age_missing_pct =
      round(mean(is.na(age_months_raw)) * 100, 1),
    
    sex_missing_pct =
      round(mean(is.na(sex_raw)) * 100, 1),
    
    vaccination_missing_pct =
      round(mean(is.na(doses_received)) * 100, 1),
    
    onset_date_missing_pct =
      round(mean(is.na(date_onset)) * 100, 1),
    
    specimen_date_missing_pct =
      round(mean(is.na(date_spe_recived)) * 100, 1),
    
    result_date_missing_pct =
      round(mean(is.na(date_res_recived)) * 100, 1)
  ) %>%
  arrange(onset_year)

print(missing_by_year)

# ---------------------------------------------------------
# 9C. Regional missingness
# ---------------------------------------------------------

missing_by_region <- measles_data %>%
  group_by(admin1) %>%
  summarise(
    total_records = n(),
    
    age_missing_pct =
      round(mean(is.na(age_months_raw)) * 100, 1),
    
    sex_missing_pct =
      round(mean(is.na(sex_raw)) * 100, 1),
    
    vaccination_missing_pct =
      round(mean(is.na(doses_received)) * 100, 1),
    
    onset_date_missing_pct =
      round(mean(is.na(date_onset)) * 100, 1),
    
    specimen_date_missing_pct =
      round(mean(is.na(date_spe_recived)) * 100, 1),
    
    result_date_missing_pct =
      round(mean(is.na(date_res_recived)) * 100, 1)
  ) %>%
  arrange(desc(total_records))

print(missing_by_region)


#Check which observation has a missing admin1 value
measles_data %>%
  filter(is.na(admin1))

#Remove an observation with a missing admin1 but non-missing admin2 (Addis Ababa) to avoid confusion in regional analyses.
measles_data <- measles_data %>%
  filter(!is.na(admin1))
# ---------------------------------------------------------
# 9D. Structural missingness for MCV2
# ---------------------------------------------------------

measles_data <- measles_data %>%
  mutate(
    mcv2_structural_missing = case_when(
      onset_year < 2018 ~ "Structural Missingness",
      TRUE ~ "Observed Period"
    )
  )

# ---------------------------------------------------------
# 9E. Missingness visualization
# ---------------------------------------------------------

# Overall missingness map
vis_miss(measles_data, warn_large_data = FALSE)

# Missingness by variable
gg_miss_var(measles_data)

# Missingness combinations
gg_miss_upset(measles_data)

# =========================================================
# 10. AGE CLEANING
# =========================================================

# Examine age distribution
summary(measles_data$age_months_raw)

# Zero ages
table(measles_data$age_months_raw == 0,
      useNA = "ifany")

# Implausibly large ages
sum(measles_data$age_months_raw > 1200,
    na.rm = TRUE)

# Recode suspicious zero ages to missing
# measles_data <- measles_data %>%
#   mutate(
#     age_months = case_when(
#       age_months_raw == 0 ~ NA_real_,
#       TRUE ~ as.numeric(age_months_raw)
#     )
#   )

# Recode zero ages to 0.5 months (15 days) to retain them in 
#analysis while acknowledging they are likely infants
measles_data <- measles_data %>%
  mutate(
    age_months = case_when(
      age_months_raw == 0 ~ 0.5,
      TRUE ~ age_months_raw
    )
  )

# Convert months to years
measles_data <- measles_data %>%
  mutate(
    age_years = age_months / 12
  )

# Create age groups
measles_data <- measles_data %>%
  mutate(
    age_group = case_when(
      age_years < 1 ~ "<1 year",
      age_years >= 1 & age_years < 5 ~ "1-4 years",
      age_years >= 5 & age_years < 15 ~ "5-14 years",
      age_years >= 15 ~ "15+ years",
      TRUE ~ NA_character_
    )
  )

# =========================================================
# 11. SEX VARIABLE CLEANING
# =========================================================

table(measles_data$sex_raw,
      useNA = "ifany")

measles_data <- measles_data %>%
  mutate(
    sex = case_when(
      sex_raw == 1 ~ "Male",
      sex_raw == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  )

# =========================================================
# 12. FINAL DIAGNOSIS CLEANING
# =========================================================

table(measles_data$final_diagnosis_raw,
      useNA = "ifany")

# Label diagnosis categories
measles_data <- measles_data %>%
  mutate(
    final_diagnosis_label = case_when(
      final_diagnosis_raw == 0 ~
        "Discarded Measles/Rubella",
      
      final_diagnosis_raw == 1 ~
        "Clinically Compatible",
      
      final_diagnosis_raw == 2 ~
        "Laboratory Confirmed",
      
      final_diagnosis_raw == 3 ~
        "Epidemiologically Linked",
      
      final_diagnosis_raw == 5 ~
        "Rubella Confirmed",
      
      TRUE ~ NA_character_
    )
  )

# Binary confirmed measles indicator
measles_data <- measles_data %>%
  mutate(
    confirmed_measles = case_when(
      final_diagnosis_raw %in% c(2, 3) ~ 1,
      final_diagnosis_raw %in% c(0, 5) ~ 0,
      TRUE ~ NA_real_
    )
  )

# =========================================================
# 13. VACCINATION VARIABLE CLEANING
# =========================================================

table(measles_data$doses_received,
      useNA = "ifany")

measles_data <- measles_data %>%
  mutate(
    doses_received = ifelse(doses_received == 9,
                            NA,
                            doses_received)
  )


measles_data <- measles_data %>%
  mutate(
    vaccinated_status = case_when(
      is.na(doses_received) ~ "Unknown",
      doses_received == 0 ~ "Unvaccinated",
      doses_received >= 1 ~ "Vaccinated"
    )
  )

# =========================================================
# 14. ADMINISTRATIVE HARMONIZATION
# =========================================================

# Harmonize regions created from former SNNPR
measles_data <- measles_data %>%
  mutate(
    admin1 = str_to_title(admin1)
  )

measles_data <- measles_data %>%
  mutate(
    admin1 = case_when(
      # South Ethiopia
      admin1 == "Snnpr" & (admin2 %in% c(
        "South Omo","SOUTH OMO",
        "Gamo Gofa","GAMO GOFA",
        "Gedeo","GEDEO",
        "Wolayta","WOLAYTA",
        "Derashe","DERASHE",
        "Konso","KONSO",
        "AMARO","Amaro Special Woreda","AMARO SPECIAL WOREDA", "Amaro",
        "Ale Special Woreda","ALE SPECIAL WOREDA",
        "BASKETO","Basketo",
        "Burji","BURJI",
        "Burji Special Woreda","BURJI SPECIAL WOREDA",
        "Derashe Special Woreda","DERASHE SPECIAL WOREDA",
        "DIRASHIE","Dirashie",
        "Gamo","GAMO",
        "Goffa","GOFFA",
        "Kindo Didaye","KINDO DIDAYE",
        "KUCHA","Kucha",
        "S Omo","S OMO",
        "Segen","SEGEN",
        "Segen Surrounding Zone","SEGEN SURROUNDING ZONE",
        "West Omo","WEST OMO"
      ) | admin2 == "" | is.na(admin2)) ~ "South Ethiopia",   # add empty/missing admin2
      
      # Central Ethiopia
      admin1 == "Snnpr" & admin2 %in% c(
        "Guraghe","GURAGHE",
        "Yem","YEM",
        "Silti","SILTI",
        "Silte","SILTE",   # ✅ added Silte explicitly
        "Hadiya","HADIYA",
        "Kembata/Tembaro","KEMBATA/TEMBARO",
        "Kembata Tembaro","KEMBATA TEMBARO",
        "Alaba","ALABA",
        "Halaba","HALABA",
        "Silite","SILITE",
        "Yemi","YEMI",
        "E Shewa","E SHEWA"
      ) ~ "Central Ethiopia",
      
      # South West Ethiopia
      admin1 == "Snnpr" & admin2 %in% c(
        "Sheka","SHEKA",
        "Bench Maji","BENCH MAJI",
        "Kefa","KEFA",
        "Dawro","DAWRO",
        "Konta","KONTA",
        "Bench Sheko","BENCH SHEKO"
      ) ~ "South West Ethiopia",
      
      # Sidama
      admin1 == "Snnpr" & admin2 %in% c(
        "Sidama","SIDAMA",
        "Awassa Ca","AWASSA CA",
        "Aleta Chiko","ALETA CHIKO",
        "Hawassa Ca","HAWASSA CA"
      ) ~ "Sidama",
      
      TRUE ~ admin1
    )
  )


# Additional region standardization

measles_data <- measles_data %>%
  mutate(admin1 = case_when(
    admin1 == "DIRE DAWA" ~ "Dire Dawa",
    admin1 == "DIre Dawa" ~ "Dire Dawa",
    admin1 == "ADDIS ABABA" ~ "Addis Ababa",
    admin1 == "AFAR" ~ "Afar", 
    admin1 == "AMHARA" ~ "Amhara",
    admin1 == "Benshangul Gumuz" ~ "Benishangul Gumuz",
    admin1 == "BENISHANGUL GUMUZ " ~ "Benishangul Gumuz",
    admin1 == "Benishangul" ~ "Benishangul Gumuz",
    admin1 == "GAMBELLA" ~ "Gambela",
    admin1 == "Gambella" ~ "Gambela",
    admin1 == "HARERI" ~ "Harari",
    admin1 == "Hareri" ~ "Harari",
    admin1 == "OROMIA" ~ "Oromia",
    admin1 == "SOMALI" ~ "Somali",
    admin1 == "TIGRAY" ~ "Tigray",
    admin1 == "South West" ~ "South West Ethiopia",
    TRUE ~ admin1
  ))


write_rds(
  measles_data,
  "measles_clean_harmonized.rds"
)


# saveRDS(
#   measles_data,
#   "measles_clean_harmonized.rds"
# )

# =========================================================
# 15. DUPLICATE ASSESSMENT
# =========================================================

duplicate_records <- measles_data %>%
  duplicated()

sum(duplicate_records)

# Optional duplicate extraction
possible_duplicates <- measles_data %>%
  group_by(
    admin1,
    admin2,
    date_onset,
    sex,
    age_months
  ) %>%
  filter(n() > 1)


exact_dups <- measles_data %>%
  group_by(across(everything())) %>%
  filter(n() > 1)
# =========================================================
# 16. DATE PLAUSIBILITY CHECKS
# =========================================================

summary(measles_data$date_onset)

# Delays

measles_data <- measles_data %>%
  mutate(
    delay_onset_to_specimen =
      as.numeric(date_spe_recived - date_onset),
    
    delay_specimen_to_result =
      as.numeric(date_res_recived - date_spe_recived),
    
    delay_onset_to_result =
      as.numeric(date_res_recived - date_onset)
  )

summary(measles_data$delay_onset_to_result)

# Negative intervals
sum(measles_data$delay_onset_to_result < 0,
    na.rm = TRUE)

# Delays > 60 days
sum(measles_data$delay_onset_to_result > 60,
    na.rm = TRUE)


#Optional additional analysis. 
#Inspect these date values 
measles_data %>%
  filter(delay_onset_to_result < 0) %>%
  select(
    admin1,
    admin2,
    date_onset,
    date_res_recived,
    delay_onset_to_result
  ) %>%
  arrange(delay_onset_to_result)

ggplot(
  measles_data %>%
    filter(!is.na(delay_onset_to_result)),
  aes(delay_onset_to_result)
) +
  geom_histogram(bins = 100)

#Zoom
ggplot(
  measles_data %>%
    filter(
      !is.na(delay_onset_to_result),
      delay_onset_to_result >= 0,
      delay_onset_to_result <= 100
    ),
  aes(delay_onset_to_result)
) +
  geom_histogram(bins = 50)

#Where do the >60 delays occur
long_delays <- measles_data %>%
  filter(delay_onset_to_result > 60)

long_delays %>%
  count(onset_year, sort = TRUE)

long_delays %>%
  count(admin1, sort = TRUE)

# =========================================================
# 17. EXPORT OUTPUTS
# =========================================================

# saveRDS(
#   measles_data,
#   "measles_clean_harmonized.rds"
# )


# Save cleaned data set
write_rds(
  measles_data,
  "cleaned_measles_surveillance.rds"
)

# Save missingness outputs
write_csv(
  variable_missingness,
  "variable_missingness_summary.csv"
)

write_csv(
  missing_by_year,
  "missingness_by_year.csv"
)

write_csv(
  missing_by_region,
  "missingness_by_region.csv"
)

# Save possible duplicates
write_csv(
  exact_dups,
  "possible_duplicate_records.csv"
)

# =========================================================
# END OF SCRIPT
# =========================================================
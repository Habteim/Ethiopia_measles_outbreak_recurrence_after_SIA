#=========================================================
# 07_analytic_dataset_construction.R
# Master analytic dataset
# One row = one SIA event
#=========================================================

library(tidyverse)
library(lubridate)
library(purrr)

#=========================================================
# Annual Birth / RI summaries
#=========================================================

birth_ri_annual <- birth_ri %>%
  group_by(
    admin1,
    year
  ) %>%
  summarise(
    
    births =
      mean(
        births,
        na.rm = TRUE
      ),
    
    mcv1 =
      mean(
        mcv1,
        na.rm = TRUE
      ),
    
    mcv2 =
      mean(
        mcv2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

#=========================================================
# Start with SIA outcomes
#=========================================================

analytic_data <- sia_outcomes %>%
  mutate(
    
    lookup_year =
      sia_year - 1
    
  )

#=========================================================
# Attach previous-year Birth / RI data
#=========================================================

analytic_data <- analytic_data %>%
  left_join(
    
    birth_ri_annual,
    
    by = c(
      "admin1",
      "lookup_year" = "year"
    )
    
  )

#=========================================================
# Attach region metadata
#=========================================================

analytic_data <- analytic_data %>%
  left_join(
    
    region_lookup,
    
    by = "admin1"
    
  )

#=========================================================
# Historical outbreak burden
#=========================================================

historical_burden <- sia %>%
  
  mutate(row_id = row_number()) %>%
  
  mutate(
    
    previous_outbreaks_12m =
      map_int(
        row_id,
        ~{
          this_admin1 <- admin1[.x]
          this_start  <- start_date[.x]
          
          outbreaks %>%
            filter(
              admin1 == this_admin1,
              outbreak_start < this_start,
              outbreak_start >= this_start - 365
            ) %>%
            nrow()
        }
      ),
    
    previous_cases_12m =
      map_dbl(
        row_id,
        ~{
          this_admin1 <- admin1[.x]
          this_start  <- start_date[.x]
          
          outbreaks %>%
            filter(
              admin1 == this_admin1,
              outbreak_start < this_start,
              outbreak_start >= this_start - 365
            ) %>%
            summarise(
              total_cases =
                sum(total_cases, na.rm = TRUE)
            ) %>%
            pull(total_cases)
        }
      ),
    
    previous_outbreak_size =
      map_dbl(
        row_id,
        ~{
          this_admin1 <- admin1[.x]
          this_start  <- start_date[.x]
          
          tmp <- outbreaks %>%
            filter(
              admin1 == this_admin1,
              outbreak_start < this_start
            ) %>%
            arrange(desc(outbreak_start))
          
          if(nrow(tmp) == 0) {
            NA_real_
          } else {
            tmp$total_cases[1]
          }
        }
      ),
    
    previous_outbreak_duration =
      map_dbl(
        row_id,
        ~{
          this_admin1 <- admin1[.x]
          this_start  <- start_date[.x]
          
          tmp <- outbreaks %>%
            filter(
              admin1 == this_admin1,
              outbreak_start < this_start
            ) %>%
            arrange(desc(outbreak_start))
          
          if(nrow(tmp) == 0) {
            NA_real_
          } else {
            tmp$duration_weeks[1]
          }
        }
      )
    
  ) %>%
  
  select(-row_id)


#Check the following 
summary(historical_burden$previous_outbreaks_12m)

summary(historical_burden$previous_cases_12m)

summary(historical_burden$previous_outbreak_size)

summary(historical_burden$previous_outbreak_duration)
#=========================================================
# Merge historical burden
#=========================================================

analytic_data <- analytic_data %>%
  
  left_join(
    
    historical_burden,
    
    by = c(
      "admin1",
      "start_date",
      "end_date",
      "sia_year"
    )
    
  )

#=========================================================
# Replace missing burden values
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    previous_outbreaks_12m =
      replace_na(
        previous_outbreaks_12m,
        0
      ),
    
    previous_cases_12m =
      replace_na(
        previous_cases_12m,
        0
      ),
    
    previous_outbreak_size =
      replace_na(
        previous_outbreak_size,
        0
      ),
    
    previous_outbreak_duration =
      replace_na(
        previous_outbreak_duration,
        0
      )
    
  )

#=========================================================
# Variable checks
#=========================================================

cat("\nRows:\n")
print(nrow(analytic_data))

cat("\nColumns:\n")
print(ncol(analytic_data))

cat("\nRegion distribution:\n")
print(
  table(
    analytic_data$admin1
  )
)

cat("\nRegion type distribution:\n")
print(
  table(
    analytic_data$region_type
  )
)

cat("\nOutbreak within 12 months:\n")
print(
  table(
    analytic_data$outbreak_within_12m
  )
)

cat("\nTime to outbreak summary:\n")
print(
  summary(
    analytic_data$time_to_next_outbreak_days
  )
)

cat("\nMCV1 summary:\n")
print(
  summary(
    analytic_data$mcv1
  )
)

cat("\nMCV2 summary:\n")
print(
  summary(
    analytic_data$mcv2
  )
)

cat("\nBirths summary:\n")
print(
  summary(
    analytic_data$births
  )
)

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
    
    names_to =
      "variable",
    
    values_to =
      "missing_pct"
    
  ) %>%
  
  arrange(
    desc(missing_pct)
  )

print(missingness)

#=========================================================
# Save outputs
#=========================================================

write_csv(
  analytic_data,
  "analytic_dataset.csv"
)

saveRDS(
  analytic_data,
  "analytic_dataset.rds"
)

write_csv(
  missingness,
  "analytic_dataset_missingness.csv"
)

#=========================================================
# End
#=========================================================
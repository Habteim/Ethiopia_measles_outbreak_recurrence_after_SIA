#=========================================================
# 07_analytic_dataset_construction.R
#
# Master analytic dataset
#
# One row = one region-SIA observation
#
# Primary survival analysis:
# Each region-SIA observation is followed until the earliest
# of:
#
# 1. First qualifying outbreak after the washout period
# 2. Next SIA in the same region
# 3. End of surveillance
#=========================================================

library(tidyverse)
library(lubridate)
library(purrr)

#=========================================================
# Annual Birth / Routine Immunization summaries
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
# Start with corrected SIA survival outcomes
#=========================================================

analytic_data <- sia_outcomes %>%
  
  mutate(
    
    #-------------------------------------------------------
    # Use the year preceding the SIA for birth and routine
    # immunization indicators
    #-------------------------------------------------------
    
    lookup_year =
      sia_year - 1
    
  )


#=========================================================
# Attach previous-year Birth / Routine Immunization data
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
# Calendar period
#
# Accounts for potential changes in surveillance,
# immunization services, campaign implementation,
# and other secular changes over the study period.
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    calendar_period =
      
      case_when(
        
        sia_year <= 2010 ~ "2005-2010",
        
        sia_year <= 2015 ~ "2011-2015",
        
        sia_year <= 2020 ~ "2016-2020",
        
        TRUE ~ "2021-2025"
        
      )
    
  ) %>%
  
  mutate(
    
    calendar_period =
      
      factor(
        
        calendar_period,
        
        levels = c(
          "2005-2010",
          "2011-2015",
          "2016-2020",
          "2021-2025"
        )
        
      )
    
  )


#=========================================================
# Historical outbreak burden
#
# Calculated during the 12 months preceding the start of
# each SIA.
#=========================================================

historical_burden <- sia %>%
  
  mutate(
    row_id = row_number()
  ) %>%
  
  mutate(
    
    #-------------------------------------------------------
    # Number of outbreaks in previous 12 months
    #-------------------------------------------------------
    
    previous_outbreaks_12m =
      
      map_int(
        
        row_id,
        
        ~{
          
          this_admin1 <-
            admin1[.x]
          
          this_start <-
            start_date[.x]
          
          outbreaks %>%
            
            filter(
              
              admin1 == this_admin1,
              
              outbreak_start < this_start,
              
              outbreak_start >=
                this_start - 365
              
            ) %>%
            
            nrow()
          
        }
        
      ),
    
    
    #-------------------------------------------------------
    # Total measles cases in previous 12 months
    #-------------------------------------------------------
    
    previous_cases_12m =
      
      map_dbl(
        
        row_id,
        
        ~{
          
          this_admin1 <-
            admin1[.x]
          
          this_start <-
            start_date[.x]
          
          outbreaks %>%
            
            filter(
              
              admin1 == this_admin1,
              
              outbreak_start < this_start,
              
              outbreak_start >=
                this_start - 365
              
            ) %>%
            
            summarise(
              
              total_cases =
                sum(
                  total_cases,
                  na.rm = TRUE
                )
              
            ) %>%
            
            pull(
              total_cases
            )
          
        }
        
      ),
    
    
    #-------------------------------------------------------
    # Size of most recent outbreak before SIA
    #-------------------------------------------------------
    
    previous_outbreak_size =
      
      map_dbl(
        
        row_id,
        
        ~{
          
          this_admin1 <-
            admin1[.x]
          
          this_start <-
            start_date[.x]
          
          tmp <- outbreaks %>%
            
            filter(
              
              admin1 == this_admin1,
              
              outbreak_start < this_start
              
            ) %>%
            
            arrange(
              desc(outbreak_start)
            )
          
          if(
            nrow(tmp) == 0
          ) {
            
            NA_real_
            
          } else {
            
            tmp$total_cases[1]
            
          }
          
        }
        
      ),
    
    
    #-------------------------------------------------------
    # Duration of most recent outbreak before SIA
    #-------------------------------------------------------
    
    previous_outbreak_duration =
      
      map_dbl(
        
        row_id,
        
        ~{
          
          this_admin1 <-
            admin1[.x]
          
          this_start <-
            start_date[.x]
          
          tmp <- outbreaks %>%
            
            filter(
              
              admin1 == this_admin1,
              
              outbreak_start < this_start
              
            ) %>%
            
            arrange(
              desc(outbreak_start)
            )
          
          if(
            nrow(tmp) == 0
          ) {
            
            NA_real_
            
          } else {
            
            tmp$duration_weeks[1]
            
          }
          
        }
        
      )
    
  ) %>%
  
  select(
    -row_id
  )


#=========================================================
# Historical burden diagnostics
#=========================================================

cat(
  "\nPrevious outbreaks in 12 months:\n"
)

print(
  summary(
    historical_burden$previous_outbreaks_12m
  )
)


cat(
  "\nPrevious cases in 12 months:\n"
)

print(
  summary(
    historical_burden$previous_cases_12m
  )
)


cat(
  "\nPrevious outbreak size:\n"
)

print(
  summary(
    historical_burden$previous_outbreak_size
  )
)


cat(
  "\nPrevious outbreak duration:\n"
)

print(
  summary(
    historical_burden$previous_outbreak_duration
  )
)


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
# Replace missing historical burden values
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
# Create outbreak history variables
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    prev_outbreak_group =
      
      if_else(
        
        previous_outbreaks_12m == 0,
        
        "None",
        
        ">=1"
        
      ),
    
    
    prev_outbreak_group =
      
      factor(
        
        prev_outbreak_group,
        
        levels = c(
          "None",
          ">=1"
        )
        
      )
    
  )


#=========================================================
# Create scaled variables for regression
#
# IMPORTANT:
#
# births represents an estimated NUMBER of births.
#
# births_1000 = births / 1000
#
# It should therefore be described as:
#
# "Estimated births, per 1,000 births"
#
# and NOT "births per 1,000 children".
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    births_1000 =
      births / 1000
    
  )


#=========================================================
# Variable checks
#=========================================================

cat("\n========================================\n")
cat("ANALYTIC DATASET DIAGNOSTICS\n")
cat("========================================\n")


cat("\nNumber of rows:\n")

print(
  nrow(analytic_data)
)


cat("\nNumber of columns:\n")

print(
  ncol(analytic_data)
)


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


cat("\nCalendar period distribution:\n")

print(
  
  table(
    analytic_data$calendar_period
  )
  
)


cat("\nSurvival events:\n")

print(
  
  table(
    analytic_data$outbreak_after_sia
  )
  
)


cat("\nCensoring reasons:\n")

print(
  
  table(
    analytic_data$censoring_reason
  )
  
)


cat("\nFollow-up time summary:\n")

print(
  
  summary(
    analytic_data$time_to_event_or_censor_days
  )
  
)


cat("\nTime to outbreak among events:\n")

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


cat("\nEstimated births summary:\n")

print(
  
  summary(
    analytic_data$births
  )
  
)


cat("\nEstimated births / 1,000 summary:\n")

print(
  
  summary(
    analytic_data$births_1000
  )
  
)


cat("\nPrevious outbreaks (12 months):\n")

print(
  
  summary(
    analytic_data$previous_outbreaks_12m
  )
  
)


cat("\nPrevious cases (12 months):\n")

print(
  
  summary(
    analytic_data$previous_cases_12m
  )
  
)


#=========================================================
# Check for duplicate region-SIA observations
#=========================================================

duplicate_sia_check <- analytic_data %>%
  
  count(
    
    admin1,
    start_date,
    end_date
    
  ) %>%
  
  filter(
    n > 1
  )


cat(
  "\nDuplicate region-SIA observations:\n"
)

print(
  duplicate_sia_check
)


#=========================================================
# Missingness assessment
#=========================================================

missingness <- analytic_data %>%
  
  summarise(
    
    across(
      
      everything(),
      
      ~mean(
        is.na(.)
      ) * 100
      
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


cat(
  "\nMissingness assessment:\n"
)

print(
  missingness
)


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
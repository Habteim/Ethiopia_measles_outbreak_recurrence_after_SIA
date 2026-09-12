#=========================================================
# 10_survival_analysis_preparation.R
#=========================================================

library(
  tidyverse
)

library(
  survival
)

#=========================================================
# Load analytic dataset
#=========================================================

analytic_data <- readRDS(
  "analytic_dataset.rds"
)

#=========================================================
# Verify duplicated SIA variables
#=========================================================

cat(
  "\nChecking duplicated variables:\n"
)

cat(
  "\nage_group identical:\n"
)

print(
  all(
    analytic_data$age_group.x ==
      analytic_data$age_group.y
  )
)

cat(
  "\nage_target identical:\n"
)

print(
  all(
    analytic_data$age_target.x ==
      analytic_data$age_target.y
  )
)

cat(
  "\ndoses identical:\n"
)

print(
  all(
    analytic_data$doses.x ==
      analytic_data$doses.y
  )
)

#=========================================================
# Clean duplicated SIA variables
#=========================================================

analytic_data <- analytic_data %>%
  
  select(
    
    -age_group.x,
    -age_target.x,
    -doses.x
    
  ) %>%
  
  rename(
    
    age_group =
      age_group.y,
    
    age_target =
      age_target.y,
    
    doses =
      doses.y
    
  )

#=========================================================
# Create clean survival analysis dataset
#=========================================================

survival_data <- analytic_data %>%
  
  mutate(
    
    #-----------------------------------
    # Event indicator
    #-----------------------------------
    
    event =
      
      as.integer(
        outbreak_after_sia == 1
      ),
    
    
    #-----------------------------------
    # Survival time
    #-----------------------------------
    
    time =
      
      time_to_event_or_censor_days,
    
    
    #-----------------------------------
    # Region type
    #-----------------------------------
    
    region_type =
      
      factor(
        region_type,
        
        levels =
          c(
            "Agrarian",
            "Pastoralist",
            "Urban"
          )
      ),
    
    
    #-----------------------------------
    # Calendar period
    #-----------------------------------
    
    calendar_period =
      
      factor(
        calendar_period,
        
        levels =
          c(
            "2005-2010",
            "2011-2015",
            "2016-2020",
            "2021-2025"
          )
      ),
    
    
    #-----------------------------------
    # Previous outbreak history
    #-----------------------------------
    
    prev_outbreak_group =
      
      factor(
        prev_outbreak_group,
        
        levels =
          c(
            "None",
            ">=1"
          )
      ),
    
    
    #-----------------------------------
    # Age target
    #-----------------------------------
    
    age_target =
      
      factor(
        age_target
      )
    
  ) %>%
  
  select(
    
    admin1,
    
    start_date,
    end_date,
    
    sia_year,
    
    calendar_period,
    
    region_type,
    
    # SIA characteristics
    
    age_group,
    age_target,
    
    target_upper_age_years,
    
    campaign_duration_days,
    
    doses,
    
    # Vaccination
    
    mcv1,
    mcv2,
    
    # Demography
    
    births,
    
    # Historical outbreak burden
    
    previous_outbreaks_12m,
    
    previous_cases_12m,
    
    previous_outbreak_size,
    
    previous_outbreak_duration,
    
    prev_outbreak_group,
    
    # Survival variables
    
    event,
    
    time,
    
    censoring_reason
    
  )

#=========================================================
# Create transformed predictors for Cox regression (address skewness)
#=========================================================

survival_data <- survival_data %>%
  
  mutate(
    
    births_1000 =
      
      births / 1000,
    
    log_previous_cases =
      
      log1p(
        previous_cases_12m
      ),
    
    
    log_previous_outbreak_size =
      
      log1p(
        previous_outbreak_size
      ),
    
    
    log_previous_outbreak_duration =
      
      log1p(
        previous_outbreak_duration
      ),
    
    
    log_doses =
      
      log1p(
        doses
      )
    
  )

#=========================================================
# Check survival dataset
#=========================================================

cat(
  "\n========================================\n"
)

cat(
  "SURVIVAL DATASET\n"
)

cat(
  "========================================\n"
)

cat(
  "\nObservations:\n"
)

print(
  nrow(
    survival_data
  )
)

cat(
  "\nVariables:\n"
)

print(
  ncol(
    survival_data
  )
)

cat(
  "\nEvents:\n"
)

print(
  table(
    survival_data$event
  )
)

cat(
  "\nMissing values:\n"
)

print(
  
  survival_data %>%
    
    summarise(
      
      across(
        
        everything(),
        
        ~sum(
          is.na(.)
        )
        
      )
      
    )
  
)

#=========================================================
# Save survival dataset
#=========================================================

write_csv(
  
  survival_data,
  
  "survival_dataset.csv"
  
)

saveRDS(
  
  survival_data,
  
  "survival_dataset.rds"
  
)

cat(
  "\n========================================\n"
)

cat(
  "SURVIVAL DATA PREPARATION COMPLETE\n"
)

cat(
  "========================================\n"
)

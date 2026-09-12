#=========================================================
# 10_cox_regression.R
#=========================================================

library(
  tidyverse
)

library(
  survival
)

library(
  broom
)

#=========================================================
# Load survival dataset
#=========================================================

survival_data <- readRDS(
  "survival_dataset.rds"
)

#=========================================================
# Create transformed predictors
#=========================================================

survival_data <- survival_data %>%
  
  mutate(
    
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
# Scale predictors
#=========================================================

survival_data <- survival_data %>%
  
  mutate(
    
    # MCV1:
    # 1 unit = 10 percentage-point increase
    
    mcv1_10pct =
      mcv1 / 0.10,
    
    
    # Births:
    # 1 unit = 1,000 births
    
    births_1000 =
      births / 1000,
    
    
    # Campaign duration:
    # 1 unit = 1 day
    
    campaign_duration_model =
      campaign_duration_days
    
  )

#=========================================================
# Define candidate predictors
#=========================================================

candidate_predictors <- c(
  
  # Vaccination
  
  "mcv1",
  
  
  # Demography
  
  "births",
  
  
  # SIA characteristics
  
  "campaign_duration_days",
  
  "age_target",
  
  
  # Geographic context
  
  "region_type",
  
  
  # Historical transmission
  
  "previous_outbreaks_12m",
  
  "log_previous_cases",
  
  
  # Temporal context
  
  "calendar_period"
  
)

#=========================================================
# Function for univariable Cox regression
#=========================================================

run_univariable_cox <- function(
    
  variable,
  
  data
  
) {
  
  #---------------------------------------
  # Create formula
  #---------------------------------------
  
  cox_formula <-
    
    as.formula(
      
      paste(
        
        "Surv(time, event) ~",
        
        variable
        
      )
      
    )
  
  
  #---------------------------------------
  # Fit model
  #---------------------------------------
  
  model <-
    
    coxph(
      
      cox_formula,
      
      data =
        data
      
    )
  
  
  #---------------------------------------
  # Extract results
  #---------------------------------------
  
  results <-
    
    tidy(
      
      model,
      
      exponentiate =
        TRUE,
      
      conf.int =
        TRUE
      
    ) %>%
    
    mutate(
      
      predictor =
        variable
      
    )
  
  
  return(
    
    list(
      
      model =
        model,
      
      results =
        results
      
    )
    
  )
  
}


#=========================================================
# Run univariable Cox models
#=========================================================

univariable_results <-
  
  map(
    
    candidate_predictors,
    
    ~run_univariable_cox(
      
      variable =
        .x,
      
      data =
        survival_data
      
    )
    
  )


#=========================================================
# Extract model results
#=========================================================

univariable_cox_table <-
  
  map_dfr(
    
    univariable_results,
    
    "results"
    
  ) %>%
  
  select(
    
    predictor,
    
    term,
    
    estimate,
    
    conf.low,
    
    conf.high,
    
    p.value
    
  ) %>%
  
  rename(
    
    hazard_ratio =
      estimate,
    
    lower_95_ci =
      conf.low,
    
    upper_95_ci =
      conf.high,
    
    p_value =
      p.value
    
  ) %>%
  
  arrange(
    
    p_value
    
  )

#=========================================================
# Print results
#=========================================================

cat(
  "\n========================================\n"
)

cat(
  "UNIVARIABLE COX REGRESSION\n"
)

cat(
  "========================================\n"
)

print(
  
  univariable_cox_table,
  
  n = Inf
  
)

#=========================================================
# Round results for presentation
#=========================================================

univariable_cox_table_rounded <-
  
  univariable_cox_table %>%
  
  mutate(
    
    across(
      
      c(
        
        hazard_ratio,
        
        lower_95_ci,
        
        upper_95_ci
        
      ),
      
      ~round(
        .x,
        3
      )
      
    ),
    
    
    p_value =
      
      round(
        p_value,
        4
      )
    
  )

cat(
  "\n========================================\n"
)

cat(
  "UNIVARIABLE COX RESULTS (ROUNDED)\n"
)

cat(
  "========================================\n"
)

print(
  
  univariable_cox_table_rounded,
  
  n = Inf
  
)

#=========================================================
# Save results
#=========================================================

write_csv(
  
  univariable_cox_table,
  
  "univariable_cox_results.csv"
  
)

write_csv(
  
  univariable_cox_table_rounded,
  
  "univariable_cox_results_rounded.csv"
  
)

#=========================================================
# Save individual models
#=========================================================

names(
  univariable_results
) <-
  
  candidate_predictors

saveRDS(
  
  univariable_results,
  
  "univariable_cox_models.rds"
  
)

#=========================================================
# Save updated survival dataset
#=========================================================

saveRDS(
  
  survival_data,
  
  "survival_dataset.rds"
  
)

write_csv(
  
  survival_data,
  
  "survival_dataset.csv"
  
)

cat(
  "\n========================================\n"
)

cat(
  "UNIVARIABLE COX ANALYSIS COMPLETE\n"
)

cat(
  "========================================\n"
)



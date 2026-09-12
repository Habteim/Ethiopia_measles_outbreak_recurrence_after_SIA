#=========================================================
# 08_exploratory_analysis.R
#
# Exploratory analysis of the post-SIA recurrence dataset
#
# Primary analysis:
# Time from SIA completion to:
#   1. First qualifying outbreak
#   2. Next SIA in the same region
#   3. End of surveillance
#
# One row = one region-SIA observation
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
    -age_group.x,
    -age_target.x,
    -doses.x
  ) %>%
  
  rename(
    age_group = age_group.y,
    age_target = age_target.y,
    doses = doses.y
  )

#=========================================================
# Basic dataset structure
#=========================================================

cat("\n========================================\n")
cat("EXPLORATORY ANALYSIS\n")
cat("========================================\n")

cat("\nNumber of observations:\n")

print(
  nrow(analytic_data)
)

cat("\nNumber of variables:\n")

print(
  ncol(analytic_data)
)

cat("\nDataset structure:\n")

glimpse(analytic_data)

#=========================================================
# Confirm primary survival outcome
#=========================================================

cat("\n========================================\n")
cat("PRIMARY SURVIVAL OUTCOME\n")
cat("========================================\n")

cat("\nEvents:\n")

print(
  table(
    analytic_data$outbreak_after_sia
  )
)

cat("\nCensoring mechanisms:\n")

print(
  table(
    analytic_data$censoring_reason
  )
)

cat("\nFollow-up time (days):\n")

print(
  summary(
    analytic_data$time_to_event_or_censor_days
  )
)

cat("\nFollow-up time (months):\n")

print(
  summary(
    analytic_data$time_to_event_or_censor_days /
      30.44
  )
)

cat("\nFollow-up time (years):\n")

print(
  summary(
    analytic_data$time_to_event_or_censor_days /
      365.25
  )
)

#=========================================================
# Time to outbreak among observed events
#
# Descriptive only.
# These values should NOT be interpreted as survival
# estimates because censoring is present.
#=========================================================

cat("\n========================================\n")
cat("TIME TO OUTBREAK AMONG EVENTS\n")
cat("========================================\n")

event_times <- analytic_data %>%
  
  filter(
    outbreak_after_sia == 1
  ) %>%
  
  pull(
    time_to_next_outbreak_days
  )

cat("\nTime to outbreak (days):\n")

print(
  summary(
    event_times
  )
)

cat("\nTime to outbreak (months):\n")

print(
  summary(
    event_times / 30.44
  )
)

cat("\nTime to outbreak (years):\n")

print(
  summary(
    event_times / 365.25
  )
)

#=========================================================
# Missingness assessment
#=========================================================

cat("\n========================================\n")
cat("MISSINGNESS ASSESSMENT\n")
cat("========================================\n")

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
    
    names_to = "variable",
    
    values_to = "missing_pct"
    
  ) %>%
  
  arrange(
    desc(missing_pct)
  )

print(
  missingness,
  n = Inf
)

#=========================================================
# Remove duplicated variables if present
#
# This section protects against duplicated variables created
# during joins.
#=========================================================

cat("\n========================================\n")
cat("VARIABLE CHECK\n")
cat("========================================\n")

cat("\nVariable names:\n")

print(
  names(analytic_data)
)

#=========================================================
# Calendar period
#
# Important because surveillance, routine vaccination,
# campaign implementation, and administrative structures
# may have changed over time.
#=========================================================

cat("\n========================================\n")
cat("CALENDAR PERIOD\n")
cat("========================================\n")

if(
  "calendar_period" %in%
  names(analytic_data)
) {
  
  print(
    
    analytic_data %>%
      
      count(
        calendar_period,
        outbreak_after_sia
      )
    
  )
  
}

cat("\nNumber of observations by calendar period:\n")

analytic_data %>%
  
  count(
    calendar_period
  ) %>%
  
  print()

#=========================================================
# Region characteristics
#=========================================================

cat("\n========================================\n")
cat("REGION CHARACTERISTICS\n")
cat("========================================\n")

cat("\nRegion type:\n")

print(
  analytic_data %>%
    
    count(
      region_type
    )
)

cat("\nRegion type by event status:\n")

print(
  
  analytic_data %>%
    
    count(
      region_type,
      outbreak_after_sia
    )
  
)

cat("\nIndividual regions:\n")

print(
  
  analytic_data %>%
    
    count(
      admin1
    )
  
)

cat("\nIndividual regions by event status:\n")

print(
  
  analytic_data %>%
    
    count(
      admin1,
      outbreak_after_sia
    )
  
)

#=========================================================
# Vaccination variables
#=========================================================

cat("\n========================================\n")
cat("ROUTINE IMMUNIZATION VARIABLES\n")
cat("========================================\n")

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

#---------------------------------------------------------
# Describe MCV1 as percentage for easier interpretation
#---------------------------------------------------------

cat("\nMCV1 coverage (%):\n")

print(
  summary(
    analytic_data$mcv1 * 100
  )
)

cat("\nMCV2 coverage (%):\n")

print(
  summary(
    analytic_data$mcv2 * 100
  )
)

#=========================================================
# Birth cohort
#=========================================================

cat("\n========================================\n")
cat("ESTIMATED BIRTH COHORT\n")
cat("========================================\n")

cat("\nBirths summary:\n")

print(
  summary(
    analytic_data$births
  )
)

#---------------------------------------------------------
# Births scaled per 1,000 births
#
# This is purely a numerical rescaling for regression
# interpretation.
#
# It does NOT mean births per 1,000 population.
#---------------------------------------------------------

analytic_data <- analytic_data %>%
  
  mutate(
    
    births_1000 =
      births / 1000
    
  )

cat("\nBirths scaled per 1,000 births:\n")

print(
  summary(
    analytic_data$births_1000
  )
)

#=========================================================
# SIA characteristics
#=========================================================

cat("\n========================================\n")
cat("SIA CHARACTERISTICS\n")
cat("========================================\n")

cat("\nSIA year:\n")

print(
  summary(
    analytic_data$sia_year
  )
)

cat("\nCampaign duration (days):\n")

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
  analytic_data %>%
    
    count(
      age_target,
      sort = TRUE
    )
)

cat("\nAge group:\n")

print(
  analytic_data %>%
    
    count(
      age_group,
      sort = TRUE
    )
)

#=========================================================
# Scale doses for regression
#
# Scaling does not change the model fit.
# It improves coefficient interpretation.
#
# We first inspect the magnitude of doses before deciding
# on the most useful scale.
#=========================================================

cat("\n========================================\n")
cat("DOSE SCALING\n")
cat("========================================\n")

print(
  summary(
    analytic_data$doses
  )
)

#---------------------------------------------------------
# Doses per 100,000
#
# Change this later if the distribution suggests another
# scale is more interpretable.
#---------------------------------------------------------

analytic_data <- analytic_data %>%
  
  mutate(
    
    doses_100000 =
      doses / 100000
    
  )

cat("\nDoses per 100,000:\n")

print(
  summary(
    analytic_data$doses_100000
  )
)

#=========================================================
# Historical outbreak burden
#=========================================================

cat("\n========================================\n")
cat("HISTORICAL OUTBREAK BURDEN\n")
cat("========================================\n")

cat("\nPrevious outbreaks within 12 months:\n")

print(
  summary(
    analytic_data$previous_outbreaks_12m
  )
)

cat("\nDistribution of previous outbreaks:\n")

print(
  
  analytic_data %>%
    
    count(
      previous_outbreaks_12m,
      sort = TRUE
    )
  
)

cat("\nPrevious cases within 12 months:\n")

print(
  summary(
    analytic_data$previous_cases_12m
  )
)

cat("\nPrevious outbreak size:\n")

print(
  summary(
    analytic_data$previous_outbreak_size
  )
)

cat("\nPrevious outbreak duration:\n")

print(
  summary(
    analytic_data$previous_outbreak_duration
  )
)

#=========================================================
# Previous outbreak history
#
# Create a simple epidemiologically interpretable variable:
#
# None = no outbreak in the preceding 12 months
# >=1  = one or more outbreaks in the preceding 12 months
#
# This may be preferable to treating a sparse count variable
# as linear in the primary Cox model.
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    prev_outbreak_group =
      
      case_when(
        
        previous_outbreaks_12m == 0 ~
          "None",
        
        previous_outbreaks_12m >= 1 ~
          ">=1",
        
        TRUE ~ NA_character_
        
      ),
    
    prev_outbreak_group =
      
      factor(
        
        prev_outbreak_group,
        
        levels =
          c(
            "None",
            ">=1"
          )
        
      )
    
  )

cat("\nPrevious outbreak group:\n")

print(
  table(
    analytic_data$prev_outbreak_group
  )
)

cat("\nPrevious outbreak group by event:\n")

print(
  table(
    analytic_data$prev_outbreak_group,
    analytic_data$outbreak_after_sia
  )
)

#=========================================================
# Descriptive comparison by event status
#
# This is descriptive only and should not be interpreted
# as an adjusted association.
#=========================================================

cat("\n========================================\n")
cat("DESCRIPTIVE COMPARISON BY EVENT STATUS\n")
cat("========================================\n")

descriptive_numeric <- analytic_data %>%
  
  group_by(
    outbreak_after_sia
  ) %>%
  
  summarise(
    
    n = n(),
    
    median_followup =
      median(
        time_to_event_or_censor_days,
        na.rm = TRUE
      ),
    
    median_mcv1 =
      median(
        mcv1,
        na.rm = TRUE
      ),
    
    median_births =
      median(
        births,
        na.rm = TRUE
      ),
    
    median_doses =
      median(
        doses,
        na.rm = TRUE
      ),
    
    median_campaign_duration =
      median(
        campaign_duration_days,
        na.rm = TRUE
      ),
    
    median_previous_outbreaks =
      median(
        previous_outbreaks_12m,
        na.rm = TRUE
      ),
    
    median_previous_cases =
      median(
        previous_cases_12m,
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  )

print(
  descriptive_numeric
)

#=========================================================
# Correlation assessment
#
# Only continuous predictors.
#
# Do NOT include the survival outcome in predictor
# correlation assessment.
#=========================================================

cat("\n========================================\n")
cat("PREDICTOR CORRELATION ASSESSMENT\n")
cat("========================================\n")

numeric_predictors <- analytic_data %>%
  
  select(
    
    mcv1,
    
    births_1000,
    
    doses_100000,
    
    campaign_duration_days,
    
    previous_outbreaks_12m,
    
    previous_cases_12m,
    
    previous_outbreak_size,
    
    previous_outbreak_duration
    
  )

cor_matrix <- cor(
  
  numeric_predictors,
  
  use =
    "pairwise.complete.obs"
  
)

cat("\nCorrelation matrix:\n")

print(
  
  round(
    cor_matrix,
    2
  )
  
)

#=========================================================
# Highly correlated predictor pairs
#=========================================================

cor_df <- as.data.frame(
  
  as.table(
    cor_matrix
  )
  
)

high_corr <- cor_df %>%
  
  filter(
    
    Var1 != Var2,
    
    abs(Freq) >= 0.70
    
  ) %>%
  
  mutate(
    
    pair =
      paste(
        pmin(
          as.character(Var1),
          as.character(Var2)
        ),
        
        pmax(
          as.character(Var1),
          as.character(Var2)
        ),
        
        sep = " -- "
      )
    
  ) %>%
  
  distinct(
    pair,
    .keep_all = TRUE
  ) %>%
  
  arrange(
    desc(
      abs(Freq)
    )
  )

cat("\nHighly correlated predictor pairs:\n")

print(
  high_corr
)

#=========================================================
# Relationship between censoring and predictors
#
# Useful because censoring by subsequent SIA is part of the
# primary analysis.
#=========================================================

cat("\n========================================\n")
cat("CENSORING ASSESSMENT\n")
cat("========================================\n")

cat("\nCensoring reason by region type:\n")

print(
  
  analytic_data %>%
    
    count(
      region_type,
      censoring_reason
    )
  
)

cat("\nCensoring reason by calendar period:\n")

print(
  
  analytic_data %>%
    
    count(
      calendar_period,
      censoring_reason
    )
  
)

#=========================================================
# Potential outliers
#
# Examine extreme values before deciding whether
# transformation is needed.
#=========================================================

cat("\n========================================\n")
cat("EXTREME VALUES\n")
cat("========================================\n")

cat("\nLargest birth cohorts:\n")

analytic_data %>%
  
  arrange(
    desc(births)
  ) %>%
  
  select(
    admin1,
    sia_year,
    births
  ) %>%
  
  head(10) %>%
  
  print()

cat("\nLargest SIA doses:\n")

analytic_data %>%
  
  arrange(
    desc(doses)
  ) %>%
  
  select(
    admin1,
    sia_year,
    doses
  ) %>%
  
  head(10) %>%
  
  print()

cat("\nLargest previous case burden:\n")

analytic_data %>%
  
  arrange(
    desc(previous_cases_12m)
  ) %>%
  
  select(
    admin1,
    sia_year,
    previous_cases_12m
  ) %>%
  
  head(10) %>%
  
  print()

cat("\nLargest previous outbreak size:\n")

analytic_data %>%
  
  arrange(
    desc(previous_outbreak_size)
  ) %>%
  
  select(
    admin1,
    sia_year,
    previous_outbreak_size
  ) %>%
  
  head(10) %>%
  
  print()

#=========================================================
# Examine skewness using quantiles
#
# This will help decide whether log transformations are
# appropriate for secondary analyses.
#=========================================================

cat("\n========================================\n")
cat("PREDICTOR QUANTILES\n")
cat("========================================\n")

quantile_vars <- c(
  
  "births",
  
  "doses",
  
  "previous_cases_12m",
  
  "previous_outbreak_size",
  
  "previous_outbreak_duration"
  
)

for(
  var in quantile_vars
) {
  
  cat(
    
    "\n",
    var,
    ":\n",
    
    sep = ""
    
  )
  
  print(
    
    quantile(
      
      analytic_data[[var]],
      
      probs =
        c(
          0,
          0.25,
          0.50,
          0.75,
          0.90,
          0.95,
          0.99,
          1
        ),
      
      na.rm = TRUE
      
    )
    
  )
  
}

#=========================================================
# Save updated analytic dataset
#=========================================================

write_csv(
  
  analytic_data,
  
  "analytic_dataset.csv"
  
)

saveRDS(
  
  analytic_data,
  
  "analytic_dataset.rds"
  
)

#=========================================================
# Save exploratory outputs
#=========================================================

write_csv(
  
  missingness,
  
  "exploratory_missingness.csv"
  
)

write_csv(
  
  high_corr,
  
  "exploratory_high_correlations.csv"
  
)

#=========================================================
# End
#=========================================================

cat("\n========================================\n")
cat("EXPLORATORY ANALYSIS COMPLETE\n")
cat("========================================\n")


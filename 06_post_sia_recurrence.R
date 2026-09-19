#=========================================================

# 06_post_sia_recurrence.R

# Link SIAs to subsequent measles outbreaks

#

# Primary analysis:

# Each region-SIA observation is followed until the earliest of:

# 1. First qualifying outbreak after the SIA washout period

# 2. Next SIA in the same region

# 3. End of surveillance

#

# A subsequent SIA therefore terminates the risk interval,

# preventing the same outbreak from being attributed to

# multiple preceding SIAs.

#=========================================================

library(tidyverse)
library(lubridate)

#=========================================================

# Parameters

#=========================================================

POST_SIA_WASHOUT_DAYS <- 42

SURVEILLANCE_END_DATE <- as.Date("2025-05-12")


#=========================================================

# Keep needed SIA variables

#=========================================================

sia_analysis <- sia %>%
  
  select(
    admin1,
    start_date,
    end_date,
    sia_year,
    age_group,
    age_target,
    doses
  ) %>%
  
  arrange(
    admin1,
    start_date
  )

#=========================================================

# Identify the next SIA in the same region

#

# The next SIA start date marks the end of the risk interval

# for the current SIA.

#=========================================================

sia_analysis <- sia_analysis %>%
  
  group_by(admin1) %>%
  
  arrange(
    start_date,
    .by_group = TRUE
  ) %>%
  
  mutate(
    
    next_sia_start_date = lead(start_date),
    
    next_sia_end_date = lead(end_date)
    
  ) %>%
  
  ungroup()

#=========================================================

# Diagnostics:

# Examine SIA intervals

#=========================================================

cat("\nNumber of region-SIA observations:\n")

nrow(sia_analysis)

cat("\nSIAs by region:\n")

sia_analysis %>%
  count(admin1) %>%
  print(n = Inf)

#=========================================================

# Link SIAs to outbreaks in the same region

#=========================================================

post_sia <- sia_analysis %>%
  
  left_join(
    outbreaks,
    by = "admin1"
  )

#=========================================================

# Keep outbreaks occurring after the post-SIA washout period

#=========================================================

post_sia_future <- post_sia %>%
  
  filter(
    
    outbreak_start >
      (end_date + days(POST_SIA_WASHOUT_DAYS))
    
  )

#=========================================================

# Restrict outbreaks to those occurring before the next SIA

#

# If there is no subsequent SIA, outbreaks may occur up to

# the end of surveillance.

#=========================================================

post_sia_eligible <- post_sia_future %>%
  
  filter(
    
    is.na(next_sia_start_date) |
      
      outbreak_start < next_sia_start_date
    
  )

#=========================================================

# Identify the first qualifying outbreak within each

# SIA-specific risk interval

#=========================================================

next_outbreak <- post_sia_eligible %>%
  
  group_by(
    admin1,
    start_date,
    end_date
  ) %>%
  
  slice_min(
    outbreak_start,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup() %>%
  
  select(
    admin1,
    start_date,
    end_date,
    
    outbreak_start,
    outbreak_end,
    total_cases,
    duration_weeks
    
  )

#=========================================================

# Merge outbreak information back to all SIAs

#=========================================================

sia_outcomes <- sia_analysis %>%
  
  left_join(
    
    
    next_outbreak,
    
    by = c(
      "admin1",
      "start_date",
      "end_date"
    )
    
    
  )

#=========================================================

# Determine the end of follow-up

#

# Follow-up ends at the earliest of:

#

# 1. qualifying outbreak

# 2. next SIA

# 3. end of surveillance

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    
    #-----------------------------------------------------
    # Administrative censoring date
    #-----------------------------------------------------
    
    surveillance_end_date =
      SURVEILLANCE_END_DATE,
    
    
    #-----------------------------------------------------
    # End of follow-up
    #-----------------------------------------------------
    
    followup_end_date =
      pmin(
        
        outbreak_start,
        next_sia_start_date,
        surveillance_end_date,
        
        na.rm = TRUE
        
      )
    
  )

#=========================================================

# Correct pmin() behaviour for observations where

# all optional dates are missing

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    followup_end_date =
      as.Date(followup_end_date)
    
  )

#=========================================================

# Define event indicator

#

# Event = 1 only if the qualifying outbreak occurs before:

# - the next SIA

# - the end of surveillance

#

# Otherwise:

# Event = 0

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    outbreak_after_sia =
      
      if_else(
        
        !is.na(outbreak_start) &
          
          outbreak_start <= surveillance_end_date &
          
          (
            is.na(next_sia_start_date) |
              outbreak_start < next_sia_start_date
          ),
        
        1,
        
        0
        
      )
    
  )

#=========================================================

# Calculate follow-up time

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    time_to_event_or_censor_days =
      
      as.numeric(
        
        followup_end_date -
          end_date
        
      ),
    
    
    time_to_event_or_censor_weeks =
      
      time_to_event_or_censor_days / 7
    
  )

#=========================================================

# Calculate time to outbreak

#

# This is only defined for observations with an event.

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    time_to_next_outbreak_days =
      
      if_else(
        
        outbreak_after_sia == 1,
        
        as.numeric(
          
          outbreak_start -
            end_date
          
        ),
        
        NA_real_
        
      ),
    
    
    time_to_next_outbreak_weeks =
      
      time_to_next_outbreak_days / 7
    
  )

#=========================================================

# Identify censoring mechanism

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    censoring_reason =
      
      case_when(
        
        outbreak_after_sia == 1 ~
          "Event: subsequent outbreak",
        
        
        !is.na(next_sia_start_date) &
          
          next_sia_start_date <= surveillance_end_date ~
          
          "Censored: next SIA",
        
        
        TRUE ~
          "Censored: end of surveillance"
        
      )
    
  )

#=========================================================

# Rename outbreak characteristics

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  rename(
    
    next_outbreak_size =
      total_cases,
    
    next_outbreak_duration =
      duration_weeks
    
  )

#=========================================================

# Fixed follow-up windows

#

# IMPORTANT:

#

# These crude variables are retained for descriptive

# purposes only.

#

# The primary recurrence estimates at 6, 12 and 24 months

# should later be calculated using Kaplan-Meier methods.

#=========================================================

sia_outcomes <- sia_outcomes %>%
  
  mutate(
    
    outbreak_within_6m =
      
      if_else(
        
        outbreak_after_sia == 1 &
          
          time_to_next_outbreak_days <= 183,
        
        1,
        
        0
        
      ),
    
    
    outbreak_within_12m =
      
      if_else(
        
        outbreak_after_sia == 1 &
          
          time_to_next_outbreak_days <= 365,
        
        1,
        
        0
        
      ),
    
    
    outbreak_within_24m =
      
      if_else(
        
        outbreak_after_sia == 1 &
          
          time_to_next_outbreak_days <= 730,
        
        1,
        
        0
        
      )
    
  )

#=========================================================

# Diagnostics

#=========================================================

cat("\n========================================\n")
cat("POST-SIA RECURRENCE ANALYSIS\n")
cat("========================================\n")

#---------------------------------------------------------

# Number of SIAs

#---------------------------------------------------------

cat("\nNumber of region-SIA observations:\n")

print(
  nrow(sia_outcomes)
)

#---------------------------------------------------------

# Events

#---------------------------------------------------------

cat("\nOutbreak events:\n")

print(
  table(
    sia_outcomes$outbreak_after_sia
  )
)

#---------------------------------------------------------

# Censoring mechanisms

#---------------------------------------------------------

cat("\nCensoring mechanisms:\n")

print(
  table(
    sia_outcomes$censoring_reason
  )
)

#---------------------------------------------------------

# Number censored by next SIA

#---------------------------------------------------------

cat("\nNumber censored by subsequent SIA:\n")

print(
  sum(
    sia_outcomes$censoring_reason ==
      "Censored: next SIA"
  )
)

#---------------------------------------------------------

# Follow-up time

#---------------------------------------------------------

cat("\nFollow-up time summary (days):\n")

print(
  
  summary(
    
    sia_outcomes$time_to_event_or_censor_days
    
  )
  
)

#---------------------------------------------------------

# Time to outbreak

#---------------------------------------------------------

cat("\nTime to outbreak summary among events:\n")

print(
  
  summary(
    
    sia_outcomes$time_to_next_outbreak_days
    
  )
  
)

#=========================================================

# Check for invalid negative or zero follow-up times

#=========================================================

cat("\nObservations with zero or negative follow-up time:\n")

sia_outcomes %>%
  
  filter(
    
    time_to_event_or_censor_days <= 0
    
  ) %>%
  
  select(
    
    admin1,
    sia_year,
    start_date,
    end_date,
    next_sia_start_date,
    outbreak_start,
    followup_end_date,
    time_to_event_or_censor_days,
    outbreak_after_sia
    
  ) %>%
  
  print(n = Inf)

#=========================================================

# Verify that each outbreak is assigned to only one SIA

#=========================================================

outbreak_assignment_check <- sia_outcomes %>%
  
  filter(
    outbreak_after_sia == 1
  ) %>%
  
  count(
    
    admin1,
    outbreak_start
    
  ) %>%
  
  filter(
    n > 1
  )

cat("\nOutbreaks assigned to more than one SIA:\n")

print(
  outbreak_assignment_check
)

#=========================================================

# Examine earliest recurrence events

#=========================================================

cat("\nEarliest outbreak recurrences:\n")

sia_outcomes %>%
  
  filter(
    outbreak_after_sia == 1
  ) %>%
  
  arrange(
    time_to_next_outbreak_days
  ) %>%
  
  select(
    
    admin1,
    sia_year,
    end_date,
    outbreak_start,
    time_to_next_outbreak_days
    
  ) %>%
  
  head(20) %>%
  
  print()


#Additional analyses
#Examine the 26 observations censored by a subsequent SIA

sia_outcomes %>%
  
  filter(
    censoring_reason == "Censored: next SIA"
  ) %>%
  
  select(
    admin1,
    sia_year,
    end_date,
    next_sia_start_date,
    followup_end_date,
    time_to_event_or_censor_days
  ) %>%
  
  arrange(
    admin1,
    end_date
  ) %>%
  
  print(n = Inf)

#Summarize by region 
sia_outcomes %>%
  
  filter(
    censoring_reason == "Censored: next SIA"
  ) %>%
  
  count(
    admin1,
    sort = TRUE
  )


#Examine events and censoring by calendar year

sia_outcomes %>%
  
  mutate(
    calendar_period = case_when(
      
      sia_year <= 2010 ~ "2005–2010",
      
      sia_year <= 2015 ~ "2011–2015",
      
      sia_year <= 2020 ~ "2016–2020",
      
      TRUE ~ "2021–2025"
      
    )
  ) %>%
  
  count(
    calendar_period,
    outbreak_after_sia
  )


#Examine number of observations per period 
sia_outcomes %>%
  
  mutate(
    calendar_period = case_when(
      
      sia_year <= 2010 ~ "2005–2010",
      
      sia_year <= 2015 ~ "2011–2015",
      
      sia_year <= 2020 ~ "2016–2020",
      
      TRUE ~ "2021–2025"
      
    )
  ) %>%
  
  count(
    calendar_period
  )


#=========================================================

# Save

#=========================================================

write_csv(
  
  sia_outcomes,
  
  "sia_outcomes.csv"
  
)

saveRDS(
  
  sia_outcomes,
  
  "sia_outcomes.rds"
  
)










#=========================================================
# SENSITIVITY ANALYSIS:
# ALTERNATIVE POST-SIA WASHOUT PERIODS
#
# Compare:
# 28 days = 4 weeks
# 42 days = 6 weeks (primary analysis)
# 56 days = 8 weeks
#=========================================================

calculate_post_sia_outcomes <- function(washout_days) {
  
  temp_post_sia <- sia_analysis %>%
    left_join(
      outbreaks,
      by = "admin1"
    )
  
  temp_future <- temp_post_sia %>%
    filter(
      outbreak_start >
        end_date + days(washout_days),
      
      # Outbreak must occur before the next SIA
      is.na(next_sia_start_date) |
        outbreak_start < next_sia_start_date,
      
      # Outbreak must occur within surveillance period
      outbreak_start <= SURVEILLANCE_END_DATE
    )
  
  temp_next_outbreak <- temp_future %>%
    group_by(
      admin1,
      start_date,
      end_date
    ) %>%
    slice_min(
      outbreak_start,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    mutate(
      time_to_next_outbreak_days =
        as.numeric(outbreak_start - end_date),
      
      outbreak_after_sia = 1
    )
  
  temp_outcomes <- sia_analysis %>%
    left_join(
      temp_next_outbreak %>%
        select(
          admin1,
          start_date,
          end_date,
          outbreak_start,
          time_to_next_outbreak_days,
          outbreak_after_sia
        ),
      by = c(
        "admin1",
        "start_date",
        "end_date"
      )
    ) %>%
    mutate(
      outbreak_after_sia =
        replace_na(
          outbreak_after_sia,
          0
        )
    )
  
  return(temp_outcomes)
}


washout_28_days <- calculate_post_sia_outcomes(28)

washout_42_days <- calculate_post_sia_outcomes(42)

washout_56_days <- calculate_post_sia_outcomes(56)

c(
  washout_28 = sum(washout_28_days$outbreak_after_sia, na.rm = TRUE),
  washout_42 = sum(washout_42_days$outbreak_after_sia, na.rm = TRUE),
  washout_56 = sum(washout_56_days$outbreak_after_sia, na.rm = TRUE)
)


table(washout_42_days$outbreak_after_sia)


median(
  washout_42_days$time_to_next_outbreak_days[
    washout_42_days$outbreak_after_sia == 1
  ],
  na.rm = TRUE
)


median_times <- c(
  washout_28 = median(
    washout_28_days$time_to_next_outbreak_days[
      washout_28_days$outbreak_after_sia == 1
    ],
    na.rm = TRUE
  ),
  
  washout_42 = median(
    washout_42_days$time_to_next_outbreak_days[
      washout_42_days$outbreak_after_sia == 1
    ],
    na.rm = TRUE
  ),
  
  washout_56 = median(
    washout_56_days$time_to_next_outbreak_days[
      washout_56_days$outbreak_after_sia == 1
    ],
    na.rm = TRUE
  )
)

median_times

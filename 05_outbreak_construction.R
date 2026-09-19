#=========================================================
# 05_outbreak_construction.R
# Construct measles outbreak episodes and recurrence intervals
#=========================================================

# Load libraries
library(tidyverse)
library(lubridate)
library(tidyr)
library(remotes)
library(ISOweek)

#=========================================================
# User-defined parameters
#=========================================================

OUTBREAK_THRESHOLD <- 5
WASHOUT_WEEKS <- 2

#=========================================================
# Confirmed measles only
#=========================================================

confirmed_cases <- measles_data %>%
  filter(confirmed_measles == 1)

#=========================================================
# Weekly case counts by region
#=========================================================

weekly_cases <- confirmed_cases %>%
  mutate(
    week_start =
      floor_date(
        date_onset,
        unit = "week",
        week_start = 1
      )
  ) %>%
  count(
    admin1,
    week_start,
    name = "cases"
  )

#=========================================================
# Create complete weekly time series
#=========================================================

weekly_cases <- weekly_cases %>%
  group_by(admin1) %>%
  complete(
    week_start =
      seq(
        min(week_start),
        max(week_start),
        by = "week"
      ),
    fill = list(cases = 0)
  ) %>%
  ungroup()

#=========================================================
# Define outbreak weeks
#=========================================================

weekly_cases <- weekly_cases %>%
  mutate(
    outbreak_week =
      cases >= OUTBREAK_THRESHOLD
  )

#=========================================================
# Identify outbreak starts
# Outbreak starts when:
# current week >= threshold
# previous two weeks < threshold
#=========================================================

weekly_cases <- weekly_cases %>%
  arrange(admin1, week_start) %>%
  group_by(admin1) %>%
  mutate(
    
    below_threshold =
      cases < OUTBREAK_THRESHOLD,
    
    two_below_before =
      lag(below_threshold, 1, default = TRUE) &
      lag(below_threshold, 2, default = TRUE),
    
    outbreak_start =
      outbreak_week &
      two_below_before,
    
    outbreak_id =
      cumsum(outbreak_start)
    
  ) %>%
  ungroup()

#=========================================================
# Keep outbreak weeks only
#=========================================================

outbreak_weeks <- weekly_cases %>%
  filter(outbreak_week)

#=========================================================
# Summarize outbreaks
#=========================================================

outbreaks <- outbreak_weeks %>%
  group_by(admin1, outbreak_id) %>%
  summarise(
    
    outbreak_start =
      min(week_start),
    
    outbreak_end =
      max(week_start),
    
    duration_weeks =
      n(),
    
    total_cases =
      sum(cases),
    
    peak_cases =
      max(cases),
    
    mean_weekly_cases =
      mean(cases),
    
    .groups = "drop"
    
  )

#=========================================================
# Remove very small outbreaks
#=========================================================

outbreaks <- outbreaks %>%
  filter(total_cases >= 5)

#=========================================================
# Add outbreak year
#=========================================================

outbreaks <- outbreaks %>%
  mutate(
    outbreak_year =
      year(outbreak_start)
  )

#=========================================================
# Calculate recurrence intervals
#=========================================================

outbreaks <- outbreaks %>%
  arrange(admin1, outbreak_start) %>%
  group_by(admin1) %>%
  mutate(
    
    next_outbreak_start =
      lead(outbreak_start),
    
    recurrence_days =
      as.numeric(
        next_outbreak_start -
          outbreak_end
      )
    
  ) %>%
  ungroup()

#=========================================================
# Diagnostics
#=========================================================

cat("\n")
cat("====================================\n")
cat("OUTBREAK DIAGNOSTICS\n")
cat("====================================\n")

cat("\nNumber of outbreaks:\n")
print(nrow(outbreaks))

cat("\nOutbreaks by region:\n")
print(
  outbreaks %>%
    count(admin1)
)

cat("\nOutbreaks by year:\n")
print(
  outbreaks %>%
    count(outbreak_year)
)

cat("\nDuration summary:\n")
print(
  summary(outbreaks$duration_weeks)
)

cat("\nSize summary:\n")
print(
  summary(outbreaks$total_cases)
)

cat("\nRecurrence summary:\n")
print(
  summary(outbreaks$recurrence_days)
)

#=========================================================
# Longest outbreaks
#=========================================================

cat("\nTop 10 longest outbreaks:\n")

print(
  outbreaks %>%
    arrange(desc(duration_weeks)) %>%
    select(
      admin1,
      outbreak_id,
      outbreak_start,
      outbreak_end,
      duration_weeks,
      total_cases
    ) %>%
    head(10)
)


#=========================================================
# Largest outbreaks
#=========================================================

cat("\nTop 10 largest outbreaks:\n")

print(
  outbreaks %>%
    arrange(desc(total_cases)) %>%
    select(
      admin1,
      outbreak_id,
      outbreak_start,
      outbreak_end,
      duration_weeks,
      total_cases
    ) %>%
    head(10)
)

#=========================================================
# Save outputs
#=========================================================

write_csv(
  outbreaks,
  "outbreak_dataset.csv"
)

saveRDS(
  outbreaks,
  "outbreak_dataset.rds"
)

write_csv(
  outbreak_weeks,
  "outbreak_weeks.csv"
)

saveRDS(
  outbreak_weeks,
  "outbreak_weeks.rds"
)


#Additional diagnostics 
#========================
weekly_cases %>%
  filter(
    admin1 == "Oromia",
    week_start >= as.Date("2012-12-31"),
    week_start <= as.Date("2014-10-13")
  ) %>%
  select(
    week_start,
    cases
  )

ggplot(
  weekly_cases %>%
    filter(
      admin1 == "Oromia",
      week_start >= as.Date("2012-12-31"),
      week_start <= as.Date("2014-10-13")
    ),
  aes(week_start, cases)
) +
  geom_line()


weekly_cases %>%
  filter(
    admin1 == "Oromia",
    week_start >= as.Date("2024-01-01")
  ) %>%
  ggplot(
    aes(week_start, cases)
  ) +
  geom_line()


oromia_cases <- weekly_cases %>%
  filter(
    admin1 == "Oromia",
    week_start >= as.Date("2024-07-01"),
    week_start <= as.Date("2024-12-31")
  ) %>%
  select(week_start, cases)



#=========================================================
# 05a_outbreak_definition_sensitivity.R
# Sensitivity analysis for outbreak construction
#=========================================================

library(tidyverse)
library(lubridate)

#=========================================================
# Confirmed measles cases
#=========================================================

confirmed_cases <- measles_data %>%
  
  filter(
    confirmed_measles == 1
  )


#=========================================================
# Weekly case counts
#=========================================================

weekly_cases_base <- confirmed_cases %>%
  
  mutate(
    
    week_start =
      floor_date(
        date_onset,
        unit = "week",
        week_start = 1
      )
    
  ) %>%
  
  count(
    
    admin1,
    week_start,
    
    name = "cases"
    
  ) %>%
  
  group_by(
    admin1
  ) %>%
  
  complete(
    
    week_start =
      seq(
        min(week_start),
        max(week_start),
        by = "week"
      ),
    
    fill =
      list(
        cases = 0
      )
    
  ) %>%
  
  ungroup()


#=========================================================
# Function to construct outbreak episodes
#=========================================================

construct_outbreaks <- function(
    
  weekly_data,
  outbreak_threshold = 5,
  separation_weeks = 2
  
) {
  
  
  #=======================================================
  # Define outbreak activity
  #=======================================================
  
  outbreak_data <-
    
    weekly_data %>%
    
    arrange(
      admin1,
      week_start
    ) %>%
    
    group_by(
      admin1
    ) %>%
    
    mutate(
      
      outbreak_week =
        cases >= outbreak_threshold,
      
      below_threshold =
        cases < outbreak_threshold
      
    )
  
  
  #=======================================================
  # Identify whether preceding weeks
  # were below the threshold
  #=======================================================
  
  for (
    
    i in seq_len(
      separation_weeks
    )
    
  ) {
    
    outbreak_data <-
      
      outbreak_data %>%
      
      mutate(
        
        !!paste0(
          "below_",
          i,
          "_week_before"
        ) :=
          
          lag(
            below_threshold,
            i,
            default = TRUE
          )
        
      )
    
  }
  
  
  #=======================================================
  # Identify outbreak starts
  #=======================================================
  
  separation_columns <-
    
    paste0(
      "below_",
      seq_len(
        separation_weeks
      ),
      "_week_before"
    )
  
  
  outbreak_data <-
    
    outbreak_data %>%
    
    rowwise() %>%
    
    mutate(
      
      sufficient_separation =
        
        all(
          c_across(
            all_of(
              separation_columns
            )
          )
        ),
      
      outbreak_start =
        
        outbreak_week &
        sufficient_separation
      
    ) %>%
    
    ungroup() %>%
    
    group_by(
      admin1
    ) %>%
    
    mutate(
      
      outbreak_id =
        cumsum(
          outbreak_start
        )
      
    ) %>%
    
    ungroup()
  
  
  #=======================================================
  # Retain outbreak activity weeks
  #=======================================================
  
  outbreak_weeks <-
    
    outbreak_data %>%
    
    filter(
      outbreak_week
    )
  
  
  #=======================================================
  # Summarize outbreak episodes
  #=======================================================
  
  outbreaks <-
    
    outbreak_weeks %>%
    
    group_by(
      admin1,
      outbreak_id
    ) %>%
    
    summarise(
      
      outbreak_start =
        min(
          week_start
        ),
      
      outbreak_end =
        max(
          week_start
        ),
      
      duration_weeks =
        n(),
      
      total_cases =
        sum(
          cases
        ),
      
      peak_cases =
        max(
          cases
        ),
      
      mean_weekly_cases =
        mean(
          cases
        ),
      
      .groups =
        "drop"
      
    ) %>%
    
    mutate(
      
      outbreak_year =
        year(
          outbreak_start
        ),
      
      outbreak_threshold =
        outbreak_threshold,
      
      separation_weeks =
        separation_weeks
      
    )
  
  
  return(
    
    list(
      
      outbreak_data =
        outbreak_data,
      
      outbreak_weeks =
        outbreak_weeks,
      
      outbreaks =
        outbreaks
      
    )
    
  )
  
}


#=========================================================
# Define sensitivity scenarios
#=========================================================

scenarios <-
  
  tibble(
    
    scenario =
      c(
        "Threshold_3",
        "Primary_threshold_5",
        "Threshold_10"
      ),
    
    outbreak_threshold =
      c(
        3,
        5,
        10
      ),
    
    separation_weeks =
      c(
        2,
        2,
        2
      )
    
  )


#=========================================================
# Run all scenarios
#=========================================================

sensitivity_results <-
  
  pmap(
    
    scenarios,
    
    function(
    
      scenario,
      outbreak_threshold,
      separation_weeks
      
    ) {
      
      
      result <-
        
        construct_outbreaks(
          
          weekly_data =
            weekly_cases_base,
          
          outbreak_threshold =
            outbreak_threshold,
          
          separation_weeks =
            separation_weeks
          
        )
      
      
      result$outbreaks <-
        
        result$outbreaks %>%
        
        mutate(
          
          scenario =
            scenario
          
        )
      
      
      return(
        result
      )
      
    }
    
  )


names(
  sensitivity_results
) <-
  
  scenarios$scenario


#=========================================================
# Combine outbreak episodes
#=========================================================

all_sensitivity_outbreaks <-
  
  map_dfr(
    
    sensitivity_results,
    
    "outbreaks"
    
  )


#=========================================================
# Summary by scenario
#=========================================================

scenario_summary <-
  
  all_sensitivity_outbreaks %>%
  
  group_by(
    scenario,
    outbreak_threshold,
    separation_weeks
  ) %>%
  
  summarise(
    
    number_outbreaks =
      n(),
    
    regions_with_outbreaks =
      n_distinct(
        admin1
      ),
    
    median_duration_weeks =
      median(
        duration_weeks,
        na.rm = TRUE
      ),
    
    median_total_cases =
      median(
        total_cases,
        na.rm = TRUE
      ),
    
    median_peak_cases =
      median(
        peak_cases,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  )


print(
  scenario_summary
)


#=========================================================
# Number of outbreaks by region
#=========================================================

scenario_by_region <-
  
  all_sensitivity_outbreaks %>%
  
  count(
    
    scenario,
    admin1,
    
    name =
      "number_outbreaks"
    
  )


print(
  scenario_by_region
)


#=========================================================
# Save outputs
#=========================================================

write_csv(
  
  scenario_summary,
  
  "outbreak_definition_sensitivity_summary.csv"
  
)


write_csv(
  
  all_sensitivity_outbreaks,
  
  "outbreak_definition_sensitivity_outbreaks.csv"
  
)


write_csv(
  
  scenario_by_region,
  
  "outbreak_definition_sensitivity_by_region.csv"
  
)


saveRDS(
  
  all_sensitivity_outbreaks,
  
  "outbreak_definition_sensitivity_outbreaks.rds"
  
)


cat(
  
  "\n========================================\n"
)

cat(
  
  "OUTBREAK DEFINITION SENSITIVITY ANALYSIS COMPLETE\n"
)

cat(
  
  "========================================\n"
  
)

#=========================================================
# 06_post_sia_recurrence.R
# Link SIAs to subsequent outbreaks
#=========================================================

library(tidyverse)
library(lubridate)

#=========================================================
# Parameters
#=========================================================

POST_SIA_WASHOUT_DAYS <- 42

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
  )

#=========================================================
# Link each SIA to future outbreaks
#=========================================================

post_sia <- sia_analysis %>%
  left_join(
    outbreaks,
    by = "admin1"
  )

#=========================================================
# Keep outbreaks occurring after washout
#=========================================================

post_sia_future <- post_sia %>%
  filter(
    outbreak_start >
      (end_date + days(POST_SIA_WASHOUT_DAYS))
  )

#=========================================================
# Nearest outbreak after each SIA
#=========================================================

next_outbreak <- post_sia_future %>%
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
  ungroup()

#=========================================================
# Time-to-event outcomes
#=========================================================

next_outbreak <- next_outbreak %>%
  mutate(
    
    time_to_next_outbreak_days =
      as.numeric(
        outbreak_start - end_date
      ),
    
    time_to_next_outbreak_weeks =
      time_to_next_outbreak_days / 7,
    
    outbreak_after_sia = 1
    
  )

#=========================================================
# Merge back to all SIAs
#=========================================================

sia_outcomes <- sia_analysis %>%
  left_join(
    next_outbreak %>%
      select(
        admin1,
        start_date,
        end_date,
        outbreak_start,
        outbreak_end,
        total_cases,
        duration_weeks,
        time_to_next_outbreak_days,
        time_to_next_outbreak_weeks,
        outbreak_after_sia
      ),
    by = c(
      "admin1",
      "start_date",
      "end_date"
    )
  )

#=========================================================
# Replace missing indicators
#=========================================================

sia_outcomes <- sia_outcomes %>%
  mutate(
    outbreak_after_sia =
      replace_na(
        outbreak_after_sia,
        0
      )
  )

#=========================================================
# Fixed follow-up windows
#=========================================================

sia_outcomes <- sia_outcomes %>%
  mutate(
    
    outbreak_within_6m =
      if_else(
        !is.na(time_to_next_outbreak_days) &
          time_to_next_outbreak_days <= 183,
        1,
        0
      ),
    
    outbreak_within_12m =
      if_else(
        !is.na(time_to_next_outbreak_days) &
          time_to_next_outbreak_days <= 365,
        1,
        0
      ),
    
    outbreak_within_24m =
      if_else(
        !is.na(time_to_next_outbreak_days) &
          time_to_next_outbreak_days <= 730,
        1,
        0
      )
    
  )

#=========================================================
# Rename outbreak characteristics
#=========================================================

sia_outcomes <- sia_outcomes %>%
  rename(
    next_outbreak_size = total_cases,
    next_outbreak_duration = duration_weeks
  )

#=========================================================
# Diagnostics
#=========================================================

cat("\nNumber of SIAs:\n")
nrow(sia_outcomes)

cat("\nOutbreak after SIA:\n")
table(sia_outcomes$outbreak_after_sia)

cat("\nTime to outbreak summary:\n")
summary(
  sia_outcomes$time_to_next_outbreak_days
)

cat("\nOutbreak within 12 months:\n")
table(
  sia_outcomes$outbreak_within_12m
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



#Additional diagnositcs 
sia_outcomes %>%
  arrange(time_to_next_outbreak_days) %>%
  select(
    admin1,
    sia_year,
    end_date,
    time_to_next_outbreak_days
  ) %>%
  head(20)


sia_outcomes %>%
  mutate(
    years_to_next_outbreak =
      time_to_next_outbreak_days / 365.25
  ) %>%
  summarise(
    median_years = median(years_to_next_outbreak, na.rm = TRUE),
    mean_years   = mean(years_to_next_outbreak, na.rm = TRUE)
  )


sia_outcomes %>%
  count(outbreak_within_12m) %>%
  mutate(
    pct = n / sum(n) * 100
  )


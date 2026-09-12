#=========================================================
# 11_kaplan_meier_analysis.R
#=========================================================

library(
  tidyverse
)

library(
  survival
)

library(
  survminer
)

#=========================================================
# Load survival dataset
#=========================================================

survival_data <- readRDS(
  "survival_dataset.rds"
)

#=========================================================
# Create survival object
#=========================================================

surv_object <- Surv(
  
  time =
    survival_data$time,
  
  event =
    survival_data$event
  
)

#=========================================================
# Overall Kaplan-Meier model
#=========================================================

km_overall <- survfit(
  
  surv_object ~ 1,
  
  data =
    survival_data
  
)

#=========================================================
# Print Kaplan-Meier results
#=========================================================

cat(
  "\n========================================\n"
)

cat(
  "OVERALL KAPLAN-MEIER ANALYSIS\n"
)

cat(
  "========================================\n"
)

print(
  summary(
    km_overall
  )
)

#=========================================================
# Median outbreak-free survival
#=========================================================

cat(
  "\nMedian outbreak-free survival:\n"
)

print(
  summary(
    km_overall
  )$table
)

#=========================================================
# Survival probabilities at fixed time points
#=========================================================

time_points <- c(
  
  90,
  
  183,
  
  365,
  
  730
  
)

km_timepoints <- summary(
  
  km_overall,
  
  times =
    time_points
  
)

km_survival <- tibble(
  
  time_days =
    km_timepoints$time,
  
  time_months =
    km_timepoints$time /
    30.44,
  
  time_years =
    km_timepoints$time /
    365.25,
  
  n_risk =
    km_timepoints$n.risk,
  
  n_events =
    km_timepoints$n.event,
  
  survival =
    km_timepoints$surv,
  
  lower_ci =
    km_timepoints$lower,
  
  upper_ci =
    km_timepoints$upper
  
)

cat(
  "\nSurvival probabilities:\n"
)

print(
  km_survival
)

#=========================================================
# Convert to cumulative outbreak probability
#=========================================================

km_cumulative_outbreak <- km_survival %>%
  
  mutate(
    
    cumulative_outbreak =
      
      1 - survival,
    
    cumulative_lower =
      
      1 - upper_ci,
    
    cumulative_upper =
      
      1 - lower_ci
    
  )

cat(
  "\nCumulative outbreak probability:\n"
)

print(
  km_cumulative_outbreak
)

#=========================================================
# Overall Kaplan-Meier plot
#=========================================================

km_plot <- ggsurvplot(
  
  km_overall,
  
  data =
    survival_data,
  
  risk.table =
    TRUE,
  
  conf.int =
    TRUE,
  
  xlab =
    "Days after supplementary immunization activity",
  
  ylab =
    "Probability of remaining outbreak-free",
  
  break.time.by =
    180,
  
  xlim =
    c(
      0,
      max(
        survival_data$time
      )
    ),
  
  ggtheme =
    theme_minimal()
  
)

print(
  km_plot
)

#=========================================================
# Save Kaplan-Meier figure
#=========================================================

ggsave(
  
  "km_overall.png",
  
  plot =
    km_plot$plot,
  
  width =
    8,
  
  height =
    6,
  
  dpi =
    300
  
)

#=========================================================
# Save risk table separately
#=========================================================

ggsave(
  
  "km_overall_risk_table.png",
  
  plot =
    km_plot$table,
  
  width =
    8,
  
  height =
    3,
  
  dpi =
    300
  
)

#=========================================================
# Save Kaplan-Meier estimates
#=========================================================

write_csv(
  
  km_survival,
  
  "km_survival_probabilities.csv"
  
)

write_csv(
  
  km_cumulative_outbreak,
  
  "km_cumulative_outbreak_probabilities.csv"
  
)

#=========================================================
# Save model
#=========================================================

saveRDS(
  
  km_overall,
  
  "km_overall_model.rds"
  
)

cat(
  "\n========================================\n"
)

cat(
  "KAPLAN-MEIER ANALYSIS COMPLETE\n"
)

cat(
  "========================================\n"
)

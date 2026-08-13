#=========================================================
#  10. SURVIVAL ANALYSIS
#=========================================================
  
library(tidyverse)
library(survival)
library(survminer)
library(broom)

#=========================================================
# Load data
#=========================================================

analytic_data <- readRDS(
  "analytic_dataset.rds"
)

analytic_data$births_1000 <-
  analytic_data$births / 1000

#=========================================================
# Create survival variables
#=========================================================

analytic_data <- analytic_data %>%
  
  mutate(
    
    event =
      outbreak_after_sia,
    
    time =
      time_to_next_outbreak_days
    
  )

#=========================================================
# Check outcome
#=========================================================

cat("\nNumber of SIAs:\n")
print(nrow(analytic_data))

cat("\nEvents:\n")
print(table(analytic_data$event))

cat("\nTime summary:\n")
print(summary(analytic_data$time))



#=========================================================
# Overall Kaplan-Meier
#=========================================================

km_fit <- survfit(
  
  Surv(time, event) ~ 1,
  
  data = analytic_data
  
)

summary(km_fit)

ggsurvplot(
  
  km_fit,
  
  risk.table = TRUE,
  
  xlab =
    "Days since SIA completion",
  
  ylab =
    "Probability of remaining outbreak-free",
  
  conf.int = TRUE
  
)

# SAVE plot
# 1. Open the PNG device
png("km_overall_plot.png", width = 8, height = 6, units = "in", res = 300)

# 2. Print the plot directly to the device
print(ggsurvplot(
  km_fit,
  risk.table = TRUE,
  xlab = "Days since SIA completion",
  ylab = "Probability of remaining outbreak-free",
  conf.int = TRUE
))

# 3. Turn the device off to finalize the file
dev.off()

#=========================================================
# Region type comparison
#=========================================================

km_region <- survfit(
  
  Surv(time, event) ~ region_type,
  
  data = analytic_data
  
)

ggsurvplot(
  
  km_region,
  
  risk.table = TRUE,
  
  pval = TRUE,
  
  conf.int = FALSE,
  
  xlab =
    "Days since SIA completion",
  
  ylab =
    "Outbreak-free probability"
  
)

#SAVE
# 1. Open the PNG device
png("km_region_type_plot.png", width = 10, height = 6, units = "in", res = 600)

# 2. Print the plot directly to the device
print(ggsurvplot(
  km_region,
  risk.table = TRUE,
  xlab = "Days since SIA completion",
  ylab = "Outbreak-free probability",
  conf.int = TRUE
))

# 3. Turn the device off to finalize the file
dev.off()

#With logrank test 
#================
# 1. Open the high-res PNG device
png("km_region_type_plot_lr.png", width = 10, height = 6, units = "in", res = 600)

# 2. Print the grouped plot directly to the device
print(ggsurvplot(
  km_region,
  risk.table = TRUE,
  pval = TRUE,                 # Add this to display the log-rank test
  conf.int = FALSE,            # Matching your group preferences
  xlab = "Days since SIA completion",
  ylab = "Outbreak-free probability"
))

# 3. Finalize the file
dev.off()

#Univariate cox models

#=========================================================
# Candidate predictors
#=========================================================

candidate_vars <- c(
  
  "mcv1",
  
  "mcv2",
  
  "births",
  
  "doses",
  
  "campaign_duration_days",
  
  "previous_outbreaks_12m",
  
  "previous_cases_12m",
  
  "region_type"
  
)


#=========================================================
# Univariate Cox models
#=========================================================

univ_results <- list()

for(v in candidate_vars){
  
  formula <- as.formula(
    
    paste(
      "Surv(time,event) ~",
      v
    )
    
  )
  
  fit <- coxph(
    
    formula,
    
    data = analytic_data
    
  )
  
  univ_results[[v]] <-
    
    tidy(
      fit,
      exponentiate = TRUE,
      conf.int = TRUE
    )
  
}

univ_results_df <-
  
  bind_rows(
    univ_results,
    .id = "variable"
  )

print(univ_results_df)


#=========================================================
# Multivariable Cox model
#=========================================================

# cox_model <- coxph(
#   
#   Surv(time,event) ~
#     
#     mcv1 +
#     
#     births +
#     
#     previous_outbreaks_12m +
#     
#     region_type +
#     
#     campaign_duration_days,
#   
#   data = analytic_data
#   
# )
# 
# summary(cox_model)

#=====================
  analytic_data$births_1000 <-
  analytic_data$births / 1000

cox_model2 <- coxph(
  Surv(time,event) ~
    births_1000 +
    previous_outbreaks_12m +
    region_type +
    campaign_duration_days,
  data = analytic_data
)

summary(cox_model2)

cox.zph(cox_model2)


#Extract HR (For table 4/5) 

multivariable_results <-
  
  tidy(
    
    cox_model,
    
    exponentiate = TRUE,
    
    conf.int = TRUE
    
  )

print(multivariable_results)

#Model diagnostics 
#Testing for assumptions 

#=========================================================
# PH assumption
#=========================================================

ph_test <- cox.zph(
  cox_model2
)

print(ph_test)

plot(ph_test)


#=========================================================
# Shared frailty model
#=========================================================

frailty_model <- coxph(
  
  Surv(time,event) ~
    
    mcv1 +
    
    births +
    
    previous_outbreaks_12m +
    
    region_type +
    
    frailty(admin1),
  
  data = analytic_data
  
)

summary(frailty_model)

#===============


frailty_model12 <- coxph(
  
  Surv(time,event) ~
    
    births_1000 +
    
    previous_outbreaks_12m +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(admin1),
  
  data = analytic_data
  
)

summary(frailty_model12)

#Model comparison 
#================
AIC(
  cox_model,
  frailty_model
)


#Save results/outputs 
write_csv(
  
  multivariable_results,
  
  "cox_multivariable_results.csv"
  
)

saveRDS(
  
  cox_model,
  
  "cox_model.rds"
  
)

saveRDS(
  
  frailty_model,
  
  "frailty_model.rds"
  
)


#A Descriptive table 1
analytic_data <- analytic_data %>%
  mutate(
    prev_outbreak_group =
      case_when(
        previous_outbreaks_12m == 0 ~ "None",
        previous_outbreaks_12m >= 1 ~ ">=1",
        TRUE ~ NA_character_
      )
  )

table(
  analytic_data$prev_outbreak_group,
  useNA = "ifany"
)

#Table 1

library(dplyr)

none <- analytic_data %>%
  filter(prev_outbreak_group == "None")

prev <- analytic_data %>%
  filter(prev_outbreak_group == ">=1")

table1 <- tibble(
  
  Characteristic = c(
    "Number of SIAs",
    "Agrarian, n (%)",
    "Pastoralist, n (%)",
    "Urban, n (%)",
    "MCV1 coverage, median (IQR)",
    "Births, median (IQR)",
    "Doses administered, median (IQR)",
    "Campaign duration, median (IQR)",
    "Time to next outbreak, median (IQR)",
    "Outbreak within 6 months, n (%)",
    "Outbreak within 12 months, n (%)"
  ),
  
  `None` = c(
    
    nrow(none),
    
    sprintf("%d (%.1f%%)",
            sum(none$region_type=="Agrarian"),
            100*mean(none$region_type=="Agrarian")),
    
    sprintf("%d (%.1f%%)",
            sum(none$region_type=="Pastoralist"),
            100*mean(none$region_type=="Pastoralist")),
    
    sprintf("%d (%.1f%%)",
            sum(none$region_type=="Urban"),
            100*mean(none$region_type=="Urban")),
    
    sprintf("%.3f (%.3f–%.3f)",
            median(none$mcv1,na.rm=TRUE),
            quantile(none$mcv1,.25,na.rm=TRUE),
            quantile(none$mcv1,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(none$births,na.rm=TRUE),
            quantile(none$births,.25,na.rm=TRUE),
            quantile(none$births,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(none$doses,na.rm=TRUE),
            quantile(none$doses,.25,na.rm=TRUE),
            quantile(none$doses,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(none$campaign_duration_days,na.rm=TRUE),
            quantile(none$campaign_duration_days,.25,na.rm=TRUE),
            quantile(none$campaign_duration_days,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(none$time_to_next_outbreak_days,na.rm=TRUE),
            quantile(none$time_to_next_outbreak_days,.25,na.rm=TRUE),
            quantile(none$time_to_next_outbreak_days,.75,na.rm=TRUE)),
    
    sprintf("%d (%.1f%%)",
            sum(none$outbreak_within_6m==1,na.rm=TRUE),
            100*mean(none$outbreak_within_6m==1,na.rm=TRUE)),
    
    sprintf("%d (%.1f%%)",
            sum(none$outbreak_within_12m==1,na.rm=TRUE),
            100*mean(none$outbreak_within_12m==1,na.rm=TRUE))
  ),
  
  `>=1` = c(
    
    nrow(prev),
    
    sprintf("%d (%.1f%%)",
            sum(prev$region_type=="Agrarian"),
            100*mean(prev$region_type=="Agrarian")),
    
    sprintf("%d (%.1f%%)",
            sum(prev$region_type=="Pastoralist"),
            100*mean(prev$region_type=="Pastoralist")),
    
    sprintf("%d (%.1f%%)",
            sum(prev$region_type=="Urban"),
            100*mean(prev$region_type=="Urban")),
    
    sprintf("%.3f (%.3f–%.3f)",
            median(prev$mcv1,na.rm=TRUE),
            quantile(prev$mcv1,.25,na.rm=TRUE),
            quantile(prev$mcv1,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(prev$births,na.rm=TRUE),
            quantile(prev$births,.25,na.rm=TRUE),
            quantile(prev$births,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(prev$doses,na.rm=TRUE),
            quantile(prev$doses,.25,na.rm=TRUE),
            quantile(prev$doses,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(prev$campaign_duration_days,na.rm=TRUE),
            quantile(prev$campaign_duration_days,.25,na.rm=TRUE),
            quantile(prev$campaign_duration_days,.75,na.rm=TRUE)),
    
    sprintf("%.0f (%.0f–%.0f)",
            median(prev$time_to_next_outbreak_days,na.rm=TRUE),
            quantile(prev$time_to_next_outbreak_days,.25,na.rm=TRUE),
            quantile(prev$time_to_next_outbreak_days,.75,na.rm=TRUE)),
    
    sprintf("%d (%.1f%%)",
            sum(prev$outbreak_within_6m==1,na.rm=TRUE),
            100*mean(prev$outbreak_within_6m==1,na.rm=TRUE)),
    
    sprintf("%d (%.1f%%)",
            sum(prev$outbreak_within_12m==1,na.rm=TRUE),
            100*mean(prev$outbreak_within_12m==1,na.rm=TRUE))
  )
  
)

table1


#Outbreaks table
outbreak_summary <- tibble(
  
  Characteristic = c(
    "Number of outbreaks",
    "Administrative regions",
    "Study period",
    "Outbreak duration, median (IQR), weeks",
    "Total cases per outbreak, median (IQR)",
    "Peak weekly cases, median (IQR)",
    "Mean weekly cases, median (IQR)",
    "Time to next outbreak, median (IQR), days"
  ),
  
  Value = c(
    
    nrow(outbreaks),
    
    n_distinct(outbreaks$admin1),
    
    paste(
      min(outbreaks$outbreak_year, na.rm = TRUE),
      "-",
      max(outbreaks$outbreak_year, na.rm = TRUE)
    ),
    
    sprintf(
      "%.1f (%.1f–%.1f)",
      median(outbreaks$duration_weeks, na.rm = TRUE),
      quantile(outbreaks$duration_weeks, 0.25, na.rm = TRUE),
      quantile(outbreaks$duration_weeks, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      median(outbreaks$total_cases, na.rm = TRUE),
      quantile(outbreaks$total_cases, 0.25, na.rm = TRUE),
      quantile(outbreaks$total_cases, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      median(outbreaks$peak_cases, na.rm = TRUE),
      quantile(outbreaks$peak_cases, 0.25, na.rm = TRUE),
      quantile(outbreaks$peak_cases, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.1f (%.1f–%.1f)",
      median(outbreaks$mean_weekly_cases, na.rm = TRUE),
      quantile(outbreaks$mean_weekly_cases, 0.25, na.rm = TRUE),
      quantile(outbreaks$mean_weekly_cases, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      median(outbreaks$recurrence_days, na.rm = TRUE),
      quantile(outbreaks$recurrence_days, 0.25, na.rm = TRUE),
      quantile(outbreaks$recurrence_days, 0.75, na.rm = TRUE)
    )
  )
)

outbreak_summary


#Births and immunization 
birth_ri_summary <- tibble(
  
  Characteristic = c(
    "Administrative regions",
    "Weekly observations",
    "Births, median (IQR)",
    "MCV1 coverage, median (IQR)",
    "MCV2 coverage, median (IQR)",
    "MCV1 coverage, range",
    "MCV2 coverage, range"
  ),
  
  Value = c(
    
    n_distinct(birth_ri_weekly$admin1),
    
    nrow(birth_ri_weekly),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      median(birth_ri_weekly$births, na.rm = TRUE),
      quantile(birth_ri_weekly$births, 0.25, na.rm = TRUE),
      quantile(birth_ri_weekly$births, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      median(birth_ri_weekly$mcv1, na.rm = TRUE),
      quantile(birth_ri_weekly$mcv1, 0.25, na.rm = TRUE),
      quantile(birth_ri_weekly$mcv1, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      median(birth_ri_weekly$mcv2, na.rm = TRUE),
      quantile(birth_ri_weekly$mcv2, 0.25, na.rm = TRUE),
      quantile(birth_ri_weekly$mcv2, 0.75, na.rm = TRUE)
    ),
    
    sprintf(
      "%.3f–%.3f",
      min(birth_ri_weekly$mcv1, na.rm = TRUE),
      max(birth_ri_weekly$mcv1, na.rm = TRUE)
    ),
    
    sprintf(
      "%.3f–%.3f",
      min(birth_ri_weekly$mcv2, na.rm = TRUE),
      max(birth_ri_weekly$mcv2, na.rm = TRUE)
    )
  )
)

birth_ri_summary


#Table (regional descrition of outbreaks after SIAs)
region_order <- c(
  "Amhara",
  "Addis Ababa",
  "South Ethiopia",
  "South West Ethiopia",
  "Oromia",
  "Somali",
  "Sidama",
  "Tigray",
  "Gambela",
  "Central Ethiopia",
  "Afar",
  "Benishangul Gumuz",
  "Dire Dawa",
  "Harari"
)

table_region <- analytic_data %>%
  
  group_by(admin1) %>%
  
  summarise(
    
    `Number of SIAs` = n(),
    
    `Median days to next outbreak` =
      round(median(time, na.rm = TRUE), 0),
    
    `SIAs with ≥1 outbreak in preceding 12 months (%)` =
      round(
        mean(previous_outbreaks_12m > 0, na.rm = TRUE) * 100,
        1
      )
    
  ) %>%
  
  mutate(
    admin1 = factor(admin1, levels = region_order)
  ) %>%
  
  arrange(admin1)

table_region


##=========================================================
# Expanded descriptive table by previous outbreak history
#=========================================================
analytic_data <- analytic_data %>%
  mutate(
    prev_outbreak_group =
      case_when(
        previous_outbreaks_12m == 0 ~ "None",
        previous_outbreaks_12m >= 1 ~ ">=1",
        TRUE ~ NA_character_
      )
  )

# library(dplyr)
# 
# table1 <- analytic_data %>%
#   
#   group_by(prev_outbreak_group) %>%
#   
#   summarise(
#     
#     SIAs = n(),
#     
#     median_mcv1 =
#       round(
#         median(mcv1, na.rm = TRUE),
#         3
#       ),
#     
#     median_births =
#       round(
#         median(births, na.rm = TRUE),
#         0
#       ),
#     
#     median_doses =
#       round(
#         median(doses, na.rm = TRUE),
#         0
#       ),
#     
#     median_campaign_days =
#       round(
#         median(campaign_duration_days, na.rm = TRUE),
#         0
#       ),
#     
#     median_time_to_outbreak =
#       round(
#         median(time_to_next_outbreak_days,
#                na.rm = TRUE),
#         0
#       ),
#     
#     outbreak_within_6m =
#       round(
#         mean(outbreak_within_6m,
#              na.rm = TRUE) * 100,
#         1
#       ),
#     
#     outbreak_within_12m =
#       round(
#         mean(outbreak_within_12m,
#              na.rm = TRUE) * 100,
#         1
#       )
#     
#   )
# 
# print(table1)
# 
# library(dplyr)
# 
# table1 <- analytic_data %>%
#   
#   group_by(prev_outbreak_group) %>%
#   
#   summarise(
#     
#     SIAs = n(),
#     
#     Agrarian =
#       paste0(
#         sum(region_type == "Agrarian"),
#         " (",
#         round(mean(region_type == "Agrarian") * 100, 1),
#         "%)"
#       ),
#     
#     Pastoralist =
#       paste0(
#         sum(region_type == "Pastoralist"),
#         " (",
#         round(mean(region_type == "Pastoralist") * 100, 1),
#         "%)"
#       ),
#     
#     Urban =
#       paste0(
#         sum(region_type == "Urban"),
#         " (",
#         round(mean(region_type == "Urban") * 100, 1),
#         "%)"
#       ),
#     
#     MCV1 =
#       paste0(
#         round(median(mcv1, na.rm = TRUE), 3),
#         " (",
#         round(quantile(mcv1, .25, na.rm = TRUE), 3),
#         "–",
#         round(quantile(mcv1, .75, na.rm = TRUE), 3),
#         ")"
#       ),
#     
#     Births =
#       paste0(
#         round(median(births, na.rm = TRUE), 0),
#         " (",
#         round(quantile(births, .25, na.rm = TRUE), 0),
#         "–",
#         round(quantile(births, .75, na.rm = TRUE), 0),
#         ")"
#       ),
#     
#     Doses =
#       paste0(
#         round(median(doses, na.rm = TRUE), 0),
#         " (",
#         round(quantile(doses, .25, na.rm = TRUE), 0),
#         "–",
#         round(quantile(doses, .75, na.rm = TRUE), 0),
#         ")"
#       ),
#     
#     Campaign_duration =
#       paste0(
#         round(median(campaign_duration_days, na.rm = TRUE), 0),
#         " (",
#         round(quantile(campaign_duration_days, .25, na.rm = TRUE), 0),
#         "–",
#         round(quantile(campaign_duration_days, .75, na.rm = TRUE), 0),
#         ")"
#       ),
#     
#     Time_to_next_outbreak =
#       paste0(
#         round(median(time_to_next_outbreak_days, na.rm = TRUE), 0),
#         " (",
#         round(quantile(time_to_next_outbreak_days, .25, na.rm = TRUE), 0),
#         "–",
#         round(quantile(time_to_next_outbreak_days, .75, na.rm = TRUE), 0),
#         ")"
#       ),
#     
#     Outbreak_6m =
#       paste0(
#         sum(outbreak_within_6m == 1, na.rm = TRUE),
#         " (",
#         round(mean(outbreak_within_6m == 1, na.rm = TRUE) * 100, 1),
#         "%)"
#       ),
#     
#     Outbreak_12m =
#       paste0(
#         sum(outbreak_within_12m == 1, na.rm = TRUE),
#         " (",
#         round(mean(outbreak_within_12m == 1, na.rm = TRUE) * 100, 1),
#         "%)"
#       )
#     
#   )
# 
# print(table1)


library(tidyverse)
library(gtsummary)

# Prepare variables for presentation
table_data <- analytic_data %>%
  mutate(
    prev_outbreak_group = factor(
      prev_outbreak_group,
      levels = c("None", ">=1"),
      labels = c(
        "No outbreak in preceding 12 months",
        "≥1 outbreak in preceding 12 months"
      )
    ),
    
    region_type = factor(region_type),
    
    age_group = factor(age_group),
    
    age_target = factor(age_target),
    
    outbreak_after_sia = factor(
      outbreak_after_sia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    outbreak_within_6m = factor(
      outbreak_within_6m,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    outbreak_within_12m = factor(
      outbreak_within_12m,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    outbreak_within_24m = factor(
      outbreak_within_24m,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    
    # Present coverage as percentages
    mcv1_pct = mcv1 * 100,
    mcv2_pct = mcv2 * 100
  )

# Create Table 2
table2_sia_characteristics <- table_data %>%
  select(
    prev_outbreak_group,
    sia_year,
    region_type,
    age_group,
    age_target,
    target_upper_age_years,
    campaign_duration_days,
    doses,
    births,
    mcv1_pct,
    mcv2_pct,
    previous_cases_12m,
    previous_outbreak_size,
    previous_outbreak_duration,
    next_outbreak_size,
    next_outbreak_duration,
    time_to_next_outbreak_days,
    outbreak_after_sia,
    outbreak_within_6m,
    outbreak_within_12m,
    outbreak_within_24m
  ) %>%
  
  tbl_summary(
    by = prev_outbreak_group,
    
    statistic = list(
      all_continuous() ~ "{median} ({p25}–{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    digits = list(
      sia_year ~ 0,
      target_upper_age_years ~ 0,
      campaign_duration_days ~ 0,
      doses ~ 0,
      births ~ 0,
      mcv1_pct ~ 1,
      mcv2_pct ~ 1,
      previous_cases_12m ~ 0,
      previous_outbreak_size ~ 0,
      previous_outbreak_duration ~ 0,
      next_outbreak_size ~ 0,
      next_outbreak_duration ~ 0,
      time_to_next_outbreak_days ~ 0
    ),
    
    label = list(
      sia_year ~ "SIA year",
      region_type ~ "Region type",
      age_group ~ "Campaign age group",
      age_target ~ "Age-target category",
      target_upper_age_years ~ "Upper target age, years",
      campaign_duration_days ~ "Campaign duration, days",
      doses ~ "Doses administered",
      births ~ "Estimated daily births",
      mcv1_pct ~ "MCV1 coverage, %",
      mcv2_pct ~ "MCV2 coverage, %",
      previous_cases_12m ~ "Measles cases in preceding 12 months",
      previous_outbreak_size ~ "Size of most recent previous outbreak",
      previous_outbreak_duration ~
        "Duration of most recent previous outbreak, weeks",
      next_outbreak_size ~ "Next outbreak size, cases",
      next_outbreak_duration ~ "Next outbreak duration, weeks",
      time_to_next_outbreak_days ~ "Time to next outbreak, days",
      outbreak_after_sia ~ "Outbreak observed during follow-up",
      outbreak_within_6m ~ "Outbreak within 6 months",
      outbreak_within_12m ~ "Outbreak within 12 months",
      outbreak_within_24m ~ "Outbreak within 24 months"
    ),
    
    missing = "no",
    missing_text = "Missing"
  ) %>%
  
  add_overall(last = FALSE) %>%
  
  add_p(
    test = list(
      all_continuous() ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    ),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  
  add_n() %>%
  
  modify_header(
    label ~ "**Characteristic**",
    n ~ "**Non-missing N**",
    all_stat_cols() ~ "**{level}**",
    p.value ~ "**p-value**"
  ) %>%
  
  bold_labels()

table2_sia_characteristics

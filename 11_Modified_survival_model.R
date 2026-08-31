#=========================================================
# SCRIPT 10: SURVIVAL MODEL REFINEMENT AND COMPARISON
#=========================================================

library(tidyverse)
library(survival)
library(survminer)
library(broom)

#=========================================================
# 1. DEFINE THE END OF SURVEILLANCE
#=========================================================

# Preferred option: derive it from the surveillance dataset
study_end_date <- max(
  measles_data$date_onset,
  na.rm = TRUE
)

# Check
study_end_date



#If surveillance data not loaded, set manually 
# study_end_date <- as.Date("2025-05-05")

#Correctly code right censored observations
analytic_surv <- analytic_data %>%
  
  mutate(
    
    # Event indicator
    event = if_else(
      outbreak_after_sia == 1,
      1L,
      0L
    ),
    
    # Observed event time or censoring time
    time = case_when(
      
      event == 1 ~
        as.numeric(
          outbreak_start - end_date
        ),
      
      event == 0 ~
        as.numeric(
          study_end_date - end_date
        ),
      
      TRUE ~ NA_real_
    ),
    
    # Main exposure
    prev_outbreak_group = factor(
      if_else(
        previous_outbreaks_12m == 0,
        "None",
        ">=1"
      ),
      levels = c("None", ">=1")
    ),
    
    # Scaled variables
    births_1000 = births / 1000,
    
    doses_million = doses / 1000000,
    
    previous_cases_100 =
      previous_cases_12m / 100,
    
    # Log transformations for strongly skewed variables
    log_previous_cases =
      log1p(previous_cases_12m),
    
    log_previous_outbreak_size =
      log1p(previous_outbreak_size),
    
    log_previous_outbreak_duration =
      log1p(previous_outbreak_duration),
    
    region_type = factor(
      region_type,
      levels = c(
        "Agrarian",
        "Pastoralist",
        "Urban"
      )
    ),
    
    age_target = factor(age_target),
    
    admin1 = factor(admin1)
  )

#Validate sensonring and survival times 
cat("\nEvent distribution:\n")

print(
  table(
    analytic_surv$event,
    useNA = "ifany"
  )
)

cat("\nMissing survival times:\n")

print(
  sum(
    is.na(analytic_surv$time)
  )
)

cat("\nSurvival-time summary:\n")

print(
  summary(
    analytic_surv$time
  )
)

cat("\nCensored observations:\n")

analytic_surv %>%
  filter(event == 0) %>%
  select(
    admin1,
    sia_year,
    end_date,
    time,
    event
  ) %>%
  print()


#Check correlation of campaign duration with campaign size 
campaign_corr <- analytic_surv %>%
  
  select(
    campaign_duration_days,
    births_1000,
    doses_million
  ) %>%
  
  cor(
    use = "pairwise.complete.obs",
    method = "spearman"
  )

round(
  campaign_corr,
  3
)


#optional pairwise tests 
cor.test(
  analytic_surv$campaign_duration_days,
  analytic_surv$doses_million,
  method = "spearman",
  exact = FALSE
)

cor.test(
  analytic_surv$campaign_duration_days,
  analytic_surv$births_1000,
  method = "spearman",
  exact = FALSE
)


#Correlation among candidate variables/covariates 
#=========================================================
# Correlation among candidate covariates
#=========================================================

candidate_covariates <- analytic_surv %>%
  
  select(
    
    births_1000,
    
    mcv1,
    mcv2,
    
    doses,
    
    campaign_duration_days,
    
    previous_outbreaks_12m,
    previous_cases_12m,
    
    previous_outbreak_size,
    previous_outbreak_duration
    
  )

cor_matrix <- cor(
  candidate_covariates,
  method = "spearman",
  use = "pairwise.complete.obs"
)

round(cor_matrix, 2)


#Find the abnormal ones
#=========================================================
# High correlations
#=========================================================

library(tidyverse)

cor_df <- as.data.frame(as.table(cor_matrix))

high_correlations <- cor_df %>%
  
  filter(
    Var1 != Var2,
    abs(Freq) >= 0.70
  ) %>%
  
  arrange(desc(abs(Freq)))

print(high_correlations)

#Publication readycorrelation_table <- round(cor_matrix, 2)
correlation_table <- round(cor_matrix, 2)

knitr::kable(
  correlation_table,
  caption = "Spearman correlation matrix of candidate covariates."
)


#Overall kaplan meier analysis 
km_overall <- survfit(
  Surv(time, event) ~ 1,
  data = analytic_surv
)

print(
  summary(
    km_overall,
    times = c(
      183,
      365,
      730
    )
  )
)

km_overall_plot <- ggsurvplot(
  
  km_overall,
  
  data = analytic_surv,
  
  risk.table = TRUE,
  
  conf.int = TRUE,
  
  break.time.by = 365,
  
  xlab =
    "Days since SIA completion",
  
  ylab =
    "Probability of remaining outbreak-free",
  
  censor = TRUE
)

km_overall_plot

#SAVE
# 1. Open the PNG device
png("km_overall_plot_mod.png", width = 10, height = 6, units = "in", res = 600)

# 2. Print the plot directly to the device
print(ggsurvplot(
  km_overall,
  risk.table = TRUE,
  xlab = "Days since SIA completion",
  ylab = "Outbreak-free probability",
  conf.int = TRUE
))

# 3. Turn the device off to finalize the file
dev.off()


#KM by previous outbreak history 
km_previous <- survfit(
  
  Surv(time, event) ~
    prev_outbreak_group,
  
  data = analytic_surv
)

summary(km_previous)

km_previous_plot <- ggsurvplot(
  
  km_previous,
  
  data = analytic_surv,
  
  risk.table = TRUE,
  
  pval = TRUE,
  
  conf.int = TRUE,
  
  censor = TRUE,
  
  break.time.by = 365,
  
  xlab =
    "Days since SIA completion",
  
  ylab =
    "Probability of remaining outbreak-free",
  
  legend.title =
    "Outbreaks in preceding\n12 months",
  
  legend.labs =
    c(
      "None",
      "≥1"
    )
)

km_previous_plot

#SAVE
# 1. Open the PNG device
png("km_previous_plot_mod.png", width = 10, height = 6, units = "in", res = 600)

# 2. Print the plot directly to the device
print(ggsurvplot(
  km_previous,
  risk.table = TRUE,
  pval = TRUE,       
  xlab = "Days since SIA completion",
  ylab = "Outbreak-free probability",
  conf.int = TRUE,
  palette = c("#F8766D", "#00BFC4"),, # Distinct colors 
  legend.labs = c("No Previous Outbreak", "Previous Outbreak (≥1)"), # Custom labels for your legend
  legend.title = "Region Type"
))

# 3. Turn the device off to finalize the file
dev.off()


#Explicit log rank test 
logrank_previous <- survdiff(
  
  Surv(time, event) ~
    prev_outbreak_group,
  
  data = analytic_surv
)

print(logrank_previous)


#Median survival by exposure group 
surv_median(
  km_previous
)


#KM by region type 
km_region <- survfit(
  
  Surv(time, event) ~
    region_type,
  
  data = analytic_surv
)

km_region_plot <- ggsurvplot(
  
  km_region,
  
  data = analytic_surv,
  
  risk.table = TRUE,
  
  pval = TRUE,
  
  conf.int = FALSE,
  
  censor = TRUE,
  
  break.time.by = 365,
  
  xlab =
    "Days since SIA completion",
  
  ylab =
    "Probability of remaining outbreak-free",
  
  legend.title =
    "Region type"
)

km_region_plot


#SAVE
# 1. Open the PNG device
png("km_region_type_plot_mod.png", width = 10, height = 6, units = "in", res = 600)

# 2. Print the plot directly to the device
print(ggsurvplot(
  km_region,
  risk.table = TRUE,
  pval = TRUE,       
  xlab = "Days since SIA completion",
  ylab = "Outbreak-free probability",
  conf.int = TRUE,
  palette = c("#E41A1C","#4DAF4A", "#377EB8"), # Distinct colors for Agrarian, Pastoralist, Urban
  legend.labs = c("Agrarian", "Pastoralist", "Urban"), # Custom labels for your legend
  legend.title = "Region Type"
))

# 3. Turn the device off to finalize the file
dev.off()


#Explicit log rank test 
logrank_previous <- survdiff(
  
  Surv(time, event) ~
    region_type,
  
  data = analytic_surv
)

print(logrank_previous)


#Median survival by exposure group 
surv_median(
  km_region
)


#Standard cox proportional hazards model: sensitivity 
cox_standard <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    previous_outbreaks_12m +
    
    region_type +
    
    campaign_duration_days,
  
  data = analytic_surv,
  
  ties = "efron",
  
  x = TRUE
)

summary(cox_standard)

#Extract results 
cox_standard_results <- tidy(
  
  cox_standard,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) %>%
  
  mutate(
    model =
      "Standard Cox model"
  )

print(cox_standard_results)


#Proportional hazards assumption test 
ph_standard <- cox.zph(
  cox_standard
)

print(ph_standard)

plot(ph_standard)

ggsave(
  plot(ph_standard),
  filename = "ph_assumption_standard_cox.png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600
)


#Primary shared frailty cox model
frailty_main <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    previous_outbreaks_12m +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_main)


#Extract fixed effects estimates 
frailty_main_results <- tidy(
  
  frailty_main,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) %>%
  
  filter(
    !str_detect(
      term,
      "frailty"
    )
  ) %>%
  
  mutate(
    model =
      "Shared-frailty model: outbreak count"
  )

print(frailty_main_results)


#Alternative historical-burden model A: previous cases
frailty_cases <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    log_previous_cases +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_cases)


#Alternative historical-burden model B: previous outbreak size
frailty_size <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    log_previous_outbreak_size +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_size)


#Optional alternative model C: previous outbreak duration
frailty_duration <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    log_previous_outbreak_duration +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_duration)


#Compare model fit 
model_comparison <- tibble(
  
  model = c(
    "Standard Cox: previous outbreaks",
    "Frailty: previous outbreaks",
    "Frailty: previous cases",
    "Frailty: previous outbreak size",
    "Frailty: previous outbreak duration"
  ),
  
  AIC = c(
    AIC(cox_standard),
    AIC(frailty_main),
    AIC(frailty_cases),
    AIC(frailty_size),
    AIC(frailty_duration)
  ),
  
  concordance = c(
    summary(cox_standard)$concordance[1],
    summary(frailty_main)$concordance[1],
    summary(frailty_cases)$concordance[1],
    summary(frailty_size)$concordance[1],
    summary(frailty_duration)$concordance[1]
  )
) %>%
  
  arrange(AIC)

print(model_comparison)


#Extract all frailty model estimates 
frailty_cases_results <- tidy(
  
  frailty_cases,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) %>%
  
  filter(
    !str_detect(term, "frailty")
  ) %>%
  
  mutate(
    model =
      "Shared-frailty model: previous cases"
  )


frailty_size_results <- tidy(
  
  frailty_size,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) %>%
  
  filter(
    !str_detect(term, "frailty")
  ) %>%
  
  mutate(
    model =
      "Shared-frailty model: previous outbreak size"
  )


frailty_duration_results <- tidy(
  
  frailty_duration,
  
  exponentiate = TRUE,
  
  conf.int = TRUE
) %>%
  
  filter(
    !str_detect(term, "frailty")
  ) %>%
  
  mutate(
    model =
      "Shared-frailty model: previous outbreak duration"
  )


all_model_results <- bind_rows(
  
  cox_standard_results,
  
  frailty_main_results,
  
  frailty_cases_results,
  
  frailty_size_results,
  
  frailty_duration_results
)

print(all_model_results)


#Check whether age target adds useful information 
frailty_age_sensitivity <- coxph(
  
  Surv(time, event) ~
    
    births_1000 +
    
    previous_outbreaks_12m +
    
    region_type +
    
    campaign_duration_days +
    
    age_target +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_age_sensitivity)

AIC(
  frailty_main,
  frailty_age_sensitivity
)

#Check whether doses improve primary model 
frailty_doses_sensitivity <- coxph(
  
  Surv(time, event) ~
    
    doses_million +
    
    previous_outbreaks_12m +
    
    region_type +
    
    campaign_duration_days +
    
    frailty(
      admin1,
      distribution = "gamma"
    ),
  
  data = analytic_surv,
  
  ties = "efron"
)

summary(frailty_doses_sensitivity)

AIC(
  frailty_main,
  frailty_doses_sensitivity
)

#Model results table 
final_model_table <- bind_rows(
  
  tidy(
    cox_standard,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    mutate(
      Model =
        "Standard Cox"
    ),
  
  tidy(
    frailty_main,
    exponentiate = TRUE,
    conf.int = TRUE
  ) %>%
    filter(
      !str_detect(term, "frailty")
    ) %>%
    mutate(
      Model =
        "Shared frailty Cox"
    )
) %>%
  
  transmute(
    
    Model,
    
    Predictor = term,
    
    HR = round(
      estimate,
      2
    ),
    
    `95% CI` = paste0(
      round(conf.low, 2),
      "–",
      round(conf.high, 2)
    ),
    
    `p-value` = case_when(
      
      p.value < 0.001 ~
        "<0.001",
      
      TRUE ~
        sprintf(
          "%.3f",
          p.value
        )
    )
  )

print(final_model_table)



#Save outputs 
write_csv(
  model_comparison,
  "model_comparison.csv"
)

write_csv(
  all_model_results,
  "all_survival_model_results.csv"
)

write_csv(
  final_model_table,
  "final_cox_frailty_results.csv"
)

saveRDS(
  analytic_surv,
  "analytic_survival_dataset.rds"
)

saveRDS(
  cox_standard,
  "standard_cox_model.rds"
)

saveRDS(
  frailty_main,
  "primary_frailty_model.rds"
)

saveRDS(
  frailty_cases,
  "frailty_previous_cases_model.rds"
)

saveRDS(
  frailty_size,
  "frailty_previous_size_model.rds"
)

saveRDS(
  frailty_duration,
  "frailty_previous_duration_model.rds"
)

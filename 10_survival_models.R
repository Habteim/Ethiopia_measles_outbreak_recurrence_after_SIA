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




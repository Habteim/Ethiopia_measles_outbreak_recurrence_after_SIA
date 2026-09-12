#=========================================================
# 14_multivariable_cox_regression.R
#
# Multivariable Cox proportional hazards regression
#
# Primary model:
#   Previous outbreak frequency
#
# Alternative model:
#   Previous transmission burden
#
# Sensitivity analyses:
#   1. Cluster-robust standard errors by region
#   2. Shared gamma frailty by region
#=========================================================


#=========================================================
# LOAD REQUIRED PACKAGES
#=========================================================

library(
  
  survival
  
)


library(
  
  car
  
)


#=========================================================
# MODEL A
#
# PRIMARY MULTIVARIABLE COX MODEL
#
# Previous outbreak frequency
#=========================================================

cox_model_A <-
  
  coxph(
    
    Surv(time, event) ~
      
      previous_outbreaks_12m +
      
      births_1000 +
      
      region_type +
      
      calendar_period +
      
      campaign_duration_days +
      
      mcv1_10pct,
    
    data =
      survival_data
    
  )


#=========================================================
# MODEL B
#
# ALTERNATIVE MULTIVARIABLE COX MODEL
#
# Previous transmission burden
#
# Replaces previous_outbreaks_12m with
# log_previous_cases
#=========================================================

cox_model_B <-
  
  coxph(
    
    Surv(time, event) ~
      
      log_previous_cases +
      
      births_1000 +
      
      region_type +
      
      calendar_period +
      
      campaign_duration_days +
      
      mcv1_10pct,
    
    data =
      survival_data
    
  )


#=========================================================
# MODEL C
#
# REDUCED VERSION OF MODEL A
#
# Campaign duration removed
#=========================================================

cox_model_C <-
  
  coxph(
    
    Surv(time, event) ~
      
      previous_outbreaks_12m +
      
      births_1000 +
      
      region_type +
      
      calendar_period +
      
      mcv1_10pct,
    
    data =
      survival_data
    
  )


#=========================================================
# MODEL COMPARISON
#
# Compare Model A with Model B
#
# Compare Model A with reduced Model C
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MODEL COMPARISON: AIC\n"
  
)


cat(
  
  "========================================\n"
  
)


AIC(
  
  cox_model_A,
  
  cox_model_B
  
)


AIC(
  
  cox_model_A,
  
  cox_model_C
  
)


#=========================================================
# MULTICOLLINEARITY
#
# Examine the primary model
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MULTICOLLINEARITY DIAGNOSTICS\n"
  
)


cat(
  
  "========================================\n"
  
)


vif(
  
  cox_model_A
  
)


#=========================================================
# MODEL RESULTS
#
# Primary model
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MODEL A: PRIMARY COX MODEL\n"
  
)


cat(
  
  "========================================\n"
  
)


summary(
  
  cox_model_A
  
)


#=========================================================
# Alternative model
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MODEL B: PREVIOUS TRANSMISSION BURDEN\n"
  
)


cat(
  
  "========================================\n"
  
)


summary(
  
  cox_model_B
  
)


#=========================================================
# Reduced model
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MODEL C: REDUCED MODEL\n"
  
)


cat(
  
  "========================================\n"
  
)


summary(
  
  cox_model_C
  
)


#=========================================================
# PROPORTIONAL HAZARDS ASSUMPTION
#
# Primary model
#=========================================================

ph_A <-
  
  cox.zph(
    
    cox_model_A
    
  )


cat(
  
  "\n========================================\n"
  
)


cat(
  
  "PROPORTIONAL HAZARDS TEST: MODEL A\n"
  
)


cat(
  
  "========================================\n"
  
)


print(
  
  ph_A
  
)


#=========================================================
# Proportional hazards assumption
#
# Alternative Model B
#=========================================================

ph_B <-
  
  cox.zph(
    
    cox_model_B
    
  )


cat(
  
  "\n========================================\n"
  
)


cat(
  
  "PROPORTIONAL HAZARDS TEST: MODEL B\n"
  
)


cat(
  
  "========================================\n"
  
)


print(
  
  ph_B
  
)


#=========================================================
# SCALED SCHOENFELD RESIDUAL PLOTS
#
# Primary Model A
#=========================================================

ph_A <-
  
  cox.zph(
    
    cox_model_A
    
  )


png(
  
  "Supplementary_Figure_S1_Schoenfeld_Model_A.png",
  
  width =
    2400,
  
  height =
    1800,
  
  res =
    300
  
)


par(
  
  mfrow =
    c(3, 2)
  
)


plot(
  
  ph_A,
  
  main =
    ""
  
)


dev.off()



plot(
  
  ph_A,
  
  var =
    "calendar_period"
  
)


#=========================================================
# SENSITIVITY ANALYSIS 1
#
# COX MODEL WITH CLUSTER-ROBUST
# STANDARD ERRORS
#
# Accounts for repeated SIAs within regions
#=========================================================

cox_cluster <-
  
  coxph(
    
    Surv(time, event) ~
      
      previous_outbreaks_12m +
      
      births_1000 +
      
      region_type +
      
      calendar_period +
      
      campaign_duration_days +
      
      mcv1_10pct +
      
      cluster(
        
        admin1
        
      ),
    
    data =
      survival_data
    
  )


cat(
  
  "\n========================================\n"
  
)


cat(
  
  "CLUSTER-ROBUST COX MODEL\n"
  
)


cat(
  
  "========================================\n"
  
)


summary(
  
  cox_cluster
  
)


#=========================================================
# SENSITIVITY ANALYSIS 2
#
# SHARED GAMMA FRAILTY COX MODEL
#
# Accounts for unobserved regional heterogeneity
#=========================================================

cox_frailty <-
  
  coxph(
    
    Surv(time, event) ~
      
      previous_outbreaks_12m +
      
      births_1000 +
      
      region_type +
      
      calendar_period +
      
      campaign_duration_days +
      
      mcv1_10pct +
      
      frailty(
        
        admin1,
        
        distribution =
          "gamma"
        
      ),
    
    data =
      survival_data,
    
    control =
      
      coxph.control(
        
        iter.max =
          100,
        
        eps =
          1e-09
        
      )
    
  )


cat(
  
  "\n========================================\n"
  
)


cat(
  
  "SHARED GAMMA FRAILTY COX MODEL\n"
  
)


cat(
  
  "========================================\n"
  
)


summary(
  
  cox_frailty
  
)


#=========================================================
# ANALYSIS COMPLETE
#=========================================================

cat(
  
  "\n========================================\n"
  
)


cat(
  
  "MULTIVARIABLE COX REGRESSION COMPLETE\n"
  
)


cat(
  
  "========================================\n"
  
)
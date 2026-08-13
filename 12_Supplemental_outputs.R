
#=========================================================
# Supplementary Table S1
# Characteristics of the births and routine immunization dataset
#=========================================================

library(dplyr)
library(tibble)
library(knitr)

table_s1 <- tibble(
  
  Characteristic = c(
    "Study period",
    "Administrative regions",
    "Weekly observations",
    "Births, median (IQR)",
    "MCV1 coverage, median (IQR)",
    "MCV2 coverage, median (IQR)",
    "MCV1 coverage, range",
    "MCV2 coverage, range"
  ),
  
  Summary = c(
    
    paste(
      format(min(birth_ri_weekly$week_start), "%Y"),
      "-",
      format(max(birth_ri_weekly$week_start), "%Y")
    ),
    
    n_distinct(birth_ri_weekly$admin1),
    
    nrow(birth_ri_weekly),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      median(birth_ri_weekly$births, na.rm=TRUE),
      quantile(birth_ri_weekly$births,0.25,na.rm=TRUE),
      quantile(birth_ri_weekly$births,0.75,na.rm=TRUE)
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      median(birth_ri_weekly$mcv1,na.rm=TRUE),
      quantile(birth_ri_weekly$mcv1,0.25,na.rm=TRUE),
      quantile(birth_ri_weekly$mcv1,0.75,na.rm=TRUE)
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      median(birth_ri_weekly$mcv2,na.rm=TRUE),
      quantile(birth_ri_weekly$mcv2,0.25,na.rm=TRUE),
      quantile(birth_ri_weekly$mcv2,0.75,na.rm=TRUE)
    ),
    
    sprintf(
      "%.3f–%.3f",
      min(birth_ri_weekly$mcv1,na.rm=TRUE),
      max(birth_ri_weekly$mcv1,na.rm=TRUE)
    ),
    
    sprintf(
      "%.3f–%.3f",
      min(birth_ri_weekly$mcv2,na.rm=TRUE),
      max(birth_ri_weekly$mcv2,na.rm=TRUE)
    )
    
  )
  
)

kable(
  table_s1,
  caption = "Supplementary Table S1. Characteristics of the weekly births and routine immunization dataset."
)


#=========================================================
# Supplementary Table S2
# Univariable Cox proportional hazards models
#=========================================================

library(survival)
library(broom)
library(dplyr)
library(purrr)
library(knitr)

candidate_variables <- c(
  
  "births_1000",
  
  "mcv1",
  "mcv2",
  
  "region_type",
  
  "campaign_duration_days",
  
  "age_target",
  
  "target_upper_age_years",
  
  "doses",
  
  "previous_outbreaks_12m",
  "previous_cases_12m",
  
  "previous_outbreak_size",
  "previous_outbreak_duration"
  
)

univariable_results <-
  
  map_dfr(
    
    candidate_variables,
    
    function(x){
      
      fit <- coxph(
        
        as.formula(
          paste(
            "Surv(time,event) ~",
            x
          )
        ),
        
        data = analytic_data
        
      )
      
      tidy(
        fit,
        exponentiate = TRUE,
        conf.int = TRUE
      ) %>%
        
        mutate(
          Variable = x
        )
      
    }
    
  )

table_s2 <-
  
  univariable_results %>%
  
  select(
    
    Variable,
    
    term,
    
    estimate,
    
    conf.low,
    
    conf.high,
    
    p.value
    
  ) %>%
  
  mutate(
    
    HR_CI = sprintf(
      "%.2f (%.2f–%.2f)",
      estimate,
      conf.low,
      conf.high
    ),
    
    `P value` =
      ifelse(
        p.value<0.001,
        "<0.001",
        sprintf("%.3f",p.value)
      )
    
  ) %>%
  
  select(
    
    Variable,
    
    term,
    
    HR_CI,
    
    `P value`
    
  )

kable(
  table_s2,
  caption="Supplementary Table S2. Univariable Cox proportional hazards models for factors associated with time to outbreak recurrence."
)


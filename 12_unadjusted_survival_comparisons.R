#=========================================================
# 12_unadjusted_survival_comparisons.R
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
# Function for Kaplan-Meier comparison
#=========================================================

run_km_comparison <- function(
    
  data,
  variable,
  variable_label,
  file_prefix
  
) {
  
  
  #=======================================================
  # Create formula
  #=======================================================
  
  km_formula <-
    
    as.formula(
      
      paste0(
        
        "Surv(time, event) ~ ",
        
        variable
        
      )
      
    )
  
  
  #=======================================================
  # Kaplan-Meier model
  #=======================================================
  
  km_fit <-
    
    do.call(
      
      survfit,
      
      list(
        
        formula =
          km_formula,
        
        data =
          data
        
      )
      
    )
  
  
  #=======================================================
  # Log-rank test
  #=======================================================
  
  logrank_fit <-
    
    do.call(
      
      survdiff,
      
      list(
        
        formula =
          km_formula,
        
        data =
          data
        
      )
      
    )
  
  
  #=======================================================
  # Calculate p-value
  #=======================================================
  
  p_value <-
    
    pchisq(
      
      logrank_fit$chisq,
      
      df =
        length(
          logrank_fit$n
        ) - 1,
      
      lower.tail =
        FALSE
      
    )
  
  #=======================================================
  # Format p-value for plot
  #=======================================================
  
  p_label <-
    
    if (p_value < 0.001) {
      
      "p < 0.001"
      
    } else {
      
      paste0(
        "p = ",
        sprintf("%.3f", p_value)
      )
      
    }
  
  
  #=======================================================
  # Print results
  #=======================================================
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    variable_label,
    "\n"
  )
  
  cat(
    "========================================\n"
  )
  
  
  cat(
    "\nLog-rank test:\n"
  )
  
  print(
    logrank_fit
  )
  
  
  cat(
    "\nP-value:",
    p_value,
    "\n"
  )
  
  
  #=======================================================
  # Kaplan-Meier plot
  #=======================================================
  
  km_plot <-
    
    ggsurvplot(
      
      fit =
        km_fit,
      
      data =
        data,
      
      risk.table =
        TRUE,
      
      conf.int =
        TRUE,
      
      pval =
        p_label,
      
      xlab =
        "Days after supplementary immunization activity",
      
      ylab =
        "Probability of remaining outbreak-free",
      
      legend.title =
        variable_label,
      
      ggtheme =
        theme_minimal()
      
    )
  
  
  #=======================================================
  # Print plot
  #=======================================================
  
  print(
    km_plot
  )
  
  
  #=======================================================
  # Save figure
  #=======================================================
  
  ggsave(
    
    paste0(
      
      file_prefix,
      
      "_km_plot.png"
      
    ),
    
    plot =
      km_plot$plot,
    
    width =
      8,
    
    height =
      6,
    
    dpi =
      300
    
  )
  
  
  #=======================================================
  # Save risk table
  #=======================================================
  
  ggsave(
    
    paste0(
      
      file_prefix,
      
      "_risk_table.png"
      
    ),
    
    plot =
      km_plot$table,
    
    width =
      8,
    
    height =
      3,
    
    dpi =
      300
    
  )
  
  
  #=======================================================
  # Return results
  #=======================================================
  
  return(
    
    list(
      
      km_fit =
        km_fit,
      
      logrank =
        logrank_fit,
      
      p_value =
        p_value,
      
      plot =
        km_plot
      
    )
    
  )
  
}


#=========================================================
# 1. Region type
#=========================================================

km_region_type <-
  
  run_km_comparison(
    
    data =
      survival_data,
    
    variable =
      "region_type",
    
    variable_label =
      "Region type",
    
    file_prefix =
      "km_region_type"
    
  )


#=========================================================
# 2. Previous outbreak history
#=========================================================

km_previous_outbreak <-
  
  run_km_comparison(
    
    data =
      survival_data,
    
    variable =
      "prev_outbreak_group",
    
    variable_label =
      "Previous outbreak within 12 months",
    
    file_prefix =
      "km_previous_outbreak"
    
  )


#=========================================================
# 3. Age target
#=========================================================

km_age_target <-
  
  run_km_comparison(
    
    data =
      survival_data,
    
    variable =
      "age_target",
    
    variable_label =
      "SIA target population",
    
    file_prefix =
      "km_age_target"
    
  )


#=========================================================
# 4. Calendar period
#=========================================================

km_calendar_period <-
  
  run_km_comparison(
    
    data =
      survival_data,
    
    variable =
      "calendar_period",
    
    variable_label =
      "Calendar period",
    
    file_prefix =
      "km_calendar_period"
    
  )


#=========================================================
# Summary of log-rank tests
#=========================================================

logrank_summary <-
  
  tibble(
    
    variable =
      c(
        
        "Region type",
        
        "Previous outbreak within 12 months",
        
        "SIA target population",
        
        "Calendar period"
        
      ),
    
    p_value =
      c(
        
        km_region_type$p_value,
        
        km_previous_outbreak$p_value,
        
        km_age_target$p_value,
        
        km_calendar_period$p_value
        
      )
    
  )

cat(
  "\n========================================\n"
)

cat(
  "SUMMARY OF LOG-RANK TESTS\n"
)

cat(
  "========================================\n"
)

print(
  logrank_summary
)

#=========================================================
# Save results
#=========================================================

write_csv(
  
  logrank_summary,
  
  "logrank_test_summary.csv"
  
)

saveRDS(
  
  logrank_summary,
  
  "logrank_test_summary.rds"
  
)

cat(
  "\n========================================\n"
)

cat(
  "UNADJUSTED SURVIVAL COMPARISONS COMPLETE\n"
)

cat(
  "========================================\n"
)


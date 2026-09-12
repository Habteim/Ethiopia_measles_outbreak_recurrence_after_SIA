#=========================================================
# 09_descriptive_analysis.R
#
# Descriptive analysis of SIAs and post-SIA outbreaks
#=========================================================

library(dplyr)
library(gtsummary)


#=========================================================
# 1. DATA PREPARATION
#=========================================================

table_data <- analytic_data %>%
  
  mutate(
    
    # Outcome variables
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
    
    # Categorical predictors
    region_type = factor(region_type),
    age_group = factor(age_group),
    age_target = factor(age_target),
    
    # Previous outbreak history
    prev_outbreak_group = factor(
      case_when(
        previous_outbreaks_12m == 0 ~
          "No outbreak",
        
        previous_outbreaks_12m >= 1 ~
          "≥1 outbreak",
        
        TRUE ~ NA_character_
      ),
      
      levels = c(
        "No outbreak",
        "≥1 outbreak"
      )
    ),
    
    # Coverage presented as percentages
    mcv1_pct = mcv1 * 100,
    mcv2_pct = mcv2 * 100
    
  )


#=========================================================
# 2. OVERALL ANALYTIC SAMPLE AND OUTCOME SUMMARY
#=========================================================

overall_summary <- tibble(
  
  Characteristic = c(
    "Number of SIAs",
    "Administrative regions",
    "Study period",
    "Outbreak within 6 months, n (%)",
    "Outbreak within 12 months, n (%)",
    "Outbreak within 24 months, n (%)",
    "Time to next outbreak, median (IQR), days"
  ),
  
  Value = c(
    
    nrow(table_data),
    
    n_distinct(table_data$admin1),
    
    paste0(
      min(table_data$sia_year, na.rm = TRUE),
      "–",
      max(table_data$sia_year, na.rm = TRUE)
    ),
    
    paste0(
      sum(table_data$outbreak_within_6m == "Yes",
          na.rm = TRUE),
      " (",
      round(
        mean(
          table_data$outbreak_within_6m == "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
      "%)"
    ),
    
    paste0(
      sum(table_data$outbreak_within_12m == "Yes",
          na.rm = TRUE),
      " (",
      round(
        mean(
          table_data$outbreak_within_12m == "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
      "%)"
    ),
    
    paste0(
      sum(table_data$outbreak_within_24m == "Yes",
          na.rm = TRUE),
      " (",
      round(
        mean(
          table_data$outbreak_within_24m == "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
      "%)"
    ),
    
    paste0(
      round(
        median(
          table_data$time_to_next_outbreak_days,
          na.rm = TRUE
        ),
        0
      ),
      " (",
      round(
        quantile(
          table_data$time_to_next_outbreak_days,
          0.25,
          na.rm = TRUE
        ),
        0
      ),
      "–",
      round(
        quantile(
          table_data$time_to_next_outbreak_days,
          0.75,
          na.rm = TRUE
        ),
        0
      ),
      ")"
    )
    
  )
)

overall_summary


#=========================================================
# 3. MAIN DESCRIPTIVE TABLE
#
# SIA characteristics stratified by
# post-SIA outbreak status
#=========================================================

table_data <- analytic_data %>%
  
  mutate(
    
    outbreak_after_sia = factor(
      outbreak_after_sia,
      levels = c(0, 1),
      labels = c("No outbreak", "Outbreak")
    ),
    
    region_type = factor(region_type),
    
    age_group = factor(age_group),
    
    age_target = factor(age_target),
    
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
    
    mcv1_pct = mcv1 * 100,
    
    mcv2_pct = mcv2 * 100
    
  )


#---------------------------------------------------------
# Table 1
#---------------------------------------------------------

table1_sia <- table_data %>%
  
  select(
    
    outbreak_after_sia,
    
    # SIA characteristics
    sia_year,
    region_type,
    age_group,
    age_target,
    target_upper_age_years,
    campaign_duration_days,
    
    # Population and vaccination
    births,
    doses,
    mcv1_pct,
    mcv2_pct,
    
    # Pre-SIA outbreak history
    previous_outbreaks_12m,
    previous_cases_12m,
    previous_outbreak_size,
    previous_outbreak_duration,
    
    # Fixed follow-up outcomes
    outbreak_within_6m,
    outbreak_within_12m,
    outbreak_within_24m
    
  ) %>%
  
  tbl_summary(
    
    by = outbreak_after_sia,
    
    statistic = list(
      
      all_continuous() ~
        "{median} ({p25}–{p75})",
      
      all_categorical() ~
        "{n} ({p}%)"
      
    ),
    
    digits = list(
      
      sia_year ~ 0,
      target_upper_age_years ~ 0,
      campaign_duration_days ~ 0,
      
      births ~ 0,
      doses ~ 0,
      
      mcv1_pct ~ 1,
      mcv2_pct ~ 1,
      
      previous_outbreaks_12m ~ 0,
      previous_cases_12m ~ 0,
      previous_outbreak_size ~ 0,
      previous_outbreak_duration ~ 0
      
    ),
    
    label = list(
      
      sia_year ~
        "SIA year",
      
      region_type ~
        "Region type",
      
      age_group ~
        "Campaign age group",
      
      age_target ~
        "Age-target category",
      
      target_upper_age_years ~
        "Upper target age, years",
      
      campaign_duration_days ~
        "Campaign duration, days",
      
      births ~
        "Estimated births",
      
      doses ~
        "Doses administered",
      
      mcv1_pct ~
        "MCV1 coverage, %",
      
      mcv2_pct ~
        "MCV2 coverage, %",
      
      previous_outbreaks_12m ~
        "Outbreaks in preceding 12 months",
      
      previous_cases_12m ~
        "Measles cases in preceding 12 months",
      
      previous_outbreak_size ~
        "Size of most recent previous outbreak",
      
      previous_outbreak_duration ~
        "Duration of most recent previous outbreak, weeks",
      
      outbreak_within_6m ~
        "Outbreak within 6 months",
      
      outbreak_within_12m ~
        "Outbreak within 12 months",
      
      outbreak_within_24m ~
        "Outbreak within 24 months"
      
    ),
    
    missing = "no"
    
  ) %>%
  
  add_overall(
    last = FALSE
  ) %>%
  
  add_p(
    
    test = list(
      
      all_continuous() ~ "wilcox.test",
      
      all_categorical() ~ "fisher.test"
      
    ),
    
    pvalue_fun =
      label_style_pvalue(
        digits = 3
      )
    
  ) %>%
  
  add_n() %>%
  
  modify_header(
    
    label ~ "**Characteristic**",
    
    n ~ "**N**",
    
    all_stat_cols() ~ "**{level}**",
    
    p.value ~ "**p-value**"
    
  ) %>%
  
  bold_labels()


table1_sia

#=========================================================
# Characteristics of subsequent outbreaks
# Among SIAs followed by an outbreak
#=========================================================

subsequent_outbreak_summary <- table_data %>%
  
  filter(
    outbreak_after_sia == "Outbreak"
  ) %>%
  
  select(
    time_to_next_outbreak_days,
    next_outbreak_size,
    next_outbreak_duration
  ) %>%
  
  tbl_summary(
    
    statistic =
      all_continuous() ~
      "{median} ({p25}–{p75})",
    
    digits =
      all_continuous() ~ 0,
    
    label = list(
      
      time_to_next_outbreak_days ~
        "Time to next outbreak, days",
      
      next_outbreak_size ~
        "Size of subsequent outbreak, cases",
      
      next_outbreak_duration ~
        "Duration of subsequent outbreak, weeks"
      
    ),
    
    missing = "no"
    
  ) %>%
  
  bold_labels()


subsequent_outbreak_summary


#=========================================================
# 4. REGIONAL DESCRIPTION OF POST-SIA OUTCOMES
#=========================================================

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


table_region <- table_data %>%
  
  group_by(admin1) %>%
  
  summarise(
    
    `Number of SIAs` =
      n(),
    
    `Median days to next outbreak` =
      round(
        median(
          time_to_next_outbreak_days,
          na.rm = TRUE
        ),
        0
      ),
    
    `Outbreak within 6 months (%)` =
      round(
        mean(
          outbreak_within_6m == "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    `Outbreak within 12 months (%)` =
      round(
        mean(
          outbreak_within_12m == "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    `SIAs with ≥1 outbreak in preceding 12 months (%)` =
      round(
        mean(
          previous_outbreaks_12m > 0,
          na.rm = TRUE
        ) * 100,
        1
      )
    
  ) %>%
  
  mutate(
    
    admin1 =
      factor(
        admin1,
        levels = region_order
      )
    
  ) %>%
  
  arrange(admin1)


table_region


#=========================================================
# 5. OVERALL OUTBREAK CHARACTERISTICS
#=========================================================

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
    
    paste0(
      min(
        outbreaks$outbreak_year,
        na.rm = TRUE
      ),
      "–",
      max(
        outbreaks$outbreak_year,
        na.rm = TRUE
      )
    ),
    
    paste0(
      round(
        median(
          outbreaks$duration_weeks,
          na.rm = TRUE
        ),
        1
      ),
      " (",
      round(
        quantile(
          outbreaks$duration_weeks,
          0.25,
          na.rm = TRUE
        ),
        1
      ),
      "–",
      round(
        quantile(
          outbreaks$duration_weeks,
          0.75,
          na.rm = TRUE
        ),
        1
      ),
      ")"
    ),
    
    paste0(
      round(
        median(
          outbreaks$total_cases,
          na.rm = TRUE
        ),
        0
      ),
      " (",
      round(
        quantile(
          outbreaks$total_cases,
          0.25,
          na.rm = TRUE
        ),
        0
      ),
      "–",
      round(
        quantile(
          outbreaks$total_cases,
          0.75,
          na.rm = TRUE
        ),
        0
      ),
      ")"
    ),
    
    paste0(
      round(
        median(
          outbreaks$peak_cases,
          na.rm = TRUE
        ),
        0
      ),
      " (",
      round(
        quantile(
          outbreaks$peak_cases,
          0.25,
          na.rm = TRUE
        ),
        0
      ),
      "–",
      round(
        quantile(
          outbreaks$peak_cases,
          0.75,
          na.rm = TRUE
        ),
        0
      ),
      ")"
    ),
    
    paste0(
      round(
        median(
          outbreaks$mean_weekly_cases,
          na.rm = TRUE
        ),
        1
      ),
      " (",
      round(
        quantile(
          outbreaks$mean_weekly_cases,
          0.25,
          na.rm = TRUE
        ),
        1
      ),
      "–",
      round(
        quantile(
          outbreaks$mean_weekly_cases,
          0.75,
          na.rm = TRUE
        ),
        1
      ),
      ")"
    ),
    
    paste0(
      round(
        median(
          outbreaks$recurrence_days,
          na.rm = TRUE
        ),
        0
      ),
      " (",
      round(
        quantile(
          outbreaks$recurrence_days,
          0.25,
          na.rm = TRUE
        ),
        0
      ),
      "–",
      round(
        quantile(
          outbreaks$recurrence_days,
          0.75,
          na.rm = TRUE
        ),
        0
      ),
      ")"
    )
    
  )
)


outbreak_summary


#=========================================================
# 6. BIRTHS AND ROUTINE IMMUNIZATION DATA SUMMARY
#=========================================================

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
    
    n_distinct(
      birth_ri_weekly$admin1
    ),
    
    nrow(
      birth_ri_weekly
    ),
    
    sprintf(
      "%.0f (%.0f–%.0f)",
      
      median(
        birth_ri_weekly$births,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$births,
        0.25,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$births,
        0.75,
        na.rm = TRUE
      )
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      
      median(
        birth_ri_weekly$mcv1,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$mcv1,
        0.25,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$mcv1,
        0.75,
        na.rm = TRUE
      )
    ),
    
    sprintf(
      "%.3f (%.3f–%.3f)",
      
      median(
        birth_ri_weekly$mcv2,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$mcv2,
        0.25,
        na.rm = TRUE
      ),
      
      quantile(
        birth_ri_weekly$mcv2,
        0.75,
        na.rm = TRUE
      )
    ),
    
    sprintf(
      "%.3f–%.3f",
      
      min(
        birth_ri_weekly$mcv1,
        na.rm = TRUE
      ),
      
      max(
        birth_ri_weekly$mcv1,
        na.rm = TRUE
      )
    ),
    
    sprintf(
      "%.3f–%.3f",
      
      min(
        birth_ri_weekly$mcv2,
        na.rm = TRUE
      ),
      
      max(
        birth_ri_weekly$mcv2,
        na.rm = TRUE
      )
    )
    
  )
)


birth_ri_summary


#=========================================================
# 7. Post-SIA outcomes by age target
#=========================================================

age_target_summary <- table_data %>%
  
  group_by(age_target) %>%
  
  summarise(
    
    n_sias =
      n(),
    
    
    post_sia_outbreak =
      sum(
        outbreak_after_sia ==
          "Post-SIA outbreak",
        na.rm = TRUE
      ),
    
    
    post_sia_outbreak_pct =
      round(
        mean(
          outbreak_after_sia ==
            "Post-SIA outbreak",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    outbreak_6m_pct =
      round(
        mean(
          outbreak_within_6m ==
            "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    outbreak_12m_pct =
      round(
        mean(
          outbreak_within_12m ==
            "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    median_time_to_outbreak =
      median(
        time_to_next_outbreak_days,
        na.rm = TRUE
      )
    
  ) %>%
  
  arrange(
    desc(post_sia_outbreak_pct)
  )


age_target_summary

#=========================================================
# 8. Post-SIA outcomes by region type
#=========================================================

region_type_summary <- table_data %>%
  
  group_by(region_type) %>%
  
  summarise(
    
    n_sias =
      n(),
    
    
    post_sia_outbreak =
      sum(
        outbreak_after_sia ==
          "Post-SIA outbreak",
        na.rm = TRUE
      ),
    
    
    post_sia_outbreak_pct =
      round(
        mean(
          outbreak_after_sia ==
            "Post-SIA outbreak",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    outbreak_6m_pct =
      round(
        mean(
          outbreak_within_6m ==
            "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    outbreak_12m_pct =
      round(
        mean(
          outbreak_within_12m ==
            "Yes",
          na.rm = TRUE
        ) * 100,
        1
      ),
    
    
    median_time_to_outbreak =
      median(
        time_to_next_outbreak_days,
        na.rm = TRUE
      )
    
  ) %>%
  
  arrange(
    desc(post_sia_outbreak_pct)
  )


region_type_summary

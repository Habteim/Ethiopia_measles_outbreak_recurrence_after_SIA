#Data cleaning, harmonization and transformation for measles line list data set 
#Load libraries 

library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(gtsummary)


#Load the data 
#=================



#Data exploration, missing data analysis and cleaning 
#========================================================
colnames(polio_clean)
skimr::skim(polio_clean)
glimpse(polio_clean)
Hmisc::describe(polio_clean)
plot(Hmisc::describe(polio_clean), which = "categorical")
plot(Hmisc::describe(polio_clean), which = "continuous")

library(naniar)
pct_miss(polio_clean)
pct_miss_case(polio_clean)
pct_complete_case(polio_clean)
gg_miss_var(polio_clean, show_pct = TRUE) +
  theme_minimal() +
  labs(x = "Variable", y = "% of missing values")

gg_miss_var(polio_clean, show_pct = TRUE) 

polio_clean %>%
  gg_miss_upset(nsets = 10)

vis_miss(polio_clean)

#Explore missingness between columns/variables 
ggplot(
  data = polio_clean, 
  mapping = aes(x = age_years, y = opv_doses)) + 
  geom_miss_point()

















#Regional data harmonization (Robust Standardize Region and Zone) 
polio_clean <- polio_clean %>%
  mutate(
    # 1. Uniform Formatting (Uppercase for internal logic)
    region = str_to_upper(str_squish(region)), 
    zone   = str_to_upper(str_squish(zone)),
    
    # 2. Map Zones to New Regions (The primary split)
    # We use str_detect to catch zones even if they have extra words (e.g., "GURAGHE ZONE")
    new_region = case_when(
      str_detect(zone, "GURAGHE|HADIYA|SILTI|HALABA|ALABA|YEM|KEBENA|MAREKO") ~ "Central Ethiopia",
      str_detect(zone, "WOLAYTA|WOLAITA|GAMO|GEDEO|SOUTH OMO|KONSO|GOFFA|WEST OMO|AMARO|BURJI|DIRASHE|ALLE|BASKETO") ~ "South Ethiopia",
      str_detect(zone, "KEFA|KAFFA|SHEKA|DAWRO|BENCH SHEKO|BENCH MAJI|KONTA") ~ "South West Ethiopia",
      str_detect(zone, "SIDAMA|HAWASSA|AWASSA") ~ "Sidama",
      TRUE ~ NA_character_  # If no match found, keep it empty for now
    ),
    
    # 3. Final Consolidation
    region = case_when(
      # If we successfully mapped a new region in Step 2, use it
      !is.na(new_region) ~ new_region,
      
      # Fix established regions
      region %in% c("OROMIYA", "OROMIA", "OROMIYAA") ~ "Oromia",
      region %in% c("BENISHANGUL GUMU", "BENISHANGUL GUMUZ", "BENESHANGUL GUMUZ") ~ "Benishangul-Gumuz",
      region %in% c("HARERI", "HARARI") ~ "Harari",
      region == "ADDIS ABABA" ~ "Addis Ababa",
      region == "DIRE DAWA" ~ "Dire Dawa",
      region == "AMHARA" ~ "Amhara",
      region == "AFAR" ~ "Afar",
      region == "GAMBELLA" ~ "Gambella",
      region == "SOMALI" ~ "Somali",
      region == "TIGRAY" ~ "Tigray",
      
      # Handle the "Unspecified" and "Unknown" leftovers
      # If it's still SNNPR or Unknown, we label it "Other/Unassigned" or a specific region
      region %in% c("SNNP", "SNNPR", "UNKNOWN") ~ "South Ethiopia", 
      
      # Default to Title Case for anything else
      TRUE ~ str_to_title(region)
    )
  )
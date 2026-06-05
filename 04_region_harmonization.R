#04_region_harmonization
#======================================

harmonize_region <- function(region){
  
  region <- str_trim(region)
  region <- str_to_title(region)
  
  case_when(
    region %in% c("Benishangul") ~
      "Benishangul Gumuz",
    
    region %in% c("Gambella") ~
      "Gambela",
    
    region %in% c("Dire Dawa") ~
      "Dire Dawa",
    
    TRUE ~ region
  )
}

#then, apply the function to all datasets
measles_data$admin1 <-
  harmonize_region(measles_data$admin1)

birth_ri$admin1 <-
  harmonize_region(birth_ri$admin1)

sia$admin1 <-
  harmonize_region(sia$admin1)


#Master region order: for plotting and other purposes, 
#we want to have a consistent order of regions across datasets. 
#We can create a master list of regions based on the unique values from all datasets, 
#and then use this list to set the factor levels for the admin1 variable in each dataset.

region_levels <- c(
  "Addis Ababa",
  "Afar",
  "Amhara",
  "Benishangul Gumuz",
  "Central Ethiopia",
  "Dire Dawa",
  "Gambela",
  "Harari",
  "Oromia",
  "Sidama",
  "Somali",
  "South Ethiopia",
  "South West Ethiopia",
  "Tigray"
)

#then, apply the factor levels to each data set to ensure a consistent order of regions across all data sets.

measles_data$admin1 <-
  factor(
    measles_data$admin1,
    levels = region_levels
  )

birth_ri$admin1 <-
  factor(
    birth_ri$admin1,
    levels = region_levels
  )

sia$admin1 <-
  factor(
    sia$admin1,
    levels = region_levels
  )


#Conduct validation checks 
setdiff(
  unique(measles_data$admin1),
  region_levels
)

setdiff(
  unique(birth_ri$admin1),
  region_levels
)

setdiff(
  unique(sia$admin1),
  region_levels
)


#Create region metadata table 
region_lookup <- tibble(
  admin1 = c(
    "Addis Ababa",
    "Dire Dawa",
    "Harari",
    "Afar",
    "Somali",
    "Amhara",
    "Benishangul Gumuz",
    "Central Ethiopia",
    "Gambela",
    "Oromia",
    "Sidama",
    "South Ethiopia",
    "South West Ethiopia",
    "Tigray"
  ),
  
  region_type = c(
    "Urban",        # Addis Ababa
    "Urban",        # Dire Dawa
    "Urban",        # Harari
    
    "Pastoralist",  # Afar
    "Pastoralist",  # Somali
    
    "Agrarian",     # Amhara
    "Agrarian",     # Benishangul Gumuz
    "Agrarian",     # Central Ethiopia
    "Agrarian",     # Gambela
    "Agrarian",     # Oromia
    "Agrarian",     # Sidama
    "Agrarian",     # South Ethiopia
    "Agrarian",     # South West Ethiopia
    "Agrarian"      # Tigray
  )
)



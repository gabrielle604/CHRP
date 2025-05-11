# 10 May 20205


# Install required packages if needed
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("spdep", quietly = TRUE)) install.packages("spdep")
if (!requireNamespace("spatialreg", quietly = TRUE)) install.packages("spatialreg")

# Load packages
library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)

# Code for importing the data (adjust as needed)
housing_pharm_prep <- read_csv("housing_pharm_prep.csv")
view(housing_pharm_prep) #extracted from ArcGIS online; from Sean, and has lots of data from MSTUDY
## including, education, and whether 
## they live within 1 mile of a pharmacy

# Explore the data structure
head(housing_pharm_prep)

## column for education level (education)
## column for access to pharmacy (within1mile)

# Clean the data - ensure housing insecurity and access are properly formatted
# Identify the column names for housing insecurity and pharmacy access
# This assumes your columns are named something like "housing_insecurity" and "Join_Count"
# Adjust the column names based on your actual data

table(housing_pharm_prep$education)

# Create a binary access variable if needed
# This assumes "Join_Count" > 0 means there is pharmacy access
housing_pharm_clean <- housing_pharm_prep %>%
  mutate(has_access = ifelse(num_pharm_within1mile_home > 0, "Access", "No Access"))

# Basic statistical tests

# Create contingency table
cont_table <- table(housing_pharm_clean$education, housing_pharm_clean$has_access)
print(cont_table)

# Chi-square test to see if there's association between housing insecurity and pharmacy access
chi_test <- chisq.test(cont_table)
print(chi_test)

# If you have small cell counts, use Fisher's exact test instead
fisher_test <- fisher.test(cont_table)
print(fisher_test)

# Summarize results by group
summary_stats <- housing_pharm_clean %>%
  group_by(education) %>%
  summarize(
    total_count = n(),
    with_access = sum(has_access == "Access"),
    percent_access = round(100 * with_access / total_count, 2)
  )
print(summary_stats)

# Visualize the results
ggplot(housing_pharm_clean, aes(x = education, fill = has_access)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Pharmacy Accessibility by Education Level",
       x = "Education",
       y = "Proportion",
       fill = "Pharmacy Access within 1 mile of Home") +
  theme_minimal()


# Spatial analysis (with coordinates), convert to spatial object first
if(all(c("Lat", "Long") %in% colnames(housing_pharm_clean))) {
  library(sf)
  library(spdep)
  library(ggplot2)
  library(dplyr)
  
  # Ensure using the correct coordinate columns
  data_sf <- st_as_sf(housing_pharm_clean, coords = c("Long", "Lat"), crs = 4326)
  
  # Create spatial weights matrix
  nb <- dnearneigh(st_coordinates(data_sf), d1 = 0, d2 = 5000) # 5km distance threshold
  nb_weights <- nb2listw(nb, style = "W")
  
  # Test for spatial autocorrelation using Moran's I
  moran_test <- moran.test(as.numeric(data_sf$has_access == "Access"), nb_weights)
  print(moran_test)
  
  # Prepare variables for spatial regression
  data_sf$access_num <- as.numeric(data_sf$has_access == "Access")
  data_sf$is_edu <- factor(data_sf$education)  # <-- keep the column name education
  
  # Run spatial lag model
  spatial_lag <- lagsarlm(access_num ~ education, data = data_sf, listw = nb_weights)
  summary(spatial_lag)
  
  # Run spatial error model
  spatial_error <- errorsarlm(access_num ~ education, data = data_sf, listw = nb_weights)
  summary(spatial_error)
  
  # Compare models
  print("AIC for spatial models:")
  AIC(spatial_lag, spatial_error)
  
  # Map the results
  ggplot() +
    geom_sf(data = data_sf, aes(color = has_access)) +
    facet_wrap(~ education) +   # <-- also facet by education
    labs(title = "Spatial Distribution of Pharmacy Access by Education Level") +
    theme_minimal()
}


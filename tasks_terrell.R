# 23 June 2025

## building from file "housing_insecurity_pharm.R"


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


# STEP 1: Pull IDs of people who answered "YES" to experiencing housing insecurity
housing_insecure_ids <- housing_pharm_clean %>%
  filter(is_homeles == "Yes") %>%
  select(id)

# STEP 2: Among those in step 1, please identify participants ID's who live within 
## a pharmacy buffer, and those who live outside a pharmacy buffer. So we will have 
## two columns of IDs for this one. See slide 10. I believe you might need to write 
## code to figure this out...we can see the dots clearly, but I don't know if you 
## have coded these yet.

within_buffer_ids <- housing_pharm_clean %>%
  filter(is_homeles == "Yes", num_pharm_within1mile_home > 0) %>%
  select(id)

outside_buffer_ids <- housing_pharm_clean %>%
  filter(is_homeles == "Yes", num_pharm_within1mile_home == 0) %>%
  select(id)

## alternate approach
housing_pharm_clean <- housing_pharm_clean %>%
  mutate(
    within_pharmacy_buffer_flag = ifelse(num_pharm_within1mile_home > 0, TRUE, FALSE)
  )



# STEP 4:
small_data <- housing_pharm_clean %>%
  filter(is_homeles == "Yes") %>%
  mutate(within_pharmacy_buffer = num_pharm_within1mile_home > 0) %>%
  select(id, race, hispanicit, age, hivstatus, Lat, Long, within_pharmacy_buffer)





library(dplyr)
library(readr)

# Step 1: Filter to housing insecure participants
housing_insecure <- housing_pharm_clean %>%
  filter(is_homeles == "Yes") %>%
  mutate(within_pharmacy_buffer = num_pharm_within1mile_home > 0) %>%
  select(id, race, hispanicit, age, hivstatus, Lat, Long, within_pharmacy_buffer)

# Step 2: Split into two datasets
within_buffer <- housing_insecure %>%
  filter(within_pharmacy_buffer == TRUE)

outside_buffer <- housing_insecure %>%
  filter(within_pharmacy_buffer == FALSE)

# Step 3: Export to CSV
write_csv(within_buffer, "housing_insecure_within_buffer.csv")
write_csv(outside_buffer, "housing_insecure_outside_buffer.csv")


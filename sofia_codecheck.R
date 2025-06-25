## -------------------------------------------------------------------------
## Pharmacy-access × Housing-insecurity analysis
## Original: 26 Apr & 23 Jun 2025 | Updated: 24 Jun 2025 (adds sofiaid)
## -------------------------------------------------------------------------

## 0. Setup ----------------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

pkgs_spatial <- c("sf", "spdep", "spatialreg")
to_install <- pkgs_spatial[!pkgs_spatial %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install)
lapply(pkgs_spatial, library, character.only = TRUE)

## 1. Load & inspect data --------------------------------------------------
#setwd("~/Desktop")
housing_pharm_prep <- read_csv("housing_pharm_prep.csv")
view(housing_pharm_prep)
# Add a unique ID column
housing_pharm_prep <- housing_pharm_prep %>%
  mutate(sofiaid = row_number())

glimpse(housing_pharm_prep)

## 2. Clean / recode -------------------------------------------------------
housing_pharm_clean <- housing_pharm_prep %>%
  rename(num_pharm_within1mile_home = Join_Count) %>%
  mutate(
    has_access = if_else(num_pharm_within1mile_home > 0, "Access", "No Access"),
    within_pharmacy_buffer_flag = num_pharm_within1mile_home > 0
  )

## 3. Contingency analysis -------------------------------------------------
cont_table <- table(housing_pharm_clean$is_homeles, housing_pharm_clean$has_access)
chi_test <- chisq.test(cont_table)
fisher_test <- fisher.test(cont_table)

print(cont_table)
print(chi_test)
print(fisher_test)

summary_stats <- housing_pharm_clean %>%
  group_by(is_homeles) %>%
  summarise(
    total_count = n(),
    with_access = sum(has_access == "Access"),
    percent_access = round(100 * with_access / total_count, 2)
  )
print(summary_stats)

## 4. Visualization example ------------------------------------------------
ggplot(housing_pharm_clean, aes(is_homeles, fill = has_access)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Pharmacy Accessibility by Housing-Insecurity Status",
    x = "Housing-insecure?",
    y = "Proportion",
    fill = "Pharmacy within 1 mile"
  ) +
  theme_minimal()


## 6. Age distribution -----------------------------------------------------
ggplot(housing_pharm_clean, aes(age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

ggplot(housing_pharm_clean, aes(has_access, age)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Age by Access Group", x = "Access Group", y = "Age") +
  theme_minimal()

## 7. Housing-insecure subset & CSV export --------------------------------
housing_insecure <- housing_pharm_clean %>%
  filter(is_homeles == "Yes") %>%
  mutate(within_pharmacy_buffer = num_pharm_within1mile_home > 0) %>%
  select(sofiaid, id, type, race, hispanicit, age, hivstatus,
         Lat, Long, within_pharmacy_buffer)

within_buffer <- housing_insecure %>%
  filter(within_pharmacy_buffer)

outside_buffer <- housing_insecure %>%
  filter(!within_pharmacy_buffer)

write_csv(within_buffer, "housing_insecure_within_buffer.csv")
write_csv(outside_buffer, "housing_insecure_outside_buffer.csv")


## -------------------------------------------------------------------------
## Histograms (bar charts) of `type`
## -------------------------------------------------------------------------

library(stringr)

# Wrap long labels so axis text doesn’t spill off the plot
within_buffer  <- within_buffer  %>% mutate(type_wrapped = str_wrap(type, 15))
outside_buffer <- outside_buffer %>% mutate(type_wrapped = str_wrap(type, 15))

p_within  <- ggplot(within_buffer,  aes(type_wrapped)) +
  geom_bar(fill = "#3182bd") +
  labs(title = "Type distribution (within pharmacy buffer)",
       x = "Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_outside <- ggplot(outside_buffer, aes(type_wrapped)) +
  geom_bar(fill = "#de2d26") +
  labs(title = "Type distribution (outside pharmacy buffer)",
       x = "Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display one after the other (or combine with patchwork/cowplot if preferred)
print(p_within)
print(p_outside)



## -------------------------------------------------------------------------
## Histograms (bar charts) of `unique identifier`
## -------------------------------------------------------------------------

library(stringr)

# Wrap long labels so axis text doesn’t spill off the plot
within_buffer  <- within_buffer  %>% mutate(sofiaid_wrapped = str_wrap(sofiaid, 15))
outside_buffer <- outside_buffer %>% mutate(sofiaid_wrapped = str_wrap(sofiaid, 15))

p_within  <- ggplot(within_buffer,  aes(sofiaid_wrapped)) +
  geom_bar(fill = "#3182bd") +
  labs(title = "Duplicate distribution (within pharmacy buffer)",
       x = "sofiaid", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_outside <- ggplot(outside_buffer, aes(sofiaid_wrapped)) +
  geom_bar(fill = "#de2d26") +
  labs(title = "Duplicate distribution (outside pharmacy buffer)",
       x = "sofiaid", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display one after the other (or combine with patchwork/cowplot if preferred)
print(p_within)
print(p_outside)

## -------------------------------------------------------------------------
## End of script
## -----
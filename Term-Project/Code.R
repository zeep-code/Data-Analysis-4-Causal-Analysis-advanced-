rm(list=ls())

# Load required libraries
library(readxl)
library(dplyr)
library(purrr)
library(tidyverse)
library(fixest)
library(modelsummary)
library(ggplot2)
library(plm) 
library(tidyr)
library(patchwork)

# Define file path (replace with your actual file path)
file_path <- "E:/CEU/Winter/DA4/Term_Project_A3/CustomQuery.xlsx"

# Function to read and merge data with improved error handling
load_and_merge_data <- function(file_path) {
  sheets <- c("Corruption", "Firm Profile", "Regulations", 
              "Workforce", "Management Practices", "Informality")  # ADDED Informality
  
  read_sheet <- function(sheet_name) {
    col_names <- read_excel(file_path, sheet = sheet_name, n_max = 0) %>% names()
    read_excel(file_path, sheet = sheet_name, skip = 8, 
               col_names = FALSE, na = c("n.a.", "NA", "")) %>% 
      setNames(col_names)
  }
  
  data_list <- lapply(sheets, read_sheet)
  common_cols <- c("Economy", "Year", "Subgroup", "Top Subgroup Level", 
                   "Subgroup Level", "Average/SE/N")
  
  merged_data <- reduce(data_list, 
                        function(x, y) full_join(x, y, by = common_cols, 
                                                 relationship = "many-to-many")) %>%
    group_by(across(all_of(common_cols))) %>%
    summarize(across(everything(), ~ first(na.omit(.x))), .groups = "drop") %>%
    ungroup()
  
  return(merged_data)
}

# Load and process data
merged_data <- load_and_merge_data(file_path) %>%
  mutate(
    Year = as.numeric(Year),
    across(-c(Economy, Subgroup, `Top Subgroup Level`, 
              `Subgroup Level`, `Average/SE/N`), as.numeric)
  )

# Save merged dataset
write_csv(merged_data, "merged_dataset.csv")

# Check if informal competition column exists
if ("Percent of firms competing against unregistered or informal firms" %in% colnames(merged_data)) {
  informal_col <- "Percent of firms competing against unregistered or informal firms"
} else {
  informal_col <- "Percent of firms identifying practices of competitors in the informal sector as a major or very severe constraint"
}

clean_data <- merged_data %>%
  mutate(
    Year = as.numeric(as.character(Year)),  # Ensure Year is numeric
    productivity = as.numeric(`Real annual labor productivity growth (%)`),
    bribery = as.numeric(`Bribery incidence (percent of firms experiencing at least one bribe payment request)`),
    corruption_obstacle = as.numeric(`Percent of firms identifying corruption as a major or very severe constraint`),
    informal_competition = as.numeric(.data[[informal_col]]),
    management_quality = as.numeric(`Management practices index`),
    female_ownership = NA,
    export_activity = NA,
    firm_size = case_when(
      `Subgroup Level` == "Small (5-19)" ~ "Small",
      `Subgroup Level` == "Medium (20-99)" ~ "Medium",
      `Subgroup Level` == "Large (100+)" ~ "Large",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(Year),  # Ensure there are no missing years
    Year >= 2006 & Year <= 2021,  # Use explicit condition to filter correctly
    !is.na(productivity)  # Ensure productivity is not missing
  )



# Print available column names
colnames(merged_data)

# ---- Data Exploration ----
# Summary statistics
summary_stats <- clean_data %>%
  select(productivity, bribery, informal_competition, corruption_obstacle, 
         management_quality, female_ownership, export_activity) %>%
  summary()

print(summary_stats)

# Check data availability 
data_availability <- clean_data %>%
  group_by(Economy, Year) %>%
  summarise(
    has_productivity = sum(!is.na(productivity)) > 0,
    has_bribery = sum(!is.na(bribery)) > 0,
    observations = n()
  ) %>%
  arrange(Economy, Year)

print(data_availability)

# ---- Panel Data Preparation ----
economy_years <- clean_data %>%
  filter(!is.na(productivity), !is.na(bribery)) %>%
  group_by(Economy) %>%
  summarise(
    years = n_distinct(Year),
    first_year = min(Year),
    last_year = max(Year)
  ) %>%
  filter(years >= 2) %>%
  arrange(desc(years))

print(economy_years)
panel_data <- clean_data %>%
  filter(Economy %in% economy_years$Economy) %>%
  filter(!is.na(productivity), !is.na(bribery)) %>%
  mutate(
    panel_id = paste(Economy, firm_size, `Subgroup Level`, `Top Subgroup Level`, sep = "_"),  # More unique ID
    high_bribery = ifelse(bribery > median(bribery, na.rm = TRUE), 1, 0)
  ) %>%
  distinct(panel_id, Year, .keep_all = TRUE)  # Remove duplicates

pdata <- pdata.frame(panel_data, index = c("panel_id", "Year"))


# ---- Fixed and Random Effects Models ----
# Modify the fixed effects model to only use variables with sufficient data
fe_model <- plm(productivity ~ bribery + informal_competition, 
                data = pdata, model = "within")

# Similarly for the random effects model
re_model <- plm(productivity ~ bribery + informal_competition, 
                data = pdata, model = "random")

hausman_test <- phtest(fe_model, re_model)

print(summary(fe_model))
print(summary(re_model))
print(hausman_test)

# ---- DiD Analysis ----
panel_counts <- panel_data %>%
  group_by(panel_id) %>%
  summarize(n_years = n_distinct(Year))

balanced_panels <- panel_counts %>%
  filter(n_years >= 2) %>%
  pull(panel_id)

did_data <- panel_data %>%
  filter(panel_id %in% balanced_panels)

panel_years <- did_data %>%
  group_by(panel_id) %>%
  summarize(min_year = min(Year), max_year = max(Year))

did_data <- did_data %>%
  left_join(panel_years, by = "panel_id") %>%
  mutate(
    post = ifelse(Year > min_year, 1, 0),
    initial_bribery = ifelse(Year == min_year, bribery, NA)
  ) %>%
  group_by(panel_id) %>%
  mutate(
    initial_bribery = max(initial_bribery, na.rm = TRUE),
    high_initial_bribery = ifelse(initial_bribery > median(panel_data$bribery, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()

cont_did_model <- feols(productivity ~ initial_bribery:post | panel_id + Year, data = did_data, vcov = "hetero")
binary_did_model <- feols(productivity ~ high_initial_bribery:post | panel_id + Year, data = did_data, vcov = "hetero")

print(summary(cont_did_model))
print(summary(binary_did_model))

# ---- Save Results ----
saveRDS(list(
  data = panel_data,
  models = list(
    "FE" = fe_model,
    "RE" = re_model,
    "DiD (Continuous)" = cont_did_model,
    "DiD (Binary)" = binary_did_model
  ),
  diagnostics = list(hausman_test)
), "corruption_productivity_analysis_results.RDS")




# ---- Figure 1: Relationship between Bribery and Productivity by Firm Size ----
# This code would use your panel_data dataframe created in your initial analysis

scatter_plot <- ggplot(panel_data, aes(x = bribery, y = productivity, color = firm_size)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linetype = "solid") +
  labs(title = "Relationship Between Bribery and Productivity",
       subtitle = "By Firm Size (2006-2021)",
       x = "Bribery Incidence (%)",
       y = "Productivity Growth (%)",
       color = "Firm Size") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", na.value = "gray50") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

print(scatter_plot)

# ---- Figure 2: Coefficient Plot for Different Model Specifications ----
# Create a dataframe with model coefficients and confidence intervals
coef_data <- data.frame(
  model = c("Fixed Effects", "Random Effects", "DiD (Continuous)", "DiD (Binary)"),
  variable = c("Bribery", "Bribery", "Initial Bribery × Post", "High Initial Bribery × Post"),
  estimate = c(0.084034, -0.0258489, 0.021085, 1.16914),
  se = c(0.020174, 0.0095290, 0.015789, 0.595836),
  lower = c(0.084034 - 1.96*0.020174, -0.0258489 - 1.96*0.0095290, 
            0.021085 - 1.96*0.015789, 1.16914 - 1.96*0.595836),
  upper = c(0.084034 + 1.96*0.020174, -0.0258489 + 1.96*0.0095290, 
            0.021085 + 1.96*0.015789, 1.16914 + 1.96*0.595836)
)

coef_plot <- ggplot(coef_data, aes(x = estimate, y = model, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3) +
  labs(title = "Effect of Corruption on Productivity",
       subtitle = "Coefficient Estimates with 95% Confidence Intervals",
       x = "Coefficient Estimate",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))

print(coef_plot)


# ---- Figure 4: Density Plot of Productivity by Bribery Level ----
density_plot <- ggplot(panel_data, aes(x = productivity, fill = factor(high_bribery))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Productivity Growth",
       subtitle = "By Bribery Level",
       x = "Productivity Growth (%)",
       y = "Density",
       fill = "High Bribery") +
  scale_fill_manual(values = c("#619CFF", "#F8766D"),
                    labels = c("Low Bribery", "High Bribery")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

print(density_plot)

# Combine plots with patchwork
combined_plot <- (scatter_plot | coef_plot) / density_plot +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Analysis of Corruption and Firm Productivity",
    subtitle = "World Bank Enterprise Survey Data (2006-2021)",
    caption = "Note: High bribery is defined as above median bribery incidence.",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )
print(combined_plot)

# Save the plot
ggsave("corruption_productivity_plots.png", combined_plot, width = 10, height = 8, dpi = 300)

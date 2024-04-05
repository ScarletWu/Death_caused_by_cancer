# Load necessary libraries
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

# Simulate the data
num_records <- 1000
years <- sample(2000:2020, size = num_records, replace = TRUE)
age_at_death <- rnorm(n = num_records, mean = 65, sd = 10)
gender <- sample(c("Male", "Female"), size = num_records, replace = TRUE)
cancer_types <- sample(c("Lung", "Breast", "Prostate", "Colorectal", "Skin"), size = num_records, replace = TRUE)

df_cancer_deaths <- data.frame(
  Year = years,
  AgeAtDeath = round(age_at_death),
  Gender = gender,
  CancerType = cancer_types
)

num_deaths <- sample(1:10, size = num_records, replace = TRUE)

df_cancer_deaths$NumDeaths <- num_deaths

# Tests

# 1. Basic summary 
print(summary(df_cancer_deaths))

# 2. Yearly distribution 
print(table(df_cancer_deaths$Year))

# 3. Gender distribution
print(table(df_cancer_deaths$Gender))

# 4. Cancer type distribution
print(table(df_cancer_deaths$CancerType))

# 5. Average age at death
print(mean(df_cancer_deaths$AgeAtDeath))

# 6. Median age at Death (by sex)
print(by(df_cancer_deaths$AgeAtDeath, df_cancer_deaths$Gender, median))

# 7. Count of records by cancer type and gender
print(table(df_cancer_deaths$CancerType, df_cancer_deaths$Gender))

# 8. Yearly trend in age at death
age_trend_model <- lm(AgeAtDeath ~ Year, data = df_cancer_deaths)
summary(age_trend_model)

# 9. Difference in age at death between genders
gender_age_test <- t.test(AgeAtDeath ~ Gender, data = df_cancer_deaths)
print(gender_age_test)

# 10. One-Way ANOVA test
anova_result <- aov(AgeAtDeath ~ CancerType, data = df_cancer_deaths)
summary(anova_result)



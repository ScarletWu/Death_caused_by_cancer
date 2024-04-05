library(ggplot2)
library(rstanarm)

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

# Visualization

ggplot(df_cancer_deaths, aes(x = CancerType, y = AgeAtDeath, fill = CancerType)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Age Distribution by Cancer Type", y = "Age at Death", x = "Cancer Type")


# Model
fit <- stan_glm(NumDeaths ~ Year + Gender, data = df_cancer_deaths, family = poisson(link = "log"))

print(summary(fit))

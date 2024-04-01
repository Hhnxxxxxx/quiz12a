# Load the ggplot2 library
library(ggplot2)

# Plotting the trend of cancer-related deaths over the years for each hospital
ggplot(hospital_annual_deaths, aes(x = Year, y = Total_Deaths, color = Hospital_Name)) +
  geom_line() + # Use line plot to show trends over time
  geom_point() + # Add points to highlight individual data points
  theme_minimal() + # Use a minimal theme for clarity
  labs(title = "Trend of Cancer-Related Deaths by Hospital in Sydney (2004-2023)",
       x = "Year",
       y = "Total Deaths",
       color = "Hospital") +
  theme(plot.title = element_text(hjust = 0.5)) # Center-align the title



library(rstanarm)

# Fit a negative binomial model
model <- stan_glm(Number_of_Deaths ~ Year + Hospital_Name, 
                  data = cancer_deaths_data, 
                  family = neg_binomial_2, 
                  prior = normal(0, 2.5), 
                  prior_intercept = normal(0, 10),
                  chains = 2, 
                  iter = 2000,
                  seed = 12345)

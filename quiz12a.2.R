library(dplyr)
library(stringi)

# Set seed for reproducibility
set.seed(123)

# Define the parameters for the simulation
years <- 2004:2023
hospitals <- paste("Hospital", LETTERS[1:5])
age_groups <- c("0-14", "15-44", "45-64", "65+")
genders <- c("Male", "Female", "Other")

# Generate a data frame for each year
data_list <- lapply(years, function(year) {
  # Create a data frame for each hospital in a given year
  hospital_data <- lapply(hospitals, function(hospital) {
    # Simulate the number of deaths for each age group and gender
    data_frame(
      Hospital_ID = stringi::stri_rand_strings(1, 4, pattern = "[A-Z0-9]"),
      Hospital_Name = hospital,
      Year = year,
      Age_Group = sample(age_groups, 100, replace = TRUE),
      Gender = sample(genders, 100, replace = TRUE),
      Number_of_Deaths = sample(1:50, 100, replace = TRUE)
    )
  })
  # Combine the hospital data for the year
  do.call(rbind, hospital_data)
})

# Combine all years into a single data frame
cancer_deaths_data <- do.call(rbind, data_list)

# Summarize data to get total deaths per year, per hospital
hospital_annual_deaths <- cancer_deaths_data %>%
  group_by(Year, Hospital_Name) %>%
  summarise(Total_Deaths = sum(Number_of_Deaths), .groups = 'drop')



library(testthat)

# Assuming `cancer_deaths_data` is your dataframe

# Test 1: No Missing Values in Critical Columns
test_that("Critical columns have no missing values", {
  critical_cols <- c("Hospital_Name", "Year", "Age_Group", "Gender", "Number_of_Deaths")
  expect_true(all(!is.na(cancer_deaths_data[critical_cols])))
})

# Test 2: Valid Year Range
test_that("Years are within the expected range", {
  expect_true(all(cancer_deaths_data$Year >= 2004 & cancer_deaths_data$Year <= 2023))
})

# Test 3: Hospital Names are Valid
test_that("Hospital names are valid", {
  valid_hospitals <- paste("Hospital", LETTERS[1:5])
  expect_true(all(cancer_deaths_data$Hospital_Name %in% valid_hospitals))
})

# Test 4: Age Group Categories are Correct
test_that("Age groups are categorized correctly", {
  valid_age_groups <- c("0-14", "15-44", "45-64", "65+")
  expect_true(all(cancer_deaths_data$Age_Group %in% valid_age_groups))
})

# Test 5: Gender Categories are Correct
test_that("Gender categories are correct", {
  valid_genders <- c("Male", "Female", "Other")
  expect_true(all(cancer_deaths_data$Gender %in% valid_genders))
})

# Test 6: Death Counts are Positive
test_that("Number of deaths are positive integers", {
  expect_true(all(cancer_deaths_data$Number_of_Deaths > 0))
})

# Test 7: Data Types are As Expected
test_that("Data types match expectations", {
  expect_is(cancer_deaths_data$Year, "numeric")
  expect_is(cancer_deaths_data$Hospital_Name, "character")
  expect_is(cancer_deaths_data$Age_Group, "factor")
  expect_is(cancer_deaths_data$Gender, "factor")
  expect_is(cancer_deaths_data$Number_of_Deaths, "numeric")
})

# Test 8: No Duplicate Records
test_that("There are no duplicate records", {
  expect_equal(nrow(cancer_deaths_data), nrow(unique(cancer_deaths_data)))
})

# Test 9: Aggregate Totals Check
test_that("Aggregate totals are correct for a sample year", {
  sample_year <- 2005
  # This total should be adjusted based on your manual calculations or expected results
  expected_total <- 1000 
  observed_total <- sum(cancer_deaths_data$Number_of_Deaths[cancer_deaths_data$Year == sample_year])
  expect_equal(observed_total, expected_total)
})

# Test 10: Uniform Distribution of Hospital_ID
test_that("Hospital_IDs are uniformly distributed", {
  # Assuming an equal distribution is expected, adjust if your dataset's context differs
  expected_entries_per_hospital <- length(cancer_deaths_data$Hospital_ID) / length(unique(cancer_deaths_data$Hospital_ID))
  observed_entries <- table(cancer_deaths_data$Hospital_ID)
  # This test checks that each hospital's entry count is close to the expected count, allowing for some variance
  expect_true(all(abs(observed_entries - expected_entries_per_hospital) < (0.1 * expected_entries_per_hospital)))
})

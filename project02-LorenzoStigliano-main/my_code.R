# Lorenzo Stigliano (s1725018, LorenzoStigliano)

# Load function definitions
source("my_functions.R")

############################ Question 2.1 setup ################################

# Calculating the coefficients and confidence intervals for the monthly trends.

# Load data in
data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

# Update the station names such that they are more legible
ghcnd_stations$Name[7] <- "ROYAL BOTANICAL GARDEN"
ghcnd_stations$Name[8] <- "BENMORE"

# Find data frame we will use, in particular for the mean of TMIN for each month
data <- left_join(ghcnd_values, ghcnd_stations, by = "ID") %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  filter(!is.na(TMIN)) %>%
  group_by(ID, Name, Month, Year) %>%
  summarise(TMIN = mean(TMIN), .groups = "drop")

# Save all data to show an example of what we are doing
saveRDS(data, file = "data/2_1_data.rds")

# Calculate the coeff and confidence intervals for the monthly trends and save
# them into a data frame that we can later plot in report.Rmd
coeff_data <- monthly_trends(data)
saveRDS(coeff_data, file = "data/2_1_coeff_data.rds")

############################ Question 2.2 setup ################################

# In this section we create the data frame used for question 2.2 and calculate
# the p-values of the hypothesis test

# Load data in again
data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

# Find the daily Residuals for BRAEMAR
data_residuals <- left_join(ghcnd_values, ghcnd_stations, by = "ID") %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  filter(Name == "BRAEMAR" & !is.na(TMIN)) %>%
  group_by(Month, Year) %>%
  summarise(Day, R = TMIN - mean(TMIN), .groups = "drop")

# Calculate the p-values using a randomization test
p_values <- randomisation_test(data_residuals, 2500)

# Save this data frame into an .rds file
saveRDS(p_values, file = "data/2_2_p_values.rds")

############################ Question 3.1 setup ################################

# We are creating the data frame which will be used throughout section 3.1. In
# particular it will have the stations as columns and the TMAX values as their
# entries such that we can create lm for estimating daily TMAX.

# Load data in again
data(ghcnd_stations, package = "StatCompLab")
data(ghcnd_values, package = "StatCompLab")

# Update the names such that they are more legible
ghcnd_stations$Name[7] <- "ROYAL BOTANICAL GARDEN"
ghcnd_stations$Name[8] <- "BENMORE"

# Here we are creating the data frame we will use in this part of the project
ghcnd_TMAX <- left_join(ghcnd_values, ghcnd_stations, by = "ID") %>%
  # We first pivot wider the values of the elements
  pivot_wider(names_from = Element, values_from = Value) %>%
  # Pick the columns of interest, we will be working with the station names
  subset(select = c(TMAX, Year, Month, Day, Name)) %>%
  # Pivot_wider again such that we create columns for each of the stations
  pivot_wider(names_from = Name, values_from = TMAX) %>%
  # Drop any rows with missing values
  drop_na()

# Add a date column used for plotting later, each row has a unique date
ghcnd_TMAX$Date <- as.Date(with(
  ghcnd_TMAX,
  paste(Year, Month, Day, sep = "-")
), "%Y-%m-%d")

# Move dates to the front of the data frame
ghcnd_TMAX <- ghcnd_TMAX %>%
  select(Date, everything())

saveRDS(ghcnd_TMAX, file = "data/3_1_data.rds")

# Station of interest we will build linear model for
station <- "BRAEMAR"
# Covariates of the linear model, these have the same column name as in ghcnd_TMAX
covariates <- c(
  "BALMORAL", "ARDTALNAIG", "FASKALLY",
  "LEUCHARS", "PENICUIK", "`ROYAL BOTANICAL GARDEN`",
  "BENMORE"
)

# Get the coefficients of the linear models for the chosen station
coeff <- get_model_coeff(ghcnd_TMAX, station, covariates)
saveRDS(coeff, file = "data/3_1_data_coeff.rds")

############################ Question 3.2 setup ################################

# We are creating the data frame which will be used throughout section 3.2, in
# particular we are calculating the proper scores, such that we can show them
# directly in the report.Rmd file

# Calculate the proper scores using the data
proper_scores <- calculate_proper_scores(ghcnd_TMAX, station, covariates)
# Save this data frame into an .rds file
saveRDS(proper_scores, file = "data/3_2_proper_scores.rds")

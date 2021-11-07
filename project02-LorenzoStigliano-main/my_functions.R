# Lorenzo Stigliano (s1725018, LorenzoStigliano)

suppressPackageStartupMessages(library(StatCompLab))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))

# Set the seed so we can have consistent results.
set.seed(123)

############################## Question 2.1 ####################################

monthly_trends <- function(data) {
  #' Getting Monthly Trends per weather station
  #'
  #' Here we are getting the monthly trends for the each of the months and for
  #' each of the different stations. This is used to find the  long-term temporal
  #' trend in TMIN for each month, for each weather station.
  #'
  #' @param data (data frame) a data frame with the following columns:
  #' 1 - ID: ID of the station (chr)
  #' 2 - Name: Name of the station (chr)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - TMIN: Average TMIN for a specific year and month (dbl)
  #'
  #' @return coeff_data (dataframe) a data frame with the following columns:
  #' 1 - Name: Name of the station (chr)
  #' 2 - ID: ID of the station (chr)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Coeff: 10*Coefficient of the linear model for the given station and
  #'            month such that it shows us the change per decade of the
  #'            temperatures. (dbl)
  #' 5 - Intercept: Intercept of the linear model for the given station
  #'                and month (dbl)
  #' 6 - Lower_Bound: Lower bound on the credible interval for Coeff (dbl)
  #' 7 - Upper_Bound: Upper bound on the credible interval for Coeff (dbl)

  # Create an empty data frame to store the values
  coeff_data <- data.frame()

  # Iterate over the stations
  for (j in c(1:8)) {

    # Get the station ID
    id <- ghcnd_stations[j, 1]
    # Get the station names
    name <- ghcnd_stations[j, 2]

    # Iterate over the months
    for (i in c(1:12)) {

      # Get the data for the given month and station
      month_data <- filter(data, data$Month == i, ID == id)
      # Create a linear model where we are trying to fit TMIN against year
      fit0 <- lm(TMIN ~ 1 + Year, data = month_data)

      # Extract the information form the linear model that we created
      # Get the coefficient
      coeff <- coef(fit0)["Year"]
      # Get the intercept
      intercept <- coef(fit0)["(Intercept)"]
      # Calculate the standard error for the coefficient such that we can plot
      # credible intervals for this value
      std_error <- sqrt(vcov(fit0)["Year", "Year"])

      # Calculate the credible bounds on the coefficient
      bound <- coeff - std_error * qnorm(c(1 - 0.05 / 2, 0.05 / 2))

      # Add the new row to the data frame
      coeff_data <- rbind(coeff_data, data.frame(
        Name = name, ID = id,
        Month = i, Coeff = coeff * 10,
        Intercept = intercept,
        Lower_Bound = bound[1] * 10,
        Upper_Bound = bound[2] * 10
      ))
    }
  }

  # Get rid of the row names
  rownames(coeff_data) <- NULL
  # Return the data frame
  return(coeff_data)
}

plot_coeff <- function(coeff_data) {
  #' Plot the “change per decade” for each month and each weather station
  #' it uses the output from the monthly_trends functions to plot this table
  #'
  #' @param coeff_data (data frame) this is in fact the output from the monthly_trends()
  #' function, has the following columns:
  #' 1 - Name: Name of the station (chr)
  #' 2 - ID: ID of the station (chr)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Coeff: 10*Coefficient of the linear model for the given station and
  #'            month such that it shows us the change per decade of the
  #'            temperatures. (dbl)
  #' 5 - Intercept: Intercept of the linear model for the given station
  #'                and month (dbl)
  #' 6 - Lower_Bound: Lower bound on the credible interval for Coeff (dbl)
  #' 7 - Upper_Bound: Upper bound on the credible interval for Coeff (dbl)
  #'
  #' @return a plot of the monthly trends for each station

  # Feed in ceoff_data
  coeff_data %>%
    # Group by name Id and month
    group_by(ID, Name, Month) %>%
    # Summarize the coeff such that it can be use later
    summarise(Coeff = Coeff, .groups = "drop") %>%
    # Plot Month against Coeff and separate on the Name of the station
    ggplot(aes(Month, Coeff, colour = Name)) +
    # Plot the points
    geom_point() +
    # Add the credible intervals on each of the Coeffs
    geom_ribbon(aes(y = Coeff, ymin = L, ymax = U),
      data = coeff_data %>%
        group_by(ID, Name, Month) %>%
        summarise(
          Coeff = Coeff,
          L = Lower_Bound,
          U = Upper_Bound, .groups = "drop"
        ),
      alpha = 0.25
    ) +
    # Facet_wrap on Name to produce multiple plots for each of the weather station
    facet_wrap(~Name) +
    ggtitle("Tempeature change per decade for each month and for each weather station.")
}

############################## Question 2.2 ####################################

randomisation_test <- function(data_residuals, J) {
  #' Preforms a randomization tests on the residuals for each month for a chosen
  #' station
  #'
  #' @param data_residuals (data frame) a data frame for a given station
  #' with the following columns:
  #' 1 - Day: Day of the month measurements taken (int)
  #' 2 - Month: Month of the year 1-12 (int)
  #' 3 - Year: Year of the measurement (int)
  #' 4 - R: Residual for the particular day, calculated by: TMIN - mean(TMIN) (for
  #' the given month in which the measurement was taken) (dbl)
  #' @param J (int) this is the number of permutations to be carried out
  #'
  #' @return p_values_all (data frame) containing p-value for each month with the
  #' following columns:
  #' 1 - Month: Month of the year 1-12 (int)
  #' 2 - Month_Residuals_Var: Residual Variance for each month (dbl)
  #' 3 - One_Sided_P_Value: P-value for the associated month (dbl)
  #' 4 - Two_Sided_P_Value: P-value for the associated month for
  #' the two sided test (dbl)

  # Variance of all residuals, under H_0
  Var_all <- (data_residuals %>%
    summarise(Var_R = var(R), .groups = "drop"))$Var_R

  # T statistics for all months
  T_stats_months <- data_residuals %>%
    group_by(Month) %>%
    summarise(T_stats = var(R) - Var_all, .groups = "drop")

  # Residuals for each month
  Variances_Residual_Monthly <- data_residuals %>%
    group_by(Month) %>%
    summarise(Variance = var(R), .groups = "drop")


  # This variable keeps tract of all the permutations greater than the test
  # statistic for each month, used for the one-sided test
  Ts_count <- numeric(12)

  # Run for the number of iterations J
  for (j in c(1:J)) {

    # Mutate the data by moving position of the residuals for each month
    # And then calculate the T_sample for this permutation
    permutation <- data_residuals %>%
      # mutate the R column so all months have some permutation of the original
      # residuals
      mutate(R = sample(R, size = n(), replace = FALSE)) %>%
      group_by(Month) %>%
      # Create test statistic for this permutation by using the residual for each month
      summarize(T_sample = var(R) - Var_all)

    # Check if the permutation is greater than the test statistic for each month
    Ts_count <- Ts_count + c(as.numeric(permutation$T_sample >= T_stats_months$T_stats))
  }
  # calculate the p value, proportion of the permutations that give
  # a test stat at least as extreme as the one we observed

  # Calculate the p value for each month, since we already have the number of
  # permutations which are greater that the test statistic all we are left to do
  # is find the mean which in this case is just divided by the over all number
  # of permutations we have done
  pvalues <- Ts_count / J

  # Create a data frame for all the p-values for each month we have done the
  # randomization test for
  p_values_all <- data.frame(
    Month = seq_len(12),
    Month_Residuals_Var = Variances_Residual_Monthly$Variance,
    P_Value_One_Sided = pvalues,
    P_Value_Two_Sided = 2 * apply(cbind(pvalues, 1 - pvalues),
      1,
      FUN = min
    )
  )

  return(p_values_all)
}

############################## Question 3.1 ####################################

model_station <- function(data_fit, station, covariates) {
  #' Function to find model for given data for a station
  #'
  #' Note: the data_fit parameter needs to be in a particular form. In particular
  #' the covariates list needs to have the same values as the columns of the
  #' data_fit data frame we want to create our linear model on.
  #'
  #' @param data_fit (data frame) a data frame with the following columns:
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX for the corresponding day for BRAEMAR
  #' 6 - BALMORAL: These are the TMAX for the corresponding day for BALMORAL
  #' 7 - ARDTALNAIG: These are the TMAX for the corresponding day for ARDTALNAIG
  #' 8 - FASKALLY: These are the TMAX for the corresponding day for FASKALLY
  #' 9 - LEUCHARS: These are the TMAX for the corresponding day for LEUCHARS
  #' 10 - PENICUIK: These are the TMAX for the corresponding day for PENICUIK
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX for the corresponding
  #' day for ROYAL BOTANICAL GARDEN
  #' 12 - BENMORE: These are the TMAX for the corresponding day for BENMORE
  #' @param station (str) this is the name of the station that we want to do our
  #' linear model on, the dependent variable
  #' @param covariates (list of str) this is the name of the other stations as the
  #' covariates that we can use as our linear model, these need to be the same as
  #' the column names of the data_fit data frame.
  #'
  #' @return the linear model with the other stations as covariates

  # Create the formula used for the linear model
  formula <- as.formula(
    paste(station,
      paste(covariates, collapse = " + "),
      sep = " ~ "
    )
  )

  # Create the linear model
  model <- lm(formula, data = data_fit)

  return(model)
}

predict_station <- function(model, data_pred) {
  #' Function to find prediction for given data
  #' Note: the data_pred parameter needs to be in a particular form.
  #'
  #' @param model (linear model produced by lm()) this is the linear model made by
  #' the model_station() function
  #' @param data_pred (data frame) a data frame with the following columns:
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX for the corresponding day for BRAEMAR
  #' 6 - BALMORAL: These are the TMAX for the corresponding day for BALMORAL
  #' 7 - ARDTALNAIG: These are the TMAX for the corresponding day for ARDTALNAIG
  #' 8 - FASKALLY: These are the TMAX for the corresponding day for FASKALLY
  #' 9 - LEUCHARS: These are the TMAX for the corresponding day for LEUCHARS
  #' 10 - PENICUIK: These are the TMAX for the corresponding day for PENICUIK
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX for the corresponding
  #' day for ROYAL BOTANICAL GARDEN
  #' 12 - BENMORE: These are the TMAX for the corresponding day for BENMORE
  #'
  #' @return the prediction for the data_pred and model passed to this function

  predict(model, newdata = data_pred, se.fit = TRUE)
}

get_model_coeff <- function(data, station, covariates) {
  #' Calculates the linear model for a given station for each month and extracts the
  #' coefficients of each of the covariates.
  #'
  #' Note: the data parameter needs to be in a particular form. Where the covariates
  #' parameter must have the same column names for this function to work.
  #'
  #' @param data (data frame) a data frame with the following columns, note that
  #' the name of the columns can be changed but need to also change @param covariates.
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX for the corresponding day for BRAEMAR (dbl)
  #' 6 - BALMORAL: These are the TMAX for the corresponding day for BALMORAL (dbl)
  #' 7 - ARDTALNAIG: These are the TMAX for the corresponding day for ARDTALNAIG (dbl)
  #' 8 - FASKALLY: These are the TMAX for the corresponding day for FASKALLY (dbl)
  #' 9 - LEUCHARS: These are the TMAX for the corresponding day for LEUCHARS (dbl)
  #' 10 - PENICUIK: These are the TMAX for the corresponding day for PENICUIK (dbl)
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX for the corresponding
  #' day for ROYAL BOTANICAL GARDEN (dbl)
  #' 12 - BENMORE: These are the TMAX for the corresponding day for BENMORE (dbl)
  #' @param station (str) this is the name of the station that we want to do our
  #' linear model on, the dependent variable
  #' @param covariates (list of str) this is the name of the other stations as the
  #' covariates that we can use as our linear model, these need to be the same as
  #' the column names of the @param data.
  #'
  #' @return output_data (data frame) with the coefficients of each station for
  #' each month. It has the following columns:
  #' 1 - Month: Month of the year 1-12 (int)
  #' 2 - 8: Are the stations with their associated coeffcient as the entries.

  # Create an empty data frame to add the information from the linear model
  output_data <- data.frame()

  # iterate over all the months
  for (i in seq_len(12)) {
    # get the data to use for the model by separating by month
    data_fit <- data %>% filter(Month == i)
    # find the model for the given data for the station of choice
    mod <- model_station(data_fit, station, covariates)
    # Extract the coefficients
    coeff <- summary(mod)$coefficients
    # Add the coefficient for each of the covariantes to a new data frame
    # There will always be 7 covariate since we use all the other stations as
    # the covariates
    data_frame_month <- data.frame(
      Month = i,
      Station_1 = coeff[covariates[1], "Estimate"],
      Station_2 = coeff[covariates[2], "Estimate"],
      Station_3 = coeff[covariates[3], "Estimate"],
      Station_4 = coeff[covariates[4], "Estimate"],
      Station_5 = coeff[covariates[5], "Estimate"],
      Station_6 = coeff[covariates[6], "Estimate"],
      Station_7 = coeff[covariates[7], "Estimate"]
    )

    # Add this data data_frame_month to data_all
    output_data <- rbind(output_data, data_frame_month)
  }
  # Rename the name of the station to the name used in covariates
  colnames(output_data) <- c("Month", covariates)

  # Return output_data, which is all the information we need
  return(output_data)
}

plot_actual_pred <- function(data, station, covariates) {
  #' This function plots the predicted value of TMAX for a given station against
  #' the observation for TMAX for a given station Using the linear model created by the
  #' function model_station. These plots are called prediction v observation plots.
  #'
  #' @param data (data frame) a data frame with the following columns, note that
  #' the name of the columns can be changed but need to also change @param covariates.
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX for the corresponding day for BRAEMAR (dbl)
  #' 6 - BALMORAL: These are the TMAX for the corresponding day for BALMORAL (dbl)
  #' 7 - ARDTALNAIG: These are the TMAX for the corresponding day for ARDTALNAIG (dbl)
  #' 8 - FASKALLY: These are the TMAX for the corresponding day for FASKALLY (dbl)
  #' 9 - LEUCHARS: These are the TMAX for the corresponding day for LEUCHARS (dbl)
  #' 10 - PENICUIK: These are the TMAX for the corresponding day for PENICUIK (dbl)
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX for the corresponding
  #' day for ROYAL BOTANICAL GARDEN (dbl)
  #' 12 - BENMORE: These are the TMAX for the corresponding day for BENMORE (dbl)
  #' @param station (str) this is the name of the station that we want to do our
  #' linear model on, the dependent variable
  #' @param covariates (list of str) this is the name of the other stations as the
  #' covariates that we can use as our linear model, these need to be the same as
  #' the column names of the data_fit data frame.
  #'
  #' @return the a plot for prediction against actual value for TMAX for each month
  #' where the plot are labeled by the number of the month.

  # Create an empty data frame to add the actual values and prediction for each of
  # the months from the linear model
  data_all <- data.frame()

  # Iterate over the month
  for (i in seq_len(12)) {

    # get the data to use for the model by separating by month
    data_fit <- data %>% filter(Month == i)
    # find the model for the given data for the given station
    mod <- model_station(data_fit, station, covariates)
    # find the predictions for TMAX for the given data for the given station
    # using the above model
    pred <- predict_station(mod, data_fit)

    # Add the actual and prediction values for TMAX for each month
    data_frame_month <- data.frame(
      Observation = pull(data_fit, station),
      Prediction = pred$fit,
      Month = numeric(length(pred$fit)) + i
    )

    # Add data_frame_month to data_all so we can plot them later
    data_all <- rbind(data_all, data_frame_month)
  }
  # Update name of the first column to observed
  names(data_all)[1] <- "Observation"

  # Plot all the actual values against predicted values for all months
  ggplot(data_all) +
    geom_point(aes(Observation, Prediction), size = 0.5) +
    # Add the line y = x to show good prediction
    geom_abline(intercept = 0, slope = 1, col = "red") +
    facet_wrap(~Month) +
    ggtitle(sprintf("Plotting predicted against observed TMAX for %s.", station))
}

plot_year_pred_actual <- function(data, year, station, covariates) {
  #' This function plots the predicted value of TMAX for a given station and
  #' the observation for TMAX for a given station for a given year.
  #' Using the linear model created by the function model_station.
  #' These plots are a time series plot to see how the prediction and observation
  #' matches for a year
  #'
  #' @param data (data frame) a data frame with the following columns, note that
  #' the name of the columns can be changed but need to also change @param covariates.
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX for the corresponding day for BRAEMAR (dbl)
  #' 6 - BALMORAL: These are the TMAX for the corresponding day for BALMORAL (dbl)
  #' 7 - ARDTALNAIG: These are the TMAX for the corresponding day for ARDTALNAIG (dbl)
  #' 8 - FASKALLY: These are the TMAX for the corresponding day for FASKALLY (dbl)
  #' 9 - LEUCHARS: These are the TMAX for the corresponding day for LEUCHARS (dbl)
  #' 10 - PENICUIK: These are the TMAX for the corresponding day for PENICUIK (dbl)
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX for the corresponding
  #' day for ROYAL BOTANICAL GARDEN (dbl)
  #' 12 - BENMORE: These are the TMAX for the corresponding day for BENMORE (dbl)
  #' @param year (int) the year we want to plot the time series data for between
  #' 1960 - 2018
  #' @param station (str) this is the name of the station that we want to do our
  #' linear model on
  #' @param covariates (list of str) this is the name of the other stations as the
  #' covariates that we can use as our linear model, these need to be the same as
  #' the column names of the data_fit data frame.
  #'
  #' @return the a line plot for predicted value of TMAX for BRAEMAR and
  #' the observation for TMAX for BRAEMAR for each day of given year.

  # Create an empty data frame to add the actual values and prediction for each of
  # the months from the linear model
  output_data <- data.frame()

  # Iterate over the months
  for (i in seq_len(12)) {

    # get the data to use for the model by separating by month we use all the month
    # over all years to predict the model
    data_fit <- data %>% filter(Month == i)
    # Then we get the data for the year and month we are trying to predict
    data_year <- data_fit %>%
      filter(Month == i) %>%
      filter(Year == year)

    # find the model for the given data for a given station
    mod <- model_station(data_fit, station, covariates)
    # find the predictions for TMAX for the given data for the station using the above
    # model, in particular for a specific year
    pred <- predict_station(mod, data_year)

    # Add the information we need to plot time series data, in particular
    # we need to add the Date of the prediction
    data_frame_month <- data.frame(
      Observation = pull(data_year, station),
      Prediction = pred$fit,
      Month = numeric(length(pred$fit)) + i,
      Date = data_year$Date
    )
    # Add data_frame_month to output_data such that we can plot it
    output_data <- rbind(output_data, data_frame_month)
  }
  # Update name of the first column to observed
  names(output_data)[1] <- "Observation"

  # Produce a time series plot of actual and observed data against all the days of
  # a given year by using the data from output_data
  ggplot(output_data %>% group_by(Month), aes(Observation, Prediction)) +
    # plot the line graph for actual data
    geom_line(aes(Date, Observation, color = "blue")) +
    # plot the line graph for actual data
    geom_line(aes(Date, Prediction, color = "#69b3a2")) +
    labs(
      x = "Date",
      y = "TMAX (in degrees Celsius)"
    ) +
    # Add a Legend to the plot
    scale_color_identity(
      name = "Legend",
      breaks = c("blue", "#69b3a2"),
      labels = c("Observed", "Prediction"),
      guide = "legend"
    ) +
    ggtitle(sprintf("Plotting time series data for observed and predicted TMAX value for %s.
    In this case we are plotting it for the year %s.", station, year))
}

plot_locations <- function(ghcnd_stations) {
  #' Plots the geographical locations of the stations
  #'
  #' @param ghcnd_stations (data frame) a data frame with the following columns:
  #' 1 - ID: ID of the station (chr)
  #' 2 - Name: Name of the station (chr)
  #' 3 - Latitude: Latitude of the position of station (dbl)
  #' 4 - Longitude: Longitude of the position of station (dbl)
  #' 5 - Elevation: Elevation of the station (int)
  #'
  #' @return the position of the geographical positions of the stations on a graph

  # Create a ggplot of with the data
  ggplot(ghcnd_stations) +
    # Plot the points by Longitude and Latitude
    geom_point(aes(Longitude, Latitude, col = Name)) +
    # Add tags to the point so we know what station they belong to
    geom_text(aes(Longitude, Latitude, label = Name), hjust = 0, vjust = 0, size = 3) +
    # Change x and y axis such that it is more visible
    coord_cartesian(
      xlim = c(min(ghcnd_stations$Longitude) - 1, max(ghcnd_stations$Longitude) + 1),
      ylim = c(min(ghcnd_stations$Latitude), max(ghcnd_stations$Latitude))
    ) +
    ggtitle("Spatial locations of stations.") +
    # Get rid of the legend
    theme(legend.position = "none")
}

############################## Question 3.2 ####################################

calculate_proper_scores <- function(all_data, station, covariates) {
  #' This function calculates the proper scores, the expected Absolute Error and
  #' Dawid-Sebastiani scores for the TMAX predictions at the chosen station. It is
  #' done by doing a 10-fold randomized cross-validation for each month.
  #'
  #' @param all_data (data frame) a data frame with the following columns (note that
  #' the name of the columns can be changed but need to also change @param covariates
  #' since they need to be the same.)
  #' 1 - Date: This is a new parameter where it just puts the Day, Year and Month
  #' together (date)
  #' 2 - Day: Day of the month measurements taken (int)
  #' 3 - Month: Month of the year 1-12 (int)
  #' 4 - Year: Year of the measurement (int)
  #' 5 - BRAEMAR: These are the TMAX values for the corresponding day for BRAEMAR (dbl)
  #' 6 - BALMORAL: These are the TMAX values for the corresponding day for BALMORAL (dbl)
  #' 7 - ARDTALNAIG: These are the TMAX values for the corresponding day for ARDTALNAIG (dbl)
  #' 8 - FASKALLY: These are the TMAX values for the corresponding day for FASKALLY (dbl)
  #' 9 - LEUCHARS: These are the TMAX values for the corresponding day for LEUCHARS (dbl)
  #' 10 - PENICUIK: These are the TMAX values for the corresponding day for PENICUIK (dbl)
  #' 11 - `ROYAL BOTANICAL GARDEN`: These are the TMAX values for the corresponding
  #' day for ROYAL BOTANICAL GARDEN (dbl)
  #' 12 - BENMORE: These are the TMAX values for the corresponding day for BENMORE (dbl)
  #' @param station (str) this is the name of the station that we want to do our
  #' linear model on, the dependent variable
  #' @param covariates (list of str) this is the name of the other stations as the
  #' covariates that we can use as our linear model, these need to be the same as
  #' the column names of the all_data data frame.
  #'
  #' @return proper_score_data (data frame) returns the proper score data for
  #' TMAX predictions at the chosen station. In particular it has the following
  #' columns:
  #' 1 - Month: Month of the year 1-12 (int)
  #' 2 - Absolute_Error: Absolute Error proper score (dbl)
  #' 3 - Std_Dev_AE: Standard Deviation of the Absolute Error proper score (dbl)
  #' 4 - Dawid_Sebastiani: Dawid Sebastiani proper score (dbl)
  #' 5 - Std_Dev_DS: Standard Deviation of the Dawid Sebastiani proper score (dbl)
  #' 6 - Difference: Difference between the Dawid Sebastiani and Absolute Error
  #' calculated by: Dawid Sebastiani - Absolute Error (dbl)

  # initialize empty vectors of size 12 to store the scores and standard deviation
  # for Absolute Error proper score
  all_scores_ae <- numeric(12)
  s_d_scores_ae <- numeric(12)

  # initialize empty vectors of size 12 to store the scores and standard deviation
  # for Dawid-Sebastiani proper score
  all_scores_ds <- numeric(12)
  s_d_scores_ds <- numeric(12)

  all_residual_variances <- numeric(12)
  all_prediction_variance <- numeric(12)

  # iterate over all the months
  for (i in seq_len(12)) {

    # Find the number of rows for a given month to be able to separate the data into
    # 10 groups
    n_rows <- nrow(all_data %>% filter(Month == i))

    # 'data' is the data used for the cross-validation
    data <-
      # we first filter 'all_data' to get the month we will be doing the linear
      # models with
      all_data %>%
      filter(Month == i) %>%
      # Mutate this data frame such that we add a new column called 'Group' which
      # defines subset of the data frame for each of the 10 cross validation
      mutate(
        Group = sample(seq_len(n_rows), size = n_rows, replace = FALSE),
        Group = (Group %% 10) + 1
      )

    # create two placeholder vectors to get the proper score of every iteration
    # of the cross validation, after the 10 fold validation we will find the mean
    # of these values to get an estimate of the proper_scores
    scores_ae <- numeric(10)
    scores_ds <- numeric(10)

    # Then loop over the partition groups, estimating the model by leaving the group
    # out, and then predicting and scoring predictions for the group.
    for (grp in seq_len(10)) {

      # We first filter out the rows which do not belong to the group we want to
      # validate on, which we will use to fit the models
      data_fit <- data %>% filter(Group != grp)
      # Get the remain rows which belong to the grp we will validate on
      data_pred <- data %>% filter(Group == grp)

      # create the linear model for the chosen 'station' by using model_station()
      mod <- model_station(data_fit, station, covariates)
      # create predictions on the data_pred data set using the model 'mod'
      pred <- predict(mod, newdata = data_pred, se.fit = TRUE)

      # add to the vector 'scores_ae' the Absolute Error proper score
      scores_ae[grp] <- mean(proper_score("ae", pull(data_pred, station),
        median = pred$fit
      ))

      # find standard deviation used for the Dawid-Sebastiani proper score
      sd_pred <- sqrt(pred$se.fit^2 + pred$residual.scale^2)
      # add to the vector 'scores_ds' the Dawid-Sebastiani proper score
      scores_ds[grp] <- mean(proper_score("ds", pull(data_pred, station),
        mean = pred$fit,
        sd = sd_pred
      ))
    }
    # add the estimate of the Absolute Error proper score for the month
    all_scores_ae[i] <- mean(scores_ae)
    # add the standard deviation of the Absolute Error proper score for the month
    s_d_scores_ae[i] <- sd(scores_ae)

    # add the estimate of the Dawid-Sebastiani proper score for the month
    all_scores_ds[i] <- mean(scores_ds)
    # add the standard deviation of the Dawid-Sebastiani proper score for the month
    s_d_scores_ds[i] <- sd(scores_ds)
  }

  # Create the data frame we will return with all the information we need as
  # explained in the doc string
  proper_score_data <- data.frame(
    Month = seq_len(12),
    Absolute_Error = all_scores_ae,
    Std_Dev_AE = s_d_scores_ae,
    Dawid_Sebastiani = all_scores_ds,
    Std_Dev_DS = s_d_scores_ds,
    Difference = all_scores_ds - all_scores_ae
  )

  # return the data frame
  return(proper_score_data)
}

plot_proper_scores <- function(proper_scores) {
  #' This function plots the proper scores, the expected Absolute Error and
  #' Dawid-Sebastiani scores calculated by using the calculate_proper_scores()
  #' function
  #'
  #' @param proper_scores (data frame) the data frame produced by the
  #' calculate_proper_scores() function
  #'
  #' @return plots two scatter plots on differnet axis for the absolute error
  #' and the Dawid_Sebastiani against the months

  # Create a 2 by 1 frame for the plots
  par(mfrow = c(2, 1))
  # plot the Absolute_Error against Month
  plot(proper_scores$Month, proper_scores$Absolute_Error,
    xlab = "Month", ylab = "Absolute Error score",
    main = "Plotting Scores against Month."
  )
  # plot the Dawid_Sebastiani against Month
  plot(proper_scores$Month, proper_scores$Dawid_Sebastiani,
    xlab = "Month", ylab = "Dawid Sebastiani score"
  )
}

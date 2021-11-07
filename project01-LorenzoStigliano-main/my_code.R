# Your Name (s1725018, LorenzoStigliano)
# Place your function definitions and associated code documentation my_code.R

multi_pois_CI <- function(m, n, lambda, alpha, type) {
  #' Simulation of Poisson Intervals
  #'
  #' Simulates m number of runs to calculate the confidence interval for n
  #' number of observations. We can pick which type of parameterisation used
  #' (1, 2 or 3).
  #'
  #' @param m (integer) non-negative integer indicating number of simulations to
  #'   preform.
  #' @param n (integer) non-negative integer indicating the number of
  #'   observations used for each sample.
  #' @param lambda (integer) a non negative integer which is the true parameter
  #'   value.
  #' @param alpha (integer) The nominal (desired) error probability for the
  #'   confidence interval.
  #' @param type (integer) 1, 2, or 3, selecting one of the three
  #'   parameterisation alternatives, theta=lambda, theta=sqrt(lambda), or
  #'   theta=log(lambda).
  #'
  #' @return CI (dataframe) a dataframe with m rows and 2 columns, which has
  #'   confidence intervals for each of the m simulations preformed. The name of
  #'   each column is "Lower" and "Upper" indicating the lower and upper bounds
  #'   of the confidence interval respectively.

  #Check if m and n are non negative, if this is the case then return an error.
  if (m < 0 || n < 0 ) {
    stop("m and n need to be positive.")
  }

  #Check that the type is either 1, 2 or 3
  if (type == 1 || type == 2 || type == 3) {
    #Create a data frame with place holder values
    CI <- data.frame(Lower = numeric(m), Upper = numeric(m))
    #Iterate over all the rows
    for (k in c(1:m)){
      #Create n samples for each row from a poisson distribution with lambda
      y <- rpois(n = n, lambda = lambda)
      #Update the row of CI data frame with the appropriate confidence interval
      #using the function pois_CI()
      CI[k, ] <- pois_CI(y, alpha = alpha, type = type)
    }

    #Return CI as a data frame
    return(as.data.frame(CI))
  }

  #If the type is  NOT either 1, 2 or 3 stop and return message
  else {
    stop(paste0("Unknown parameterisation type ", type))
  }
}

tidy_multi_pois_CI <- function(m, n, lambda, alpha){
  #' Tidy multiple simulation of Poisson Intervals
  #'
  #' Simulates m number of runs to calculate the confidence interval for n
  #' number of observations. It does this for all the 3 types of parameterisation.
  #' Therefore we have a set of 3*m rows.
  #'
  #' @param m (integer) non-negative integer indicating number of simulations to
  #'   preform.
  #' @param n (integer) non-negative integer indicating the number of
  #'   observations used for each sample.
  #' @param lambda (integer) a non negative integer which is the true parameter value.
  #' @param alpha (float) The nominal (desired) error probability for the
  #'   confidence interval.
  #'
  #' @return multi_data_frame (dataframe) with 3*m rows and three columns,
  #'   named Lower, Upper, and Type containing the confidence intervals from
  #'   three calls to multi_pois_CI, with the Type column containing values 1, 2,
  #'   or 3 to indicate which method calculated each interval.

  #Create the output data frame, without populating it
  multi_data_frame <- data.frame(Lower = numeric(m*3), Upper = numeric(m*3),
                                      Type = numeric(m*3))

  #Loop over the 3 possible types of method to calculate the interval
  for (i in c(1:3)){

    #Index for the lower bound to put the m confidence intervals for each type
    lower <- 1+m*(i-1)
    #Index for the upper bound to put the m confidence intervals for each type
    upper <- m*i
    #Put the confidence interval calculated using multi_pois_CI() into correct
    #position in the output dataframe
    multi_data_frame[(lower:upper),1:2] <- multi_pois_CI(m, n, lambda, alpha, i)
    #Update the type column in output dataframe
    multi_data_frame[(lower:upper), 3] <- list(rep(i, m))
  }
  #Return the output dataframe
  return(multi_data_frame)
}

log_prob_NY <- function(N,Y,xi,a,b){
  #' Log Probability of P(N, Y)
  #'
  #' Calculates the P(N, Y) when the combination of N and Y is valid. That is
  #' when N is greater than all values of Y. This is calculated using by taking
  #' logs of the expression we have shown in question 2.1, that is log(P(N, Y)).
  #'
  #' @param N (a vector of length J) This is the total number of people buried
  #'   for each of the J excavations.
  #' @param Y (a data frame of size J×2) This is the total number of left and
  #'   right femurs found in each of the J excavations.
  #' @param xi (double) is the model hyperparameter for the geometric distribution
  #'   this value is between 0 and 1.
  #' @param a (double) is the model hyperparameter for the beta distribution
  #' @param b (double) is the model hyperparameter for the beta distribution
  #'
  #' @return (double) returns a single value, which is the log P(N,Y)

  #Check if all the Ns are greater or equal to all Ys and make sure Ys non zero
  if (all(N>=Y[,1] & N>=Y[,2]) & all(Y>=0)){

    #Calculate estimator for a tilda and b tilda
    a_tilda <- a + sum(Y)
    b_tilda <- b + 2*sum(N) - sum(Y)

    #Calculate the log sum of Ns under the geomtric distribution with parameter xi
    log_p_N <- sum(dgeom(N, xi,log = TRUE))

    #Calculate the sum of the log of N choose Y for both columns of Y.
    log_choose_1 <- sum(lchoose(N,Y[,1]))
    log_choose_2 <- sum(lchoose(N,Y[,2]))

    #Sum all the values together
    lbeta(a_tilda, b_tilda) - lbeta(a,b) + log_p_N + log_choose_1 + log_choose_2
  }
  else{
    #Return -Inf if combination of Ns and Ys is not valid
    -Inf
  }
}

arch_importance <- function(K,Y,xi,a,b){
  #' Importance sampling function
  #'
  #' We construct a Bayesian credible interval for each Nj using importance
  #' sampling. In this case we have control over the number of K samples we
  #' generate.
  #'
  #' @param K (integer) number of samples to generate.
  #' @param Y (a data frame of size J×2) This is the total number of left and
  #'   right femurs found in each of the J excavations.
  #' @param xi (double) is the model hyperparameter for the geometric distribution
  #'   this value is between 0 and 1, otherwise will create NaNs.
  #' @param a (double) is the model hyperparameter for the beta distribution
  #' @param b (double) is the model hyperparameter for the beta distribution
  #'
  #' @return df_final (dataframe) returns Kx(J+2) dataframe, where K is the
  #'   number of rows and J+2 columns, where the first J containing samples from
  #'   the prior distributions for Nj , The second last column called "Log_Weights"
  #'   containing the re-normalised log-importance-weights, and the final column
  #'   "Phi" which contains samples from the conditional distribution
  #'   Beta(a_tilda, b_tilda) for each row of the importance sample.

  #Calculate J, that is the number of rows in Y
  J <- nrow(Y)

  #Create an K*J matrix for all the samples we will use from a geom dist
  N_s <- matrix(rgeom(K*J,xi),nrow = K, byrow = TRUE)
  #Update the column names such that they are called N1, ..., Nj
  colnames(N_s) <- paste("N", seq_len(ncol(N_s)), sep = "")

  #Place holder list for the K log weights we will calculate
  log_weights <- numeric(K)

  #Loop over all the rows of N_s, that is the samples N^[k]
  for (k in c(1:K)){
    #Get the kth row of N_s
    N_k <- N_s[k,]
    #Calculate the log weight by using log_prob_NY() and the sum of dgeom()
    log_weights[k] <- log_prob_NY(N_k,Y,xi,a,b) -
      sum(dgeom(N_k,xi,log = TRUE))
  }

  #Update the log_weights by calculating the re-normalised log-importance-weights
  #that is subtracting the max of the log_weights form all the log_weights
  log_weights <- log_weights - max(log_weights)

  #Creating a dataframe with the N_s and Log_Weights
  df_Ns_LW <- cbind(N_s, Log_Weights = log_weights)

  #Question 2.5
  #Creating a_tilda and b_tilda to sample Phi values
  a_tilda <- a + sum(Y)
  b_tilda <- b + 2*rowSums(N_s) - sum(Y)

  #Creating a dataframe (df_final) with the N_s, Log_Weights and Phi
  df_final <- cbind(df_Ns_LW, Phi = rbeta(K, a_tilda, b_tilda))

  #Return the final dataframe
  return(as.data.frame(df_final))
}

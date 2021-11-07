# We can avoid printing a lot of startup messages when
# loading a package like this:
suppressPackageStartupMessages(library(tidyverse))
library(StatCompLab)
library(ggplot2)

thename <- function(arguments) {
  expressions
}

my_function <- function(param1, param2 = NULL, param3 = 4) {
  # 'if': run a block of code if a logical statement is true
  if (is.null(param2)) {
    param1 + param3
  } else {
    # 'else', companion to 'if':
    #    run this other code if the logical statement wasn't true
    param1 + param2 / param3
  }
}

my_function(1)
my_function(1, param3 = 2)
my_function(1, param2 = 8, param3 = 2)
my_function(1, param2 = 8)

n <- 100
theta_true <- c(2, -2, -2, 3)
X <- cbind(1, seq(0, 1, length.out = n))
y <- rnorm(n = n,
           mean = X %*% theta_true[1:2],
           sd = exp(X %*% theta_true[3:4]))

ggplot(data.frame(x = X[, 2], y = y)) + 
  geom_point(aes(x, y))

neg_log_lik <- function(theta, y, X) {
  -sum(dnorm(x = y, #basecase letter for the RV in our case y
             mean = X %*% theta[1:2],
             sd = exp(X %*% theta[3:4]),
             log = TRUE))
}
neg_log_lik(theta_true,y,X)
opt <- optim(par = c(0, 0, 0, 0),
             fn = neg_log_lik, y = y, X = X,
             method = "BFGS")
opt

# If convergence = 0, then as a result this means that it has converted.

data <- data.frame(x = X[, 2],
                   y = y,
                   expectation_true = X %*% theta_true[1:2],
                   sigma_true = exp(X %*% theta_true[3:4]),
                   expectation_est = X %*% opt$par[1:2],
                   sigma_est = exp(X %*% opt$par[3:4]))

ggplot(data) +
  geom_line(aes(x, sigma_true)) +
  geom_line(aes(x, sigma_est), col = "red") +
  xlab("x") +
  ylab(expression(sigma))

# Data Wrangling 
suppressPackageStartupMessages(library(tidyverse))
data_long <-
  data %>%
  pivot_longer(cols = -c(x, y),
               values_to = "value",
               names_to = c("property", "type"),
               names_pattern = "(.*)_(.*)")

ggplot(data_long, aes(x, value)) +
  geom_line(aes(col = type)) +
  facet_wrap(vars(property), ncol=1)

opt <- optim(par = c(0, 0, 0, 0),
             fn = neg_log_lik, y = y, X = X,
             method = "BFGS",
             hessian = TRUE)
covar <- solve(opt$hessian)
covar

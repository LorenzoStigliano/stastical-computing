suppressPackageStartupMessages(library(tidyverse))
library(StatCompLab)
library(ggplot2)

K <- 10000
mu <- log(2) - 1/2
m <- 0:15
# How to make an empty array 
P_Y <- numeric(length(m))

for (k in seq_len(K))
{
  x <- rnorm(1,mean = 1,sd = 1)
  P_Y <- P_Y + dpois(m, lambda = exp(mu + x))
}

P_Y <- P_Y/K

# Plot the results
suppressPackageStartupMessages(library(ggplot2))
ggplot(data.frame(m = m,
                  P_Y = P_Y,
                  P_Poisson = dpois(m, lambda = 2))) +
  geom_point(aes(m, P_Y, col = "MC")) +
  geom_point(aes(m, P_Poisson, col = "Poisson")) +
  geom_line(aes(m, P_Y, col = "MC")) +
  geom_line(aes(m, P_Poisson, col = "Poisson")) 


doverpois <- function(m, mu, sigma, K) {
  P_Y <- numeric(length(m))
  for (loop in seq_len(K)) {
    x <- rnorm(1, sd = sigma)
    P_Y <- P_Y + dpois(m, lambda = exp(mu + x))
  }
  P_Y <- P_Y / K
  
  data.frame(m = m,
             P_Y = P_Y,
             P_Poisson = dpois(m, lambda = exp(mu + sigma^2/2)))
}
doverpois

suppressPackageStartupMessages(library(ggplot2))
ggplot(doverpois(m = 0:30, mu = log(8)-0.125, sigma = 0.5, K = 10000)) +
  geom_point(aes(m, P_Y, col = "MC")) +
  geom_point(aes(m, P_Poisson, col = "Poisson")) +
  geom_line(aes(m, P_Y, col = "MC")) +
  geom_line(aes(m, P_Poisson, col = "Poisson"))

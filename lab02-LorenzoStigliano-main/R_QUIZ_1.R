n <- 100
theta_true <- c(0, 0)
k <- cbind(1, c(1:n))
y <- rpois(n = n,
           lambda = exp(k %*% theta_true[1:2]))
y
negloglik <- function(theta, y) {
  -sum(dpois(x = y, 
             lambda = exp(cbind(1, c(1:length(y))) %*% theta[1:2]),
             log = TRUE))
}

negloglik(theta_true,y)

opt <- optim(par = c(0, 0), negloglik, y = y)
opt
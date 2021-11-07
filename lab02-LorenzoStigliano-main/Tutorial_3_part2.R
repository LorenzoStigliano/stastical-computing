gamma <- 1/(1+1000)
a <- 2
b <- 2

estimate <- function(y,xi,a,b,K){
  
  N <- rgeom(K, xi)
  phi <- rbeta(K, a, b)
  
  samples <- data.frame(N=N,phi=phi)
  
  loglike <- arch_loglike(param = samples, y = y)
  P_Y <- mean(exp(loglike))

  c(p_y = P_Y,
    E_N = mean(samples$N * exp(loglike) / P_Y),
    E_phi = mean(samples$phi * exp(loglike) / P_Y))
} 

estimate(y = c(237, 256), xi = 1/1001, a = 0.5, b = 0.5, K = 10000)
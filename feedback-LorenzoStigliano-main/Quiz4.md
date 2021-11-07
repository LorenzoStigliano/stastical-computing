Feedback for StatComp Quiz 4
================

Name: Lorenzo Stigliano (s1725018)

Submission time: 2021-03-20 16:33:22

Total marks (of maximum 5): 5

(Note: these marks are preliminary, and have not yet been checked for
possible late submission)

### Task label: Q-importance

Mark(s): 5

Generated feedback:

Your answer:

``` r
importance <- function(samples, log_weights) {
  
  weights = exp(log_weights- max(log_weights))
  normalized_weights = weights/sum(weights)
  
  mean = sum(samples*normalized_weights)
  sd = sqrt(sum((samples-mean)**2*normalized_weights))
  
  neff0 = sum(weights)^2/sum(weights^2)
  neff = sd**2/sum((samples-mean)**2*normalized_weights**2)
  
  if(all(samples==samples[1])){
    return(data.frame(mean = mean, sd = sd, neff0 = neff0, neff = NA))
  }
  else{
    return(data.frame(mean = mean, sd = sd, neff0 = neff0, neff = neff))
  }
}
```

Correct answer:

``` r
importance <- function(samples, log_weights) {
  weights <- exp(log_weights - max(log_weights))
  weights <- weights / sum(weights)
  m <- sum(weights * samples)
  out <- data.frame(
    mean = m,
    sd = sqrt(sum(weights * (samples - m)^2)),
    neff0 = 1 / sum(weights^2)
  )
  if (out$sd == 0) { # Or use length(unique(samples) == 1)
    out$neff <- NA_real_ # Or just NA, but NA_real_ is more precise
  } else {
    out$neff <- out$sd^2 / sum(weights^2 * (samples - m)^2)
  }
  out
}
```

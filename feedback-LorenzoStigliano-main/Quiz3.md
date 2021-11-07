Feedback for StatComp Quiz 3
================

Name: Lorenzo Stigliano (s1725018)

Submission time: 2021-03-04 15:51:49

Total marks (of maximum 5): 5

(Note: these marks are preliminary, and have not yet been checked for
possible late submission)

### Task label: Q-theory

Mark(s): 3

Generated feedback:

Your answer:

``` r
c(2,4)
```

Correct answer:

``` r
c(2, 4)
# 1. False: the randomness is in the observation y, which is used in both scores, making them dependent.
# 2. True: The squared error is minimised if and only if \mu_F = E_G(y).
# 3. False: This score can be made arbitrarily close to 0 by increasing \sigma_F, but S(G,G) = 1 > 0
#           The Dawid-Sebastiani score has a +\log(\sigma_F^2) term that fixes that problem.
# 4. True: The logarithmic score is strictly proper.
```

### Task label: Q-scores

Mark(s): 2

Generated feedback:

Your answer:

``` r
score_ds_diff <- function(pred_A, pred_B, obs) {
  a <- proper_score("ds", obs, mean = pred_A$mean, sd = pred_A$sd)
  b <- proper_score("ds", obs, mean = pred_B$mean, sd = pred_B$sd)
  return(a - b)
}
```

Correct answer:

``` r
score_ds_diff <- function(pred_A, pred_B, obs) {
  proper_score("ds", obs, mean = pred_A$mean, sd = pred_A$sd) -
    proper_score("ds", obs, mean = pred_B$mean, sd = pred_B$sd)
}
```

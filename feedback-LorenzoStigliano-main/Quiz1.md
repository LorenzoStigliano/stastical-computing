Feedback for StatComp Quiz 1
================

Name: Lorenzo Stigliano (s1725018)

Submission time: 2021-01-21 13:07:07

Total marks (of maximum 5): 5

(Note: these marks are preliminary, and have not yet been checked for
possible late submission)

### Task label: Q-ggplot

Mark(s): 1

Generated feedback:

Your answer:

``` r
3
```

Correct answer:

``` r
3
```

### Task label: add-function

Mark(s): 3

Generated feedback:

Your answer:

``` r
negloglik <- function(theta, y) {
  -sum(dpois(x = y,
             lambda = exp(cbind(1, c(1:length(y))) %*% theta[1:2]),
             log = TRUE))
}
negloglik
```

Correct answer:

``` r
negloglik <- function(theta, y) {
  n <- length(y)
  -sum(dpois(y, exp(theta[1] + seq_len(n) * theta[2]), log = TRUE))
}
```

### Task label: Q-fisher

Mark(s): 1

Generated feedback:

Your answer:

``` r
c(1,3,4)
```

Correct answer:

``` r
c(1, 3, 4)
```

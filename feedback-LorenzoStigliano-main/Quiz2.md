Feedback for StatComp Quiz 2
================

Name: Lorenzo Stigliano (s1725018)

Submission time: 2021-02-03 14:13:59

Total marks (of maximum 5): 5

(Note: these marks are preliminary, and have not yet been checked for
possible late submission)

### Task label: Q-theory

Mark(s): 3

Generated feedback:

Your answer:

``` r
c(2,3,6)
```

Correct answer:

``` r
c(2, 3)
# or c(2, 3, 6); the likelihood was intended to be twice differentiable
# 1. False: f(E(theta|y)) is not equal to E(f(theta)|y) for all functions f()
#           Counterexample: f(x) = exp(x) when x~N(mu,s^2)
#                           E(f(x)) = exp(mu + s^2/2)
#                           f(E(x)) = exp(mu)
# 2. True: When it is well-defined, the location of the maximum transforms in a simpe way
# 3. True: The remaining term in from Bayes' formula involves only p(y),
#          which doesn't depend on y, so the derivative with respect to
#          theta is zero
# 4. False: If the importance sampling distribution is not well-adapted
#          to the target distribution, the variance can become arbitrarily large
#          for importance sampling
# 5. False: The negated 2nd order derivative of the log-likelihood is not always non-negative.
# 6. False: The expectation of the negated 2nd order derivative of the log-likelihood
#           can be negative for some models.
#           Counterexample: y ~ Unif(0, theta), -d^2 log p(y|theta)/dtheta^2
#             p(y|theta) = I(0 < y < theta) / theta, for theta > y
#             - log p(y|theta) = log(theta) for theta > y
#             - d log p(y|theta) /dtheta= 1 / theta for theta > y
#             - d^2 log p(y|theta) /dtheta^2= - 1 / theta^2 for theta > y
#             and the ML estimate is at the boundary of the allowed space,
#             at theta = y.
#           The original intent of the question was to have log p(y|theta)
#           be twice differentiable and bounded, in which case the statement
#           would have been true!
#           Both False and True have therefore been accepted as answers.
```

### Task label: Q-integration

Mark(s): 2

Generated feedback:

Your answer:

``` r
my_integrator <- function(fun, lims, n, ...) {
  a = lims[1]
  b = lims[2]
  delta_x = (b-a)/n
  sum = 0
  for (k in c(1:n)){
    step = k - 1/2 
    sum = sum + fun(a + delta_x*step, ...)
  }
  delta_x*sum
}

my_integrator
```

Correct answer:

``` r
my_integrator <- function(fun, lims, n, ...) {
  x <- lims[1] + diff(lims) * ((seq_len(n) - 1/2) / n)
  w <- diff(lims) / n
  sum(w * fun(x, ...))
}
```

---
title:compwrap (Comparison Wrapper functions)
author: Toshiaki Ara
date: "2018-11-29"
---

# Introduction

This package contains two wrapper functions.

1. brunner.munzel.test in lawstat package
2. posthocTGH in userfriendlyscience package

These functions in compwrap package accept formula.

Also, this package provide a function for
 permuted Brunner-Munzel test
 in the case of small sample size.

# Installation


```r
library(devtools)
install_github("toshi-ara/compwrap")
```

# Example
## Brunner-Munzel test

```r
Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)

dat <- data.frame(
    value = c(Y, N),
    group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
                   levels = c("Y", "N"))
)

library(compwrap)

## Default
brunner.munzel.test(Y, N)
```

```
## 
## 	Brunner-Munzel Test
## 
## data:  x and y
## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
## 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y) 
##         0.788961
```

```r
## Formula interface
brunner.munzel.test(value ~ group, data = dat)
```

```
## 
## 	Brunner-Munzel Test
## 
## data:  value by group
## Brunner-Munzel Test Statistic = 3.1375, df = 17.683, p-value =
## 0.005786
## 95 percent confidence interval:
##  0.5952169 0.9827052
## sample estimates:
## P(X<Y)+.5*P(X=Y) 
##         0.788961
```

## permuted Brunner-Munzel test

`brunner.munzel.permutation.test` takes time to obtain results.


```r
library(compwrap)

Y <-  c(1,2,1,3,1,2,2,4,1,1)
N <-  c(3,4,5,2,3,2,1,5,4,3)

dat <- data.frame(
    value = c(Y, N),
    group = factor(rep(c("Y", "N"), c(length(Y), length(N))),
                   levels = c("Y", "N"))
)

## Default
brunner.munzel.permutation.test(Y, N)
```

```
## 
## 	Brunner-Munzel Permutation Test
## 
## data:  Y and N
## p-value = 0.02004
```

```r
## Formula interface
brunner.munzel.permutation.test(value ~ group, data = dat)
```

```
## 
## 	Brunner-Munzel Permutation Test
## 
## data:  value by group
## p-value = 0.02004
```

### Comparison with other methods


```r
# Wilcoxson sum-rank test
wilcox.test(value ~ group, data = dat)
```

```
## Warning in wilcox.test.default(x = c(1, 2, 1, 3, 1, 2, 2, 4, 1, 1), y =
## c(3, : cannot compute exact p-value with ties
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  value by group
## W = 20, p-value = 0.02196
## alternative hypothesis: true location shift is not equal to 0
```

```r
# exact Wilcoxson sum-rank test
library(coin)
wilcox_test(value ~ group, data = dat, distribution = "exact")
```

```
## 
## 	Exact Wilcoxon-Mann-Whitney Test
## 
## data:  value by group (Y, N)
## Z = -2.3299, p-value = 0.0207
## alternative hypothesis: true mu is not equal to 0
```

## Tukey test and Games-Howell test



### Tukey method


```r
## Default
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet, method="tukey")
```

```
##     n means variances
## 1 220   103      3210
## 2 120   123      5128
## 3 120   143      7489
## 4 118   135      4737
## 
##     diff ci.lo ci.hi    t  df    p
## 2-1 20.0  -0.3    40 2.54 574  .06
## 3-1 40.3  20.0    61 5.12 574 <.01
## 4-1 32.6  12.2    53 4.12 574 <.01
## 3-2 20.3  -2.7    43 2.27 574  .11
## 4-2 12.6 -10.5    36 1.41 574   .5
## 4-3 -7.7 -30.8    15 0.86 574  .83
```

```r
## Formula interface
posthocTGH(weight ~ Diet, data = ChickWeight, method="tukey")
```

```
##     n means variances
## 1 220   103      3210
## 2 120   123      5128
## 3 120   143      7489
## 4 118   135      4737
## 
##     diff ci.lo ci.hi    t  df    p
## 2-1 20.0  -0.3    40 2.54 574  .06
## 3-1 40.3  20.0    61 5.12 574 <.01
## 4-1 32.6  12.2    53 4.12 574 <.01
## 3-2 20.3  -2.7    43 2.27 574  .11
## 4-2 12.6 -10.5    36 1.41 574   .5
## 4-3 -7.7 -30.8    15 0.86 574  .83
```

### Games-Howell method (default)


```r
## Default
posthocTGH(y=ChickWeight$weight, x=ChickWeight$Diet)
```

```
##     n means variances
## 1 220   103      3210
## 2 120   123      5128
## 3 120   143      7489
## 4 118   135      4737
## 
##     diff  ci.lo ci.hi    t  df    p
## 2-1 20.0   0.36    40 2.64 201  .04
## 3-1 40.3  17.54    63 4.59 176 <.01
## 4-1 32.6  13.45    52 4.41 203 <.01
## 3-2 20.3  -6.20    47 1.98 230   .2
## 4-2 12.6 -10.91    36 1.39 236  .51
## 4-3 -7.7 -33.90    19 0.76 226  .87
```

```r
## Formula interface
posthocTGH(weight ~ Diet, data = ChickWeight)
```

```
##     n means variances
## 1 220   103      3210
## 2 120   123      5128
## 3 120   143      7489
## 4 118   135      4737
## 
##     diff  ci.lo ci.hi    t  df    p
## 2-1 20.0   0.36    40 2.64 201  .04
## 3-1 40.3  17.54    63 4.59 176 <.01
## 4-1 32.6  13.45    52 4.41 203 <.01
## 3-2 20.3  -6.20    47 1.98 230   .2
## 4-2 12.6 -10.91    36 1.39 236  .51
## 4-3 -7.7 -33.90    19 0.76 226  .87
```


---
title:compwrap (Comparison Wrapper functions)
author: Toshiaki Ara
date: "2019-03-03"
---

# Introduction

This package contains two wrapper function.

1. posthocTGH in userfriendlyscience package

This function in compwrap package accepts formula.

# Installation


```r
remote::install_github("toshi-ara/compwrap")
```

# Example
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


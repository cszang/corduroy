corduroy
=====================================================================

[![Travis-CI Build Status](https://travis-ci.org/cszang/corduroy.svg?branch=master)](https://travis-ci.org/cszang/corduroy)
![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

No-fuzz auto-unification of messed up coordinates.

## Installation

```r
devtools::install_github("cszang/corduroy")
```

## Usage

Example:

```r
library(corduroy)

messed <- data.frame(
  coords = c(
    "49°40'46.148\"N",
    "E 48.232°",
    "46°42'4\"",
    "34.2"))

messed$coords <- unify_coords(messed$coords)
```

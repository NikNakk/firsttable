
<!-- README.md is generated from README.Rmd. Please edit that file -->

# firsttable

firsttable package for R

This function takes a `data.frame` or `tibble` and a row and column
specification and generates a table along the lines of the first table
used in many medical journal articles. The row specification can either
use one of the `_row` functions, or if the defaults are appropriate can
just be a bare column name or calculation using a column. These
calculations are implemented using `rlang::eval_tidy` and support the
quasiquotation operators such as `!!` and `!!!`.

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/NikNakk/firsttable?branch=main&svg=true)](https://ci.appveyor.com/project/NikNakk/firsttable)
[![R-CMD-check](https://github.com/NikNakk/firsttable/workflows/R-CMD-check/badge.svg)](https://github.com/NikNakk/firsttable/actions)
[![Codecov test
coverage](https://codecov.io/gh/NikNakk/firsttable/branch/main/graph/badge.svg)](https://codecov.io/gh/NikNakk/firsttable?branch=main)
<!-- badges: end -->

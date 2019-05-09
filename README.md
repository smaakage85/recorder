
<!-- README.md is generated from README.Rmd. Please edit that file -->
recordr <img src="man/figures/logo.png" align="right" height=140/>
==================================================================

[![Travis-CI Build Status](https://travis-ci.org/smaakage85/customsteps.svg?branch=master)](https://travis-ci.org/smaakage85/recordr) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/modelgrid)](https://CRAN.R-project.org/package=recordr) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/modelgrid)](https://CRAN.R-project.org/package=recordr)

`recordr` is a lightweight toolkit to validate new observations when computing their predictions with a machine learning model. The validation process consists of two steps:

1.  record relevant statistics and meta data of the variables from the original training data for the machine learning model
2.  use these data to run a set of basic validation tests on the new set of observations.

Installation
------------

`recordr` can be installed from CRAN with `install.packages('recordr')`. If you want the development version then install directly from GitHub:

``` r
devtools::install_github("smaakage85/recordr")
```

Basics
------

Load package.

``` r
library(recordr)
```

Divide `iris` into training data and new data.

``` r
set.seed(1)
trn_idx <- sample(seq_len(nrow(iris)), 100)
data_training <- iris[trn_idx, ]
data_new <- iris[-trn_idx, ]
```

Record statistics and meta data of training data.

``` r
tape <- record(data_training)
#> 
#> [RECORD]
#> 
#> ... recording meta data and statistics of 5 columns and 100 rows... 
#> 
#> [STOP]
```

Run validation tests on new data with `play`.

``` r
playback <- play(tape, data_new)
#> 
#> [PLAY]
#> 
#> ... playing data.tape on new data with 5 columns and 50 rows ...
#> 
#> [STOP]
```

Print test results.

``` r
playback
#> 
#> [PLAY]
#> 
#> # of rows in new data: 50
#> # of rows passing all tests: 49
#> # of rows failing one or more tests: 1
#> 
#> Test results (failures):
#> > 'missing_variable': no failures
#> > 'mismatch_class': no failures
#> > 'mismatch_levels': no failures
#> > 'new_variable': no failures
#> > 'outside_range': Petal.Length[row(s): 11]
#> > 'new_level': no failures
#> > 'new_NA': no failures
#> > 'new_text': no failures
#> 
#> Test descriptions:
#> 'missing_variable': variable observed in training data but missing in new data
#> 'mismatch_class': 'class' in new data does not match 'class' in training data
#> 'mismatch_levels': 'levels' in new data and training data are not identical
#> 'new_variable': variable observed in new data but not in training data
#> 'outside_range': value in new data without recorded range in training data
#> 'new_level': new 'level' in new data compared to training data
#> 'new_NA': NA observed in new data but not in training data
#> 'new_text': new text in new data compared to training data
#> 
#> [STOP]
```

You can then extract the results of (any) failed tests for the rows of new data with `get_failed_tests()`:

``` r
failed_tests <- get_failed_tests(playback)
# print.
kable(head(failed_tests, 15))
```

| outside\_range.Petal.Length |
|:----------------------------|
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| TRUE                        |
| FALSE                       |
| FALSE                       |
| FALSE                       |
| FALSE                       |

If you to know more about all of the exciting features of `recordr`, take a look at the vignette.

Also, if you have any feedback on the package, please let me hear from you.

# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.3.2 (2016-10-31) |
|system   |x86_64, darwin13.4.0         |
|ui       |RStudio (1.0.44)             |
|language |(EN)                         |
|collate  |en_US.UTF-8                  |
|tz       |America/New_York             |
|date     |2016-11-19                   |

## Packages

|package       |*  |version |date       |source                                        |
|:-------------|:--|:-------|:----------|:---------------------------------------------|
|ccdrAlgorithm |*  |0.0.2   |2016-11-19 |local (itsrainingdata/ccdrAlgorithm@NA)       |
|Rcpp          |   |0.12.8  |2016-11-17 |cran (@0.12.8)                                |
|sparsebnUtils |*  |0.0.2   |2016-11-19 |Github (itsrainingdata/sparsebnUtils@db676a0) |

# Check results
1 packages with problems

## sparsebn (0.0.1)
Maintainer: Bryon Aragam <sparsebn@gmail.com>  
Bug reports: https://github.com/itsrainingdata/sparsebn/issues

0 errors | 1 warning  | 0 notes

```
checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Loading required package: sparsebnUtils
Loading required package: ccdrAlgorithm
Loading required package: discretecdAlgorithm

sparsebn v0.0.1, Copyright (c) 2016
	Bryon Aragam, University of California, Los Angeles
	Jiaying Gu, University of California, Los Angeles
... 6 lines ...
---> Bugs? Please report any bugs at https://github.com/itsrainingdata/sparsebn/issues.

A list of interventions was not specified: Assuming data is purely observational.
Note: method with signature 'ddiMatrix#dMatrix' chosen for function '-',
 target signature 'ddiMatrix#dtCMatrix'.
 "diagonalMatrix#triangularMatrix" would also be valid
A list of interventions was not specified: Assuming data is purely observational.
Quitting from lines 144-153 (sparsebn-vignette.Rmd) 
Error: processing vignette 'sparsebn-vignette.Rmd' failed with diagnostics:
could not find function "get.solution"
Execution halted
```


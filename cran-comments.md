## Test environments
* local OS X install, R 3.5.0
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)
* r-hub (devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## CRAN Package Check Results for Package ccdrAlgorithm 

From https://cran.rstudio.com/web/checks/check_results_ccdrAlgorithm.html

Version: 0.0.4
Check: for unstated dependencies in ‘tests’
Result: WARN
    '::' or ':::' import not declared from: ‘Matrix’
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc

This has been fixed.

## Dependencies

CHECK has been run on all dependencies and passed.

## Reverse dependencies

No reverse dependencies are affected by this update.

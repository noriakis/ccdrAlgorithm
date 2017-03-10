## Test environments
* local OS X install, R 3.3.3
* ubuntu 12.04.5 (travis-ci: oldrel, devel, and release)
* win-builder (devel and release)
* r-hub (oldrel, devel, and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## CRAN Package Check Results for Package ccdrAlgorithm 

From https://cran.rstudio.com/web/checks/check_results_ccdrAlgorithm.html

Version: 0.0.2
Check: compiled code
Result: NOTE
    File ‘ccdrAlgorithm/libs/ccdrAlgorithm.so’:
     Found no calls to: ‘R_registerRoutines’, ‘R_useDynamicSymbols’
    
    It is good practice to register native routines and to disable symbol
    search.
    
    See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual. 
    
This has been fixed.

Version: 0.0.2
Flags: --no-stop-on-test-error
Check: tests
Result: ERROR
     Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
     > library(testthat)
     > library(ccdrAlgorithm)
     >
     > test_check("ccdrAlgorithm")
     Error: 'cor_vector' is not an exported object from 'namespace:sparsebnUtils'
     testthat results ================================================================
     OK: 0 SKIPPED: 0 FAILED: 0
     Execution halted 
     
This is an issue caused by a recent update to the dependency `sparsebnUtils`, and is fixed in this update to `ccdrAlgorithm`.

## Dependencies

CHECK has been run on all dependencies and passed.

## Reverse dependencies

* sparsebn: You will receive updates to this package that fixes these issues very soon from myself. This package depends on the current submission for basic functionality.

//
//  Auxiliary.h
//  ccdr_proj
//
//  Created by Bryon Aragam on 3/20/14.
//  Copyright (c) 2014-2015 Bryon Aragam. All rights reserved.
//

#ifndef Auxiliary_h
#define Auxiliary_h

#include <random>

#include "defines.h"
#include "SparseBlockMatrix.h"

//------------------------------------------------------------------------------/
//   AUXILIARY FUNCTIONS FOR CCDR CODEBASE
//------------------------------------------------------------------------------/

//
// Various random functions that are needed in the main CCDr code. The sign function is a critical function for the
//  algorithm; the rest of the functions are mainly for testing in a C++ development environment (e.g Xcode), 
//  outside of R.
//
//

double sign(double x);                          // usual sign function

int rand_int(int max_int);                      // generate a random integer between 1 and max_int

SparseBlockMatrix randomSBM(int pp,             //
                            int nnz,            // generate a random SparseBlockMatrix
                            double coef);       //

std::vector<double> random_unif(int len);       // generate a random Uniform[0,1] vector of length len

std::vector<double> lambdaGrid(double maxlam,   //
                               double minlam,   // generate a grid of lambdas based on the usual log-scale
                               int nlam);       //

// 
// sign
//
//   Returns the usual sign function for a real number
//
double sign(double x){
	if(x > 0) return 1;
	else if(x < 0) return -1;
	else return 0;
}

//
// rand_int
//
//   Generate a random number between 1 and max_int
//
int rand_int(int max_int){
    return (rand() % max_int + 1);
}

//
// randomSBM
//
//   Generate a random object from the class SparseBlockMatrix by simply adding random elements
//
//   NOTE: This function does NOT necessarily return a DAG
//
SparseBlockMatrix randomSBM(int pp,
                            int nnz,
                            double coef){
    
    SparseBlockMatrix final = SparseBlockMatrix(pp);
    
    for(int k = 0; k < nnz; ++k){
        // generate a random row
        int row = 1;
        while(row < 2) row = rand_int(pp); // row must be > 1 for acyclicity, so we keep generating new random ints until we get one > 1
        
        // generate a random column (must be < row in order to guarantee acyclicity)
        int col = rand_int(row - 1);
        
        row--; col--; // re-index since we're using C++
        
        final.addBlock(row, col, coef, 0);
    }
    
    return final;
    
}

//
// random_unif
//
//   Generate a random integer between 1 and max_int (nothing fancy here)
//
std::vector<double> random_unif(int len){
    std::vector<double> c(len, 0);
    
    std::default_random_engine generator;
    std::uniform_real_distribution<double> distribution(0.0, 1.0);
    
    for(int i = 0; i < len; ++i){
        double x = distribution(generator);
        c[i] = x;
    }
    
    return c;
}

//
// lambdaGrid
//
//   Generate a sequence / grid of lambdas on the standard log-scale for the CCDr algorithm. The scale is determined
//     by the maximum and minimum desired values and the length of the desired grid: nlam values are chosen, decreasing
//     on a log-scale, in the interval [minlam, maxlam] (inclusive)
//
//   NOTE: The typical use of this function uses the choices maxlam = sqrt(n) and minlam = rlam*maxlam, where rlam is some
//          ratio (typically around 0.001-0.1). These defaults are NOT enforced by this function and need to be inserted
//          via the calling code.
// 
//
std::vector<double> lambdaGrid(double maxlam, double minlam, int nlam){
    double fmlam = maxlam / minlam;             // naming convention is consistent with legacy R code
    double delta = log(fmlam) / (nlam - 1.0);   // the difference between successive lambda values is based on a log-scale
    
    std::vector<double> lambda_grid(nlam, 0);
    lambda_grid[0] = maxlam;
    for(int i = 1; i < nlam; ++i){
        lambda_grid[i] = log(lambda_grid[i-1]) - delta;
        lambda_grid[i] = exp(lambda_grid[i]);
    }
    
    #ifdef _DEBUG_ON_
        // the final value should be = minlam, if it isn't something went wrong in the calculations
        if(fabs(lambda_grid[nlam - 1] - minlam) > 1e-6){
            OUTPUT << "Whoa lambdaGrid didn't work! last = " << lambda_grid[nlam - 1] << " minlam = " << minlam << std::endl;
            FILE_LOG(logERROR) << "Whoa lambdaGrid didn't work! last = " << lambda_grid[nlam - 1] << " minlam = " << minlam;
        }
    #endif
    
    return lambda_grid;
}

#endif

//
//  defines.h
//  ccdr_proj
//
//  Created by Bryon Aragam on 5/1/14.
//  Copyright (c) 2014-2015 Bryon Aragam. All rights reserved.
//

#ifndef defines_h
#define defines_h

//------------------------------------------------------------------------------/
//   CCDR GLOBAL DEFINES
//------------------------------------------------------------------------------/

//
// THIS HEADER SHOULD BE LOADED FIRST IN ALL FILES!!
//
// In order to maintain consistency across the codebase, all of the main preprocessor
//   directive and defines are restricted to this file. In addition, any includes that
//   depend on one of these directives are included here.
//
// The two main defines are:
//
//    1) _DEBUG_ON_ : When defined, debugging code is activated and the log file is
//                    written to.
//    2) _COMPILE_FOR_RCPP_ : When defined, the assumption is that Rcpp is compiling
//                            the code through R. As a result, the log file is completely
//                            disabled, output is redirected to R, and the Rcpp.h header
//                            is loaded.
//
//

#define _DEBUG_ON_
#undef _DEBUG_ON_

#define _COMPILE_FOR_RCPP_
//#undef _COMPILE_FOR_RCPP_

#ifdef _DEBUG_ON_
    #include <string>
    #include <fstream>
    #include <sstream>
    #include <iomanip>
#endif

// Include the Rcpp header and redirect output to R if we are compiling using Rcpp
#ifdef _COMPILE_FOR_RCPP_
    #include <Rcpp.h>
    #define OUTPUT Rcpp::Rcout
    #define ERROR_OUTPUT Rcpp::Rcerr

    // Old code that disabled FILE_LOG and debugging has been removed; works fine with Rcpp
#else
    #define OUTPUT std::cout
    #define ERROR_OUTPUT std::cerr
#endif

#endif

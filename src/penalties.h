//
//  penalties.h
//  ccdr_proj
//
//  Created by Bryon Aragam on 6/18/14.
//  Copyright (c) 2014-2015 Bryon Aragam. All rights reserved.
//

#ifndef penalties_h
#define penalties_h

#include <math.h>

#include "defines.h"

//------------------------------------------------------------------------------/
//   PENALTY FUNCTION DEFINITIONS
//------------------------------------------------------------------------------/

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

double MCPPenalty(double b, double lambda, double gamma){
    if(b < gamma * lambda)
        return lambda * (b - 0.5 * b * b / (gamma * lambda));
    else
        return 0.5 * lambda *lambda * gamma;
}

double MCPThreshold(double z, double lambda, double gamma){
    if(fabs(z) <= lambda){
        return 0;
    } else if(lambda < fabs(z) && fabs(z) <= lambda * gamma){
        return sign(z) * gamma * (fabs(z) - lambda) / (gamma - 1.0);
    } else if(fabs(z) > lambda * gamma){
        return z;
    }

    FILE_LOG(logERROR) << "There was a problem calculating the threshold function: z = " << z;

    return 0;
}

double LassoPenalty(double b, double lambda, double gamma = 0){
    return lambda * b;
}

double LassoThreshold(double z, double lambda, double gamma = 0){
    if(fabs(z) <= lambda){
        return 0;
    } else {
        return sign(z) * (fabs(z) - lambda);
    }

    FILE_LOG(logERROR) << "There was a problem calculating the threshold function: z = " << z;

    return 0;
}
#endif

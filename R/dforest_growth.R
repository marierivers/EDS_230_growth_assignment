
#' Forest Growth Derivative

#' Parameters
#' @param Time years
#' @param C forest size, units of kg carbon
#' @param parms list of all parameters:
#'              r - exponential growth rate used before canopy closure has been reached; units of kg Carbon per year
#'              g - linear growth rate used once canopy closure has been reached; units of kg Carbon per year
#'              K - carrying capacity, units of carbon
#'              canopy_threshold - canopy closure threshold; the size of the forest 
#'                  at which growth rates change from exponential to linear
#'
#' @return list of forest growth rates

dforest_growth = function(Time, C, parms) {
  
  if (C < parms$canopy_threshold) {
    # early exponential growth rate (before canopy closure has been reached)
    dC_dt <- parms$r * C
  } else if (C >= parms$canopy_threshold) {
    # linear growth rate once canopy closure has been reached
    dC_dt <- parms$g * (1 - C/parms$K)
  }
  
  return(list(dC_dt))
  
}

# name derivative functions starting with d to remind yourself that they are computing a derivative
# Note: ODE solver in R wants a function that returns the derivative as a list
# inputs time, the variable(s) and a parameter list
# function, have independent variable first, dependent variable second, and parameter list last
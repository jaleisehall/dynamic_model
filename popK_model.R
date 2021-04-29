#'  Dynamic Population Growth Model w/ Carrying Capacity K
#' @param time time
#' @param P initial population
#' @param parms$r growth rate 
#' @param parms$K carrying capacity
#' @return change in population
#' @examples use with ode solver
#' ode(y = initial_pop, time = time, func = popK_model, parms = parms)

popK_model = function(time, P, parms) { 
  
  #function with the carrying capacity and growth rate set as parameters 
  popK_model = parms$r*P*(1-(P/parms$K))
  
  #add the carrying capacity (K)
  popK_model = ifelse(P > parms$K, 0, popK_model)
  
  #return the population for the given time step
  return(list(popK_model)) 
}
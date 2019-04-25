Calc.Xi <-
function(xi, skewness){
  return((2*(1+xi)*sqrt(1-2*xi))/(1-3*xi) - skewness)
}

Calc.Sigma <-
function(sigma, xi, variance){
  return((sigma^2)/(((1-xi)^2)*(1-2*xi))-variance)
}

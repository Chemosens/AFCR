#'@description calculates a group threshold from lawless method (2010)
#'@param numericConcentrations increasing numeric concentrations (not loggued)
#'@param triangularMatrix a triangular matrix from getTriangularMatrix
#'@export
lawlessCorrection=function(numericConcentrations, triangularMatrix,p_chance=1/3)
{
  pd=apply(triangularMatrix, 2, sum)/nrow(triangularMatrix)
  p_chance=1/3
  pd_adj=(pd-p_chance)/(1-p_chance)
  model=lm(log(pd_adj/(1-pd_adj),base=10)~log(numericConcentrations,base=10))
  plot(log(numericConcentrations,base=10),log(pd_adj/(1-pd_adj),base=10))
  lines(log(numericConcentrations,base=10),model$coefficients[1]+model$coefficients[2]*log(numericConcentrations,base=10))
  abline(h=0)
  print("ok")
  print(model$coefficient)
  abline(v=-model$coefficient[1]/model$coefficient[2])
  results=list(p50=-model$coefficient[1]/model$coefficient[2])
  return(results)
}

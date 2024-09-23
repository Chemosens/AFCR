#'@export

geom_avg=function(conc)
{
  ga=rep(NA,length(conc)-1)
  for(i in 2:length(conc))
  {
    ga[i-1]=sqrt(conc[i-1]*conc[i])
  }
  return(ga)
}
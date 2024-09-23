cumSumDroite=function(x)
{ #x=c(2,1,1,NA,4, NA, NA,1,1) ;cumSumDroite(x)
  #y=c(4,2,1,NA,4,NA,NA,2,1)
  y=x
  for(i in (length(x)-1):1)
  { 
    if(!is.na(y[i]))
    {
      if(!is.na(y[i+1]))
      {
        y[i]=y[i]+y[i+1]
      }
    }
  }
  return(y)
}

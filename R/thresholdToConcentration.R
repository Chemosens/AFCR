#' @export
thresholdToConcentration=function(threshold,decreasingNumConcentrations,minConc=0,maxConc=NULL)
{ 
  if(is.null(maxConc)){stop("please enter correct maxConc parameter (not null)")}
  if(is.null(minConc)){stop("please enter correct minConc parameter (not null)")}
  
  if(is.null(decreasingNumConcentrations)){stop("decreasingNumConcentrations is required")}
 
  J=length(decreasingNumConcentrations)
  if(threshold==0)
  {
    thresholdNum=(minConc+decreasingNumConcentrations[J])/2
  }
  else
  { 
    if(threshold==J)
    {thresholdNum=(1/2)*(decreasingNumConcentrations[1] + maxConc)}
    else{
    conc=decreasingNumConcentrations[J-threshold];
    toAdd=decreasingNumConcentrations[J-threshold+1]
    thresholdNum=(1/2)*(conc + toAdd)
    }
  }
  return(thresholdNum)
}
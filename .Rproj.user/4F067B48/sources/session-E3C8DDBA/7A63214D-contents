#' returns the threshold (=0 if the subject found the right samples for all concentrations, 1 if he/she found the right samples for all concentrations but the lightest one, etc.
#' @param df result of keepLastOccurrence
#' @param decreasingConcentrations vector contaning the products corresponding to the decreasing concentrations
#'  @export
getThreshold=function(df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
{
  products=levels(factor(df[,"Produit"]))
  if(!all(decreasingConcentrations%in%products)){stop("One decreasingConcentrations is not in the products")}
  subjects=levels(factor(df[,"Panéliste"]))
  observedThreshold=rep(NA,length(subjects));names(observedThreshold)=subjects
  for(suj in subjects)
  {
    #print(suj)
    dataSuj=df[df[,"Panéliste"]==suj,"Res"]
    names(dataSuj)=df[df[,"Panéliste"]==suj,"Produit"]
    dataSujOrdered=dataSuj[decreasingConcentrations]
    threshold=0
    i=1
    continue=TRUE
    while(i<length(decreasingConcentrations)+1&continue)
    {
      if(dataSujOrdered[decreasingConcentrations[i]]=="OK")
      {
       # print("Succeed")
        i=i+1
      }
      else
      {
        continue=FALSE
      }
    }
    observedThreshold[suj]=i-1

  }
  return(length(decreasingConcentrations)-observedThreshold)
}

#' returns the threshold (=0 if the subject found the right samples for all concentrations, 1 if he/she found the right samples for all concentrations but the lightest one, etc.
#' @param res result of keepLastOccurrence (or dataframe)
#' @inheritParams keepLastOccurence
#' @param decreasingConcentrations vector contaning the products corresponding to the decreasing concentrations
#' @export
#' @examples
#' data(triangular)
#' res=keepLastOccurence(triangular,subjectName="Paneliste")
#' seuils=getThreshold(res=res,
#' decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))

getThreshold=function(res,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res")
{
  if(inherits(res,"afc"))
  {
    productName=res$productName
    subjectName=res$subjectName
    descriptorName=res$descriptorName
    timeName=res$ timeName
    resName="Res"
    df=res$df
  }
  if(inherits(res,"data.frame"))
  {
    df=res
  }
  products=levels(factor(df[,productName]))
 # print(products)
  if(!all(decreasingConcentrations%in%products)){stop("One decreasingConcentrations is not in the products")}
  subjects=levels(factor(df[,subjectName]))
  observedThreshold=rep(NA,length(subjects));names(observedThreshold)=subjects
  for(suj in subjects)
  {
    #print(suj)
    dataSuj=df[df[,subjectName]==suj,resName]
    names(dataSuj)=df[df[,subjectName]==suj,productName]
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

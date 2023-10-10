#' getThreshold
#'
#' getThreshold returns the observed sensitivity thresholds of the subjects (=0 if the subject found the right sample for all concentrations, 1 if he/she found the right sample for all concentrations but the lightest one, etc.
#' @param resName name of the column containing the result "OK" or "KO" for the test.
#' @param res result of keepLastOccurrence (or dataframe)
#' @param rata data.frame containing columns named as productName, subjectName and 'Score' containing a score attributed to the test
#' @inheritParams keepLastOccurence
#' @param decreasingConcentrations vector contaning the products corresponding to the decreasing concentrations
#' @export
#' @examples
#' data(triangular)
#' res=keepLastOccurence(triangular,subjectName="Paneliste")
#' seuils=getThreshold(res=res,
#' decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
#' data(rata)
#' seuils=getThreshold(res=res,
#' decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),rata=rata)
getThreshold=function(res,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",rata=NULL,decreasingNumConcentrations=NULL)
{
  if(inherits(res,"afc"))
  {
    productName=res$productName
    subjectName=res$subjectName
    descriptorName=res$descriptorName
    timeName=res$timeName
    resName="Res"
    df=res$df
  }
  if(inherits(res,"data.frame"))
  {
    df=res
  }
  if(!is.null(rata))
  {
    if(!subjectName%in%colnames(rata)){stop("subjectName is not in rata colnames")}
    if(!productName%in%colnames(rata)){stop("productName is not in rata colnames")}
    if(!"Score"%in%colnames(rata)){stop("'Score' should be in rata colnames")}
    df2=merge(df,rata,by.x=c(subjectName,productName),by.y=c(subjectName,productName))
    df=df2[,c(subjectName,productName,resName,timeName,"nClicks","Score.y")]
    colnames(df)=c(subjectName,productName,resName,timeName,"nClicks","score")
  }
  products=levels(factor(df[,productName]))
 # print(products)
  if(!all(decreasingConcentrations%in%products)){stop("One decreasingConcentrations is not in the products")}
  subjects=levels(factor(df[,subjectName]))
  observedThreshold=rep(NA,length(subjects));names(observedThreshold)=subjects
  dfLastSucceed=data.frame()
  for(suj in subjects)
  {
    dataSuj=df[df[,subjectName]==suj,]
    # if(!is.null(decreasingNumConcentrations))
    # {
    #   dataSuj[,"avg"]=dataSuj[,]+dataSuj[,]/2
    # }
    rownames(dataSuj)=df[df[,subjectName]==suj,productName]
    dataSujOrdered=dataSuj[decreasingConcentrations,]
    threshold=0
    i=1 # concentration index
    continue=TRUE
    while(i<length(decreasingConcentrations)+1&continue)
    {
      if(dataSujOrdered[decreasingConcentrations[i],resName]=="OK")
      {
       # print("Succeed")
        i=i+1
      }
      else
      {
        if(i>1)
        {
          infoToRemember=dataSujOrdered[decreasingConcentrations[i-1],]
        }
        if(i==1)
        {
          infoToRemember=dataSujOrdered[decreasingConcentrations[i],]
        }
  #      print(infoToRemember)
        continue=FALSE
      }
    }
    observedThreshold[suj]=i-1
  # print(observedThreshold)
    if(observedThreshold[suj]==length(decreasingConcentrations))
    {
      infoToRemember=dataSujOrdered[decreasingConcentrations[length(decreasingConcentrations)],]
    }

    dfLastSucceed=rbind(dfLastSucceed,infoToRemember)
  }
  threshold=length(decreasingConcentrations)-observedThreshold
  dfres=cbind(dfLastSucceed,threshold)
  if(!is.null(decreasingNumConcentrations))
  {
    thresholdNum=rep(NA,length(threshold))
    for(i in 1:length(observedThreshold))
    {
      print(observedThreshold[i])
      conc=decreasingNumConcentrations[observedThreshold[i]]
      if(observedThreshold[i]==0)
      {
        thresholdNum[i]=decreasingNumConcentrations[1]
      }
      else
      {
        if(observedThreshold[i]==length(decreasingConcentrations))
        {toAdd=conc}
        else{toAdd=decreasingNumConcentrations[observedThreshold[i]+1]}
        thresholdNum[i]=(1/2)*(conc + toAdd)
        # if(!is.null(rata))
        # {
        #   relatedScores[i]=1/2()
        # }

      }

    }
    dfres=cbind(dfres,thresholdNum=thresholdNum)
  }

  if(!is.null(rata)){
    namesCol=c(subjectName,productName,timeName,"nClicks",resName,"threshold","score")
  }else{
      namesCol=c(subjectName,productName,timeName,"nClicks",resName,"threshold")
  }
  if(!is.null(decreasingNumConcentrations))
  {
    namesCol=c(namesCol,"thresholdNum")
  }
  return(dfres[,namesCol])
}

#' @export
getIntensityThreshold=function(intensityData,subjectName="Panéliste",scoreName="Score",productName="Produit",decreasingConcentrations=NULL,decreasingNumConcentrations=NULL, minConc=NULL,maxConc=NULL)
{
  subjects=levels(factor(intensityData[,subjectName]))
  J=length(decreasingConcentrations)
  threshold=rep(NA,length(subjects));names(threshold)=subjects
  for(subject in subjects)
  {
    intensityDatai=intensityData[intensityData[,subjectName]==subject,]
    scores=intensityDatai[,scoreName]
    print(scores)
    scores=as.numeric(scores)
    names(scores)=intensityDatai[,productName]
    print(scores)
    print(intensityDatai[,productName])
    print(decreasingConcentrations)
    reorderedScores=scores[decreasingConcentrations]
    print(reorderedScores)
    difference=diff(reorderedScores)
    if(all(difference<=0)){thresholdIndex=0}
    else{
      if(difference[1]>0){thresholdIndex=J}
      else
      {
        indexLastDecrease=which(difference>0)[1]
        # if equality after positive, choose the moment of strict rising
        increasing=difference[1:(indexLastDecrease-1)]
        lastIncreasing=increasing[length(increasing)]
        
        while(lastIncreasing==0)
        {
          increasing=increasing[-length(increasing)]
          lastIncreasing=increasing[length(increasing)]
        }
        indexToKeep=length(increasing)
        thresholdIndex=J-indexToKeep
      }
    }

    if(!is.null(decreasingNumConcentrations))
    {
      thresholdNum=thresholdToConcentration(thresholdIndex,decreasingNumConcentrations,minConc,maxConc)
      threshold[subject]=thresholdNum
    }
    else
    {
      threshold[subject]=thresholdIndex
    }
  }
  
  return(threshold)
}
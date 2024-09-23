#' @export
getJumpThreshold=function(intensityData,subjectName="Panéliste",scoreName="Score",productName="Produit",decreasingConcentrations=NULL,decreasingNumConcentrations=NULL, minConc=NULL,maxConc=NULL,tolerance=0)
{
  subjects=levels(factor(intensityData[,subjectName]))
  J=length(decreasingConcentrations)
  threshold=rep(NA,length(subjects));names(threshold)=subjects
  for(subject in subjects)
  {
    intensityDatai=intensityData[intensityData[,subjectName]==subject,]
    scores=intensityDatai[,scoreName]
    scores=as.numeric(scores)
    names(scores)=intensityDatai[,productName]
    reorderedScores=scores[decreasingConcentrations]
    reorderedScores=c(reorderedScores[1],reorderedScores,reorderedScores[length(reorderedScores)])
    difference=diff(reorderedScores)
    thresholdIndex=length(decreasingConcentrations)-which.min(difference)+1
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
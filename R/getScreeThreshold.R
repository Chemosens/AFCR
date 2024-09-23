#' @export
getScreeThreshold=function(intensityData,subjectName="Panéliste",scoreName="Score",productName="Produit",decreasingConcentrations=NULL,decreasingNumConcentrations=NULL, minConc=NULL,maxConc=NULL)
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
    difference[difference>0]=0 # on remplace par zero les pentes positives en concentrations décroissantes )
    difference0=diff(difference)
    thresholdIndex=length(decreasingConcentrations)-which.max(difference0)+1
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
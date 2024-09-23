#' @export
getIBT=function(intensityData,subjectName="Panéliste",scoreName="Score",productName="Produit",decreasingConcentrations=NULL,decreasingNumConcentrations=NULL, minConc=NULL,maxConc=NULL)
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
    reorderedScores=rev(scores[decreasingConcentrations])
    difference=diff(reorderedScores)
    names(difference)=paste0(names(reorderedScores)[-1],"-",names(reorderedScores)[-length(reorderedScores)])
    difference2=difference
    difference2[difference2<=0]=NA
    vec=cumSumDroite(difference2)
    ind=which.max(vec)[1]
    # Decreasing concentrations, the intensities are compared. 
    # If Ick+1 > Ick 
    # i=1
    # saved_highest_decreasing=highest_decreasing=0
    # while(i<length(decreasingConcentrations)) #from the decreasing concentrations
    # {
    #   print(paste0("i=",i))
    #   score_i0=reorderedScores[i]
    #   score_i=score_i0
    #   score_nexti=reorderedScores[i+1]
    #   if(score_nexti>=score_i){i=i+1;print("je passe")}
    #   else
    #   { print("tant que c'est décroissant on continue à compter")
    #     while(score_nexti<score_i & i<length(decreasingConcentrations))
    #     {
    #       score_i=reorderedScores[i]
    #       score_nexti=reorderedScores[i+1]
    #       highest_decreasing=score_i0-score_nexti
    #       if(highest_decreasing>=saved_highest_decreasing)
    #       {
    #         print("highest")
    #         print(highest_decreasing)
    #         saved_highest_decreasing=highest_decreasing
    #         thr=i
    #       }
    #       i=i+1
    #     }
    #   }
    # }
    thresholdIndex=ind
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
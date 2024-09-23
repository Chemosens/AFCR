#' @inheritParams getThreshold
#' @description This function calculates an intensity threshold based on the first moment when the scores are only increasing in the data (in increasing concentrations). An increasing is defined up to a tolerance. A tolerance of 0 returns the moment when the is (not strictly increasing), a tolerance of 1 allows a small decreasing of 1.
#' @param tol O by default. If scaleBySubject=TRUE, the tolerance is tol. If scaleBySubject=TRUE, the tolerance is tol*(max(score_of_this_subject)-min(score(score_of_this_subject))
#' @param intensityData data containing columns for subjectName (containing the name of the subject),scoreName (containing the intensity) and productName (containing the name of the concentration)
#' @param scaleBySubject if TRUE the tolerance is calculated by subject (as a proportion of the distance max-min of the scale)
#' @title getIntensityThreshold
#' @export
getIntensityThreshold=function(intensityData,subjectName="Panéliste",scoreName="Score",productName="Produit",decreasingConcentrations=NULL,decreasingNumConcentrations=NULL, minConc=NULL,maxConc=NULL,tol=0,scaleBySubject=TRUE)
{
  intensityData[scoreName]=as.numeric(as.character(intensityData[,scoreName]))
  subjects=levels(factor(intensityData[,subjectName]))
  J=length(decreasingConcentrations)
  threshold=rep(NA,length(subjects));names(threshold)=subjects
  significance=matrix(NA,length(subjects),J-1);rownames(significance)=subjects;
  colnames(significance)=paste0(decreasingConcentrations[-1],"-",decreasingConcentrations[-length(decreasingConcentrations)])
  for(subject in subjects) # The threshold is calculated by subject
  {

    intensityDatai=intensityData[intensityData[,subjectName]==subject,]
    scores=intensityDatai[,scoreName]
    if(scaleBySubject)
    {
      etendue=max(scores,na.rm=T)-min(scores,na.rm=T)
      tolerance=tol*etendue
    }
    else
    {
      tolerance=tol
    }
 
    scores=as.numeric(scores)
    names(scores)=intensityDatai[,productName]
    reorderedScores=scores[decreasingConcentrations] # scored are ordered by concentrations
    difference=diff(reorderedScores) # The difference should be negative (previous score higher than one) or null
    # The threshold is the first concentration where the difference become negative or null (or inferior to a tolerance)
    significance[subject,]=isSignif(difference,tolerance)
    if(all(difference<=tolerance))
    { # If all differences are negative or null (subject coherent)
      if((difference[length(difference)])<(-tolerance)){print("in");thresholdIndex=0} # if the difference is stritcly negative for the lightest concentration, the threshold is zero
      else{ 
        
        lastNull=J #if the difference is null for the lightest concentration, the next strictly negative is detected
        continue=T
        thresholdIndex=1
        while(continue)
        {
          if(difference[lastNull-1]>=(-tolerance))
          {
            lastNull=lastNull-1
            thresholdIndex=thresholdIndex+1
            if(thresholdIndex==J){continue=FALSE}
          }
          else
          {
            continue=FALSE
          }
        }
      }
    }
    else{ # if the subject is unconsistent (one positive difference)
      if(difference[1]>tolerance){thresholdIndex=J} # if the highest concentration is found lighter than the others, the theshold is J
      else
      {
        indexLastDecrease=which(difference>tolerance)[1] # highest concentration with strictly positive difference
        print(indexLastDecrease)
        # if equality after positive, choose the moment of strict rising
        increasing=difference[1:(indexLastDecrease-1)]
        print(increasing)
        lastIncreasing=increasing[length(increasing)]
        continue=T
        while(lastIncreasing>=(-tolerance)&continue) # Each equality 
        {
          increasing=increasing[-length(increasing)]
          if(length(increasing)==0){continue=FALSE}
          else{
            lastIncreasing=increasing[length(increasing)]
          }
         
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
  res=list(threshold=threshold,significance=significance,data=intensityData,decreasingConcentrations=decreasingConcentrations, decreasingNumConcentrations=decreasingNumConcentrations,minConc=minConc,maxConc=maxConc,tol=tol,scaleBySubject=scaleBySubject,subjectName=subjectName,scoreName=scoreName,productName=productName)
  return(res)
}
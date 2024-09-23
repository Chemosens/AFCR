#' @param resIntensity
#' @param subject name of the subject to be plotted
#' @param displayAFC should the result of AFC be displayed. 
#' @export
#' @importFrom ggplot2 scale_color_manual 
plotIntensityThresholdBySubject=function(resIntensity,subject,
                                         displayAFC=FALSE,revertX=FALSE)
{
  decreasingNumConcentrations=resIntensity$decreasingNumConcentrations
  decreasingConcentrations=resIntensity$decreasingConcentrations
  scoreName=resIntensity$scoreName
  subjectName=resIntensity$subjectName
  productName=resIntensity$productName
  minConc=resIntensity$minConc
  maxConc=resIntensity$maxConc
  resIntensityS=resIntensity$data[resIntensity$data[,subjectName]==subject,]
  score=concentration=concentration2=Res=NULL
  colnames(resIntensityS)[which(colnames(resIntensityS)==productName)]="concentration"
  colnames(resIntensityS)[which(colnames(resIntensityS)==scoreName)]="score"
  
  resIntensityS[,"concentration"]=factor(resIntensityS[,"concentration"])
  if(!revertX)
  {
    resIntensityS[,"concentration"]=fct_relevel(resIntensityS[,"concentration"],decreasingConcentrations)
  }
  if(revertX& is.null(decreasingNumConcentrations))
  {
    resIntensityS[,"concentration"]=fct_relevel(resIntensityS[,"concentration"],rev(decreasingConcentrations))
  }
  
  if(!is.null(decreasingNumConcentrations))
  {
    resIntensityS[,"concentration2"]=resIntensityS[,"concentration"]
    correspondance=decreasingNumConcentrations
    names(correspondance)=decreasingConcentrations
    resIntensityS[,"concentration2"]=correspondance[as.character(resIntensityS[,"concentration2"])]
  }
  if(!is.null(decreasingNumConcentrations))
  {
    concentration=resIntensityS[,"concentration2"]
    score=resIntensityS[,"score"]
    segmentsToPlot=data.frame(x0=concentration[-length(concentration)],x1=concentration[-1],y0=score[-length(score)],y1=score[-1],sig=rev(resIntensity$significance[subject,]))
    p=ggplot(segmentsToPlot,aes(x=x1,xend=x0,y=y1,yend=y0, color=sig))+scale_color_manual(values=c("red"="red","green"="green","black"="black"))+geom_segment()+theme_bw()+ggtitle(paste0("Scores according to concentrations ",subject))+geom_vline(xintercept=resIntensity$threshold[[subject]],col="blue")
  }
  else
  {
    concentration=resIntensityS[,"concentration"]
    score=resIntensityS[,"score"]
    segmentsToPlot=data.frame(x0=concentration[-length(concentration)],x1=concentration[-1],y0=score[-length(score)],y1=score[-1],sig=rev(resIntensity$significance[subject,]))
    segmentsToPlot[,"x0"]=fct_relevel( segmentsToPlot[,"x0"],decreasingConcentrations)
    segmentsToPlot[,"x1"]=fct_relevel( segmentsToPlot[,"x1"],decreasingConcentrations)
    p=ggplot(segmentsToPlot,aes(x=x1,xend=x0,y=y1,yend=y0, color=sig))+scale_color_manual(values=c("red"="red","green"="green","black"="black"))+geom_segment()+theme_bw()+ggtitle(paste0("Scores according to concentrations ",subject))+geom_vline(xintercept=length(concentration)-resIntensity$threshold[[subject]]+0.5,col="blue")
  }
  return(p)
}
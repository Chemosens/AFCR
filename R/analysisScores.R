#' analyseSores
#' Returns the plots with the scores according to their concentrations
#' @inheritParams getThreshold
#' @param scoreName name of the column containing the scores in rata data.frame
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 geom_line theme_bw ggtitle aes ggplot
#' @export
#' @examples
#' data(rata)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
analyseScores=function(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",scoreName="Score")
{
  subject=score=concentration=NULL
  rata2=rata[,c(productName,subjectName,scoreName)]
  colnames(rata2)=c("concentration","subject","score")
  rata2[,"concentration"]=factor(rata2[,"concentration"])
  rata2[,"concentration"]=fct_relevel(rata2[,"concentration"],decreasingConcentrations)
  p=ggplot(rata2,aes(x=concentration,y=score,group=subject,color=subject))+geom_line()+theme_bw()+ggtitle("Scores according to concentrations")

  # res=keepLastOccurence(triangular,subjectName=subjectName)
  # thr=getThreshold(res,decreasingConcentrations=decreasingConcentrations,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName,resName=resName,rata=rata)
  # thr2=thr[,c(productName,subjectName,scoreName)]
  # colnames(thr2)=c("concentration","subject","score")
  # p+geom_point(x=thr2[,"concentration"],y=thr2[,"score"],col=thr2[,"subject"])
  return(p)
}
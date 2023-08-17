#' analyseSores
#'
#' Returns the plots with the scores according to their concentrations
#' @inheritParams getThreshold
#' @inheritParams keepLastOccurence
#' @param triangular if NULL, the obtained triangular thresholds are not represented on the graph. If not NULL, contains the dataset containing the results of AFC test containing these columns: subjectName, productName,descriptorName,timeName
#' @param scoreName name of the column containing the scores in rata data.frame
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 scale_shape_manual geom_line theme_bw ggtitle aes ggplot geom_point
#' @export
#' @examples
#' data(rata)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
#' data(triangular)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular)
analyseScores=function(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",scoreName="Score",triangular=NULL,displayAFC=FALSE)
{
  subject=score=concentration=NULL
  rata2=rata[,c(productName,subjectName,scoreName)]
  colnames(rata2)=c("concentration","subject","score")
  rata2[,"concentration"]=factor(rata2[,"concentration"])
  rata2[,"concentration"]=fct_relevel(rata2[,"concentration"],decreasingConcentrations)
  p=ggplot(rata2,aes(x=concentration,y=score,group=subject,color=subject))+geom_line()+theme_bw()+ggtitle("Scores according to concentrations")

  if(!is.null(triangular))
  {
     res=keepLastOccurence(triangular,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName)
     thr=getThreshold(res,decreasingConcentrations=decreasingConcentrations,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName,resName=resName,rata=rata)
     wrongs=res$df
     thr2=thr[,c(productName,subjectName,"score")]
     colnames(thr2)=c("concentration","subject","score")
     p=p+geom_point(data=thr2,mapping=aes(x=concentration,y=score,col=subject),size=4,shape=2)
     if(displayAFC)
     {
       df_wrong=merge(rata2,wrongs,by.y=c(subjectName,productName),by.x=c("subject","concentration"),all.x=TRUE)
       df_wrong=df_wrong[!is.na(df_wrong[,"Res"]),]
       p=p+geom_point(data=df_wrong,mapping=aes(x=concentration,y=score,col=subject,shape=Res))+scale_shape_manual(values=c("OK"=20,"KO"=4))
     }
   }
  return(p)
}
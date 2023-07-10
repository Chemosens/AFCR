#' keepLastOccurence
#'
#' Returns the dataframe with the last click only. Also indicates the time of the last click, the result (KO or OK, entered in descriptor after an underscore) and the number of clicks
#' @param x is the dataset containing the results of AFC test containing these columns: subjectName, productName,descriptorName,timeName.
#' @param subjectName name of the column containing the subject information
#' @param productName name of the column containing the subject information
#' @param descriptorName name of the column containing the subject information
#' @param timeName name of the column containing the subject information
#' @export
#' @examples
#' data(triangular)
#' df=keepLastOccurence(triangular,subjectName="Paneliste")

keepLastOccurence=function(x,subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps")
{
  subjects=levels(factor(x[,subjectName]))
  products=levels(factor(x[,productName]))
  df=NULL
  for(suj in subjects)
  {
    for(prod in products)
    {
      dataToKeep=x[x[,subjectName]==suj&x[,productName]==prod&!(x[,descriptorName]%in%c("START","STOP")),]
      dataToKeep[,"nClicks"]=1
      if(dim(dataToKeep)[1]>1)
      {
        dataToKeep[,"nClicks"]=dim(dataToKeep)[1]
        dataToKeep=dataToKeep[which.max(dataToKeep[,timeName]),]
      }
      if(is.null(df)){df=dataToKeep}
      else{df=rbind(df,dataToKeep)}
    }
  }
  df[,"Res"]=substr(df[,descriptorName],nchar(as.character(df[,productName]))+2,nchar(as.character(df[,productName]))+3)
  res=list(df=df,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName)
  class(res)="afc"
  return(res)
}
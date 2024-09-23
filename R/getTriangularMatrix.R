#'@export
getTriangularMatrix=function(triangular, subjectName="Paneliste", productName="Produit", descriptorName="Descripteur",timeName="Temps",increasingConcentrations)
{
  resultName="Res";passed="OK"
  df=keepLastOccurence(triangular,subjectName=subjectName,productName=productName,timeName = timeName,descriptorName = descriptorName)
  res=df$df[,c(subjectName,productName,resultName)]
  subjects=levels(factor(res[,subjectName]))
  products=levels(factor(res[,productName]))
  matrixRes=matrix(NA,length(subjects),length(products))
  if(!all(increasingConcentrations%in%products)){stop("increasingConcentrations are not in productName column")}
  rownames(matrixRes)=subjects;colnames(matrixRes)=increasingConcentrations
  for(subject in subjects)
  {
    for(product in products)
    {
      res_ps=res[res[,subjectName]==subject &res[,productName]==product ,resultName]
      if(length(res_ps)==1)
      {
        if(res_ps==passed){symbol=1}else{symbol=0}
        matrixRes[subject,product]=symbol
      }
      else(print("warning: pb in the file"))
    }
  }
  return(matrixRes)
}
  
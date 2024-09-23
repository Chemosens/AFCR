#'@title fromTriangularMatrixToThreshold
#'@description from a triangular matrix (sorted from the lowest to the highest concentration), returns the BET interval results
#' @param a matrix whose rows are subjects and concentrations as columns. 
#'@export
#'@examples M=matrix(1,4,6);rownames(M)=paste0("S",1:4);colnames(M)=paste0("C",1:6)
#' M[1,5]=0; fromTriangularMatrixToThreshold(M)
fromTriangularMatrixToThreshold=function(triangularMatrix)
{
  nSubjects=nrow(triangularMatrix)
  threshold=rep(NA,nrow(triangularMatrix));names(threshold)=rownames(triangularMatrix)
  for(i in 1:nSubjects)
  {
    wrongAnswers=which(triangularMatrix[i,]==0)
    if(length(wrongAnswers)>=1)
    {
      lastWrongAnswer=wrongAnswers[length(wrongAnswers)]
      if(lastWrongAnswer<ncol(triangularMatrix))
      {
        threshold[i]=paste0(colnames(triangularMatrix)[lastWrongAnswer],"-",colnames(triangularMatrix)[lastWrongAnswer+1])
      }
      else{threshold[i]=paste0(colnames(triangularMatrix)[lastWrongAnswer],"++")}
    }
    else{threshold[i]=paste0(colnames(triangularMatrix)[1],"--")}
  }
  return(threshold)
}
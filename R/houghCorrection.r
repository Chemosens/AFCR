#' @param triangularMatrix
#' @param p_chance probability to give the good answer by chance
#' @param numericConcentrations increasing numericConcentrations
#' @export
houghCorrection=function(triangularMatrix,p_chance=1/3,numericConcentrations=NULL)
{
  old=triangularMatrix
  I=nrow(triangularMatrix)
  J=ncol(triangularMatrix)
  p_correct=apply(triangularMatrix,2,sum)/I
  p_disc1=(p_correct-p_chance)/(1-p_chance)
  p_disc2=(p_correct-p_chance^2)/(1-(p_chance^2))
  for(i in 1:nrow(triangularMatrix))
  {
    continue=TRUE
    j=1
    while(j <=ncol(triangularMatrix)& continue)
    {
        if(triangularMatrix[i,j]==1)
        {
            if(j<J)
            {
              if(any(triangularMatrix[i,(j+1):J]==0))
              {
                triangularMatrix[i,j]= rbinom(size=1,n=1,p=p_disc1[j])
              }
              if(all(triangularMatrix[i,(j+1):J]==1))
              {
                triangularMatrix[i,j]= rbinom(size=1,n=1,p=p_disc1[j]) 
                
                if(j<J-1)
                {
                  triangularMatrix[i,j+1]= rbinom(size=1,n=1,p=p_disc2[j]) 
                }
                continue=FALSE
              }
            }
            if(j==J){continue=FALSE}
             
            j=j+1
            
        }
        else{j=j+1}
      }
  }
  return(list(triangularMatrix=triangularMatrix, old=old))
}

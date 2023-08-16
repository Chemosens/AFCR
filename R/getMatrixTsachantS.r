#' @param p probability to fail an AFC test
#' @param K number of successive AFC tests
#'@param eps 0
#'@param modelEps "last"
getMatrixTsachantS=function(p,K,eps=0,modelEps="last")
{
  res=matrix(NA,K+1,K+1)
  for(Tk in 1:(K+1))
  {
    for(Sj in 1:(K+1))
    {
      res[Sj,Tk]=probaTsachantS(Tk-1,Sj-1,p,eps=eps,K=K,modelEps=modelEps)
    }
  }
  return(res)
}
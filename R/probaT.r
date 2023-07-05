
probaT=function(probaS,p=7/8)
{
  tVector=rep(NA,length(probaS))
  for(k in 0:(length(probaS)-1))
  {
    S=0
    for(j in 0:(length(probaS)-1))
    {
      S=S+probaTinterS(probaS,k,j,p)
    }
    tVector[k+1]=S
  }
  return(tVector)
}
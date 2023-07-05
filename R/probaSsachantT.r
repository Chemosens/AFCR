probaSsachantT=function(probaS,Tk,Sj,p)
{
  int=probaTinterS(probaS,Tk,Sj,p)
  pT=probaT(probaS,p)[Tk+1]
  return(int/pT)
}
probaTinterS=function(probaS,Tk,Sj,p,K=NULL,eps=0,modelEps="none")
{
  return(probaS[Sj+1]*probaTsachantS(Tk=Tk,Sj=Sj,p=p,K=K,eps=eps,modelEps=modelEps))
}
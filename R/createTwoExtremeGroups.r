#'@importFrom stats t.test wilcox.test
createTwoExtremeGroups=function(N=200,n=40,p=2/3,probaS=rep(1/7,7),K=6,concentrations,test="wilcox")
{
  hyper=selectExtreme(N=N,n=n,p=p,probaS=probaS,K=K)
  hypo=selectExtreme(N=N,n=n,p=p,probaS=probaS,K=K,lower=FALSE)
  vec_gp1=round(n*hyper$probaSSachantG)
  vec_gp2=round(n*hypo$probaSSachantG)
  v_g1=v_g2=c()
  for(i in 1:length(vec_gp1))
  {
    v_g1=c(v_g1,rep(concentrations[i],vec_gp1[i]))
    v_g2=c(v_g2,rep(concentrations[i],vec_gp2[i]))
  }
  if(test=="t")
  {
    return(t.test(v_g1,v_g2))
  }
  if(test=="wilcox")
  {
    return(wilcox.test(v_g1,v_g2))
  }

}
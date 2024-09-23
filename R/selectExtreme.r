#'' @title selectExtreme
#' #@description returns the extremes of a distribution
#' @param N number of individuals in the experiment
#' @param n number of individuals required in the extremal group
#' @inheritParams getMatrixTsachantS
#' @param lower TRUE for the group with the lower value of T (threhsold), FALSE for the group with the highest threshold
#' @param probaS distribution of probability
#' @export
#' @return nG
#' @importFrom ggplot2 geom_col geom_vline
#' @importFrom forcats fct_relevel
#' @description This function an extreme group from a given experimentation : given p, probaS, K (number of samples) and N (number of tests) it returns a group containing n subjects (with the lowest -when lower = TRUE or the highest values).
#' @importFrom graphics barplot 
selectExtreme=function(N=200,n=40,lower=TRUE,p,probaS,K)
{
  thr=NULL
  t_distr=probaT(probaS=probaS,p=p)
  probaTsachantS=getMatrixTsachantS(p=p,K=K,eps=0,modelEps="last")
  ncat=nG=rep(0,K+1);names(nG)=paste0("T",0:K)
  pG=cs=0
  probaTinterSMat=matrix(0,K+1,K+1);rownames(probaTinterSMat)=paste0("]c_",0:K,";c_",1:(K+1),"]"); colnames(probaTinterSMat)=paste0("T",1:(K+1));
  # Calculation of probability of T knowing S
  for(i in 1:(K+1))
  {
    for(j in 1:(K+1))
    {
      probaTinterSMat[i,j]=probaTinterS(probaS=probaS,Tk=j-1,Sj=i-1,p=p)
    }
  }

  if(lower==FALSE)
  {
    probaTinterSMat=t(apply(probaTinterSMat,1,rev))
    t_distr=rev(t_distr)
  }

  # Cumulative values: while cs <=n, the probabilities are added
  i=1
  while(cs<=n)
  {
    ncat[i]=N*t_distr[i]
    cs=cumsum(ncat)[K]
    if(cs>n) # Si on dépasse pour le i suivant : On garde seulement la dernière colonne de probaTinterSmat et on la multiplie par la proba d'être dans ce groupe
    {
      if(i==1) # if only the first category contains the n subjects
      {
        nG[i]=n
        pG=pG+t_distr[i]
      }
      if(i>=2)
      {
        cs0=cumsum(ncat[1:(i-1)])
        nG[i]=n-cs0[length(cs0)] #nG is the number selected in each category
        pLast=nG[i]/ncat[i]
        pG=pG+t_distr[i]*pLast
      }

    }
    else
    {
      nG[i]=ncat[i]
      pG=pG+t_distr[i]
    }

    i=i+1
  }
  if(i>2)
  {
    mat=probaTinterSMat[,1:(i-1)]
    mat[,i-1]=mat[,i-1]*pLast #ici, on multiplie la probabilité par celle du groupe
    pGInterS=apply(mat,1,sum)
  }
  else
  {
    pGInterS=probaTinterSMat[,1]
  }

  probaSsachantG=pGInterS/pG
  if(lower==FALSE)
  {
    nG=rev(nG); names(nG)=rev(names(nG))
    #probaSsachantG=rev(probaSsachantG);
  }
 # barplot(probaSsachantG,names.arg=0:K,main=paste0("composition of the obtained group"))
  df=data.frame(thr=names(probaSsachantG),p=probaSsachantG)
  df[,"thr"]=factor(df[,"thr"])
  df[,"thr"]=fct_relevel(df[,"thr"],paste0("]c_",0:K,";c_",1:(K+1),"]"))
  gg=ggplot(df,aes(x=thr,y=p))+geom_col()+xlab("threshold")
  return(list(nG=nG,probaSSachantG=probaSsachantG,pG=pG,gg=gg,pGInterS=pGInterS))
}

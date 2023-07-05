probaTsachantS=function(Tk,Sj,p,eps=0,K=NULL, modelEps="last")
{
  #eps est la probabilité de se tromper si le seuil théorique est inférieur à j (il parait que ça arrive...)
  if(modelEps!="none")
  {
    correctionEps=1-eps
    if(Sj==K){correctionEps=1}
  }
  else{correctionEps=1}

  if(Tk>Sj)# si le seuil observe est plus haut (+concentre) que le seuil theorique: proba = 0
  {
    if(modelEps=="none")
    {
      proba=0
    }
    if(modelEps=="last")
    {
      if(Tk==Sj+1)
      {
        proba=eps
      }
      else
      {
        proba=0
      }
    }
    # if(modelEps=="equi")
    # {
    #   proba=eps/(K-Sj+1)
    # }
  }

  if(Tk==Sj)
  {
    if(Sj==0){proba=correctionEps}else{proba=p*correctionEps}
  } #la proba de se tromper ? partir de l? est de 7/8 (doit etre inclus ci dessous)

  if(Tk<Sj)
  {
    q=(1-p)
    proba=correctionEps*p*q^(Sj-Tk) # la proba de se tromper sur les j derniers termes suit une loi g?om?trique tronqu?e de parametre n=j
    if(Tk==0){proba=correctionEps*q^Sj} #la proba de ne pas faire du tout d'erreur  sur les tests restants est q^j
  }
  # }
  return(proba)
}
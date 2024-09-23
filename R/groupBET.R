#'@title groupBET
#'@description returns the BET of a group of subjects
#'@param dfT dataframe whose colnames are "t" (containing the proportion of subject having this BET) and "BET", value of the BET
#'@param nb_total number of subjects in the distribution 
#'@param type 'geom' or 'arithm' for geometric or arithmetic average
#'@export
groupBET=function(dfT,nb_total=193,type="geom")
{
  if(type=="geom")
  {
    bet_group=1
    for(i in 1:length(dfT[,"t"]))
    {
      nb_i=dfT[i,"t"]*nb_total
      bet_group=bet_group*dfT[i,"BET"]^nb_i
    }
    bet_group=bet_group^(1/nb_total)
  }
  if(type=="arithm")
  {
    bet_group=0
    for(i in 1:length(dfT[,"t"]))
    {
      nb_i=dfT[i,"t"]*nb_total
      bet_group=bet_group+dfT[i,"BET"]*nb_i
      print(bet_group)
    }
    bet_group=bet_group/nb_total
  }
  return(bet_group)
}

#' analyseSores
#'
#' Returns the plots with the scores according to their concentrations
#' @inheritParams getThreshold
#' @inheritParams keepLastOccurence
#' @param triangular if NULL, the obtained triangular thresholds are not represented on the graph. If not NULL, contains the dataset containing the results of AFC test containing these columns: subjectName, productName,descriptorName,timeName
#' @param scoreName name of the column containing the scores in rata data.frame
#' @param decreasingNumConcentrations numerical values of the used concentrations
#' @param revertX if TRUE, allows the x-axis the to be inverted
#' @param regression if TRUE, adds a linear regression and its statistical indicators (R2, RMSE) to the graph
#' @param displayAFC if TRUE, the AFC result is displayed (a point when the AFC test was succeeded, a cross if it was failed )
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ylim labs geom_text geom_abline scale_shape_manual geom_line theme_bw ggtitle aes ggplot geom_point
#' @importFrom stats coef lm
#' @description This function returns graphical results
#' @export
#' @examples
#' data(rata)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
#' data(triangular)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular)
analyseScores=function(rata,subject=NULL,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",scoreName="Score",
                       triangular=NULL,displayAFC=FALSE,representationAFC="label",displayIBT=TRUE,decreasingNumConcentrations=NULL,regression=FALSE, revertX=FALSE, logY=FALSE,minConc=0,maxConc=NULL,y_add=NULL,colors_values=NULL)
{
# Initializations and default parameters
  score=concentration=concentration2=Res=NULL
  scale_y=10
  rata2=rata[,c(productName,subjectName,scoreName)]
  colnames(rata2)=c("concentration","subject","score")
  rata2[,"concentration"]=factor(rata2[,"concentration"])
  if(!is.null(subject))
  {
    rata2=rata2[rata2[,"subject"]%in%subject,]
  }
  if(!is.null(triangular)&!is.null(subject))
  {
    triangular=triangular[triangular[,subjectName]%in%subject,]
  }
  if(is.null(subject)){subject=unique(rata2[,"subject"])}
  if(is.null(colors_values))
  {
    if(length(subject)>1)
    {
      colors_values=rainbow(length(subject));names(colors_values)=subject
    }
    else
    {
      colors_values="black";names(colors_values)=subject
    }
  }
  
  if(!revertX)
  {
      rata2[,"concentration"]=fct_relevel(rata2[,"concentration"],decreasingConcentrations)
  }
  if(revertX& is.null(decreasingNumConcentrations))
  {
    rata2[,"concentration"]=fct_relevel(rata2[,"concentration"],rev(decreasingConcentrations))
  }

  if(!is.null(decreasingNumConcentrations))
  {
    rata2[,"concentration2"]=rata2[,"concentration"]
    correspondance=decreasingNumConcentrations
    names(correspondance)=decreasingConcentrations
    rata2[,"concentration2"]=correspondance[as.character(rata2[,"concentration2"])]
  }
  
  if(logY){rata2[,"score"]=log(rata2[,"score"]+1)}
  
  # Initialisation of the graph concentration/ intensity
  if(!is.null(decreasingNumConcentrations))
  {
   p=ggplot(rata2,aes(x=concentration2,y=score,group=subject,color=subject))+geom_line()+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Scores according to concentrations") + scale_color_manual(values=colors_values)
  }
  else
  {
    p=ggplot(rata2,aes(x=concentration,y=score,group=subject,color=subject))+geom_line()+theme_bw()+theme(plot.title = element_text(hjust = 0.5))+ggtitle("Scores according to concentrations")+ scale_color_manual(values=colors_values)
  }
  
  # Getting intensity threshold
  ibt=NULL
  if(displayIBT)
  {
    ibt=getIBT(rata,subjectName=subjectName,scoreName=scoreName,productName=productName,decreasingConcentrations=decreasingConcentrations,decreasingNumConcentrations=decreasingNumConcentrations, minConc=minConc,maxConc=maxConc)
    if(is.null(decreasingNumConcentrations))
    {
      print("warning: displayIBT only available when decreasingNumConcentrations is filled")
    }
    if(!is.null(decreasingNumConcentrations))
    { 
      p=p+geom_point(x=ibt[1],y=0,shape=3,size=4,color="black")
    }
  }

  
 # Getting BET threshold
  if(!is.null(triangular))
  {
     res=keepLastOccurence(triangular,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName)
     thr=getThreshold(res,decreasingConcentrations=decreasingConcentrations,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName,resName=resName,rata=rata,decreasingNumConcentrations=decreasingNumConcentrations,minConc=minConc,maxConc=maxConc)
     wrongs=res$df
     if("avg"%in%colnames(thr))
     {
       thr2=thr[,c(productName,subjectName,"score","avg","thresholdNum")]
       colnames(thr2)=c("concentration","subject","score","avg","thresholdNum")
     }
     else
     {
       thr2=thr[,c(productName,subjectName,"score")]
       colnames(thr2)=c("concentration","subject","score")
     }
     if(logY){thr2[,"score"]=log(thr2[,"score"]+1)}
     if(is.null(decreasingNumConcentrations))
     {
         p=p+geom_point(data=thr2,mapping=aes(x=concentration,y=score,col=subject),size=4,shape=2)+ scale_color_manual(values=colors_values)
     }
    if(!is.null(decreasingNumConcentrations))
    {
      thr2[,"concentration2"]=correspondance[as.character(thr2[,"concentration"])]
     #p=p+geom_point(data=thr2,mapping=aes(x=thresholdNum,y=avg,col=subject),size=4,shape=4)+ scale_color_manual(values=colors_values)
      thr2[,"avg"]=0
      p=p+geom_point(data=thr2,mapping=aes(x=thresholdNum,y=avg,col=subject),size=4,shape=4)+ scale_color_manual(values=colors_values)
    }

    if(displayAFC)
    {
       df_wrong=merge(rata2,wrongs,by.y=c(subjectName,productName),by.x=c("subject","concentration"),all.x=TRUE)
       df_wrong=df_wrong[!is.na(df_wrong[,"Res"]),]
       df_wrong[df_wrong[,"Res"]=="OK","Res"]="1"
       df_wrong[df_wrong[,"Res"]=="KO","Res"]="0"
       if(is.null(decreasingNumConcentrations))
       {
         if(representationAFC=="point")
         {
           p=p+geom_point(data=df_wrong,mapping=aes(x=concentration,y=score,col=subject,shape=Res))+scale_shape_manual(values=c("1"=20,"0"=4))+ scale_color_manual(values=colors_values)
         }
         
         if(representationAFC=="label")
         {
           if(is.null(y_add)){y_add=(max(df_wrong[,"score"])-min(df_wrong[,"score"]))/scale_y}
           df_wrong2=df_wrong
           df_wrong2[,"score"]=df_wrong[,"score"]+y_add
           p=p+geom_text(data=df_wrong2,mapping=aes(x=concentration,y=score,col=subject,label=Res))+ scale_color_manual(values=colors_values)
           #p=p+ylim(min(df_wrong[,"score"])-y_add,max(df_wrong[,"score"])+y_add)
           
         }
       }
        if(!is.null(decreasingNumConcentrations))
       {
          if(representationAFC=="point")
          {
            p=p+geom_point(data=df_wrong,mapping=aes(x=concentration2,y=score,col=subject,shape=Res))+scale_shape_manual(values=c("1"=20,"0"=4))+ scale_color_manual(values=colors_values)
          }
          if(representationAFC=="label")
          {
            if(is.null(y_add)){y_add=(max(df_wrong[,"score"])-min(df_wrong[,"score"]))/scale_y}
            df_wrong2=df_wrong
            df_wrong2[,"score"]=df_wrong[,"score"]+y_add
            p=p+geom_text(data=df_wrong2,mapping=aes(x=concentration2,y=score,col=subject,label=Res))+ scale_color_manual(values=colors_values)
           # p=p+ylim(min(df_wrong[,"score"])-y_add,max(df_wrong[,"score"])+y_add)
          }
       }
    }
     # Attempts of regressions on the data
    subtitle=""
    r2=rmse=r2n=rmsen=slopeNeg=slopePos=NA
     if(regression & !is.null(decreasingNumConcentrations)&length(unique(rata[,subjectName]))==1)
     {
      
       relevantDataPos=rata2[,"concentration2"]>=thr2[,"concentration2"]
       relevantDataNeg=rata2[,"concentration2"]<thr2[,"concentration2"]

       if(sum(relevantDataPos)>2)
       {
         reslm=lm(rata2[relevantDataPos,"score"]~rata2[relevantDataPos,"concentration2"])
         r2=summary(reslm)$adj.r.squared
         ssr=sum(summary(reslm)$residuals^2)
         rmse=sqrt(ssr/length(summary(reslm)$residuals))
         if(subtitle==""){subtitle=paste0("R2: ",round(summary(reslm)$r.squared,digits=2),"; RMSE:", round(rmse,2))}
         p=p + geom_abline(intercept = coef(reslm)[1], slope = coef(reslm)[2], col="blue") + labs(subtitle=subtitle)
         slopePos=coef(reslm)[1]
       }

       if(sum(relevantDataNeg)>2)
       {
         reslm=lm(rata2[relevantDataNeg,"score"]~rata2[relevantDataNeg,"concentration2"])
         r2n=summary(reslm)$adj.r.squared
         ssr=sum(summary(reslm)$residuals^2)
         rmsen=sqrt(ssr/length(summary(reslm)$residuals))
         slopeNeg=coef(reslm)[1]
         subtitle=paste0(subtitle, "R2n: ",round(summary(reslm)$r.squared,digits=2),"; RMSEn:", round(rmse,2),"; slope:",round(slopeNeg,digit=1))
         p=p + geom_abline(intercept = coef(reslm)[1], slope = coef(reslm)[2], col="green") + labs(subtitle=subtitle)
       }
     }
  }
  if(!is.null(decreasingNumConcentrations)){p=p+scale_x_continuous(breaks=rev(decreasingNumConcentrations))}
  
  p=p+scale_y_continuous(breaks=0:10,limits=c(0,10+y_add))+
    ylab("intensity")+xlab("log-concentrations")
  p=p+theme(panel.grid.minor=element_line(color="white"))
  listRes=list(p=p,ibt=ibt,thr2=thr2,r2=r2,rmse=rmse,r2n=r2n,rmsen=rmsen,slopeNeg=slopeNeg,slopePos=slopePos)
  return(listRes)
}
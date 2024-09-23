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
#' @param subject if NULL (default), a graph of all subjects is produced. It could also contain the name of only one ore more subjects. 
#' @param representationAFC 'label' or 'point'. If label, '0' or '1' is displayed. Else, 0 corresponds to a x and 1 to a o
#' @param displayIBT if TRUE the IBT threshold is calculated and represented as a +
#' @param displayBET if TRUE the BET threshold is calculated and represented as a x
#' @param y_add when representationAFC=='label', the label is displayed on the ordinate of the intensity + y_add
#' @param colors_values a vector containing the colors to be used. Default to "black" when length(subject)==1
#' @param logY FALSE by default. If true the intensities are logged (base = )
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 ylim theme element_text xlab ylab element_line scale_x_continuous scale_y_continuous labs geom_text geom_abline scale_shape_manual geom_line theme_bw ggtitle aes ggplot geom_point
#' @importFrom stats coef lm
#' @description This function returns graphical results for threshold and intensity with concentrations as abscissa and intensities as ordinates. This can be done by subject (selecting one subject with the subject parameter). In this case, the IBT and BET can be calculated and represented. When several subjects are selected, a color is attributed for each subject and the IBT thresholds are not calculated. 
#' @export
#' @importFrom grDevices rainbow
#' @examples
#' data(rata)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
#' data(triangular)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular)
analyseScores=function(rata,subject=NULL,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Panéliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",scoreName="Score",
                       triangular=NULL,displayAFC=FALSE,displayBET=TRUE,representationAFC="label",displayIBT=TRUE,decreasingNumConcentrations=NULL,regression=FALSE, revertX=FALSE, logY=FALSE,minConc=0,maxConc=NULL,y_add=NULL,colors_values=NULL)
{
# Initializations and default parameters
  score=concentration=concentration2=Res=thresholdNum=avg=NULL
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
  
  if(logY){rata2[,"score"]=log(rata2[,"score"]+1,base=10)}
  
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
       if(is.null(decreasingNumConcentrations))
    {
     if(length(subject)>1)
     {
       print("displayIBT only available when decreasingNumConcentrations is filled and for 1 subject")
        displayIBT=FALSE 
      }
      if(length(subject)==1)
      {
        ibt=getIBT(rata,subjectName=subjectName,scoreName=scoreName,productName=productName,decreasingConcentrations=decreasingConcentrations,decreasingNumConcentrations=decreasingNumConcentrations, minConc=minConc,maxConc=maxConc)$threshold
        p=p+geom_point(x=ibt[1],y=0,shape=3,size=4,color="black")
      }
       
    }
    if(!is.null(decreasingNumConcentrations))
    { 
      if(length(subject)==1)
      {
        ibt=getIBT(rata,subjectName=subjectName,scoreName=scoreName,productName=productName,decreasingConcentrations=decreasingConcentrations,decreasingNumConcentrations=decreasingNumConcentrations, minConc=minConc,maxConc=maxConc)$threshold
        
        p=p+geom_point(x=ibt[1],y=0,shape=3,size=4,color="black")
      }
      else{ print("displayIBT only available for length(subject)=1");displayIBT=FALSE }
    }
   
  }
  
 # Getting BET threshold
  thr2=r2=rmse=r2n=rmsen=slopeNeg=slopePos=NULL
  if(!is.null(triangular))
  {
  
     res=keepLastOccurence(triangular,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName)
    
      if(displayBET)
      {
        thr=getThreshold(res,decreasingConcentrations=decreasingConcentrations,subjectName=subjectName,productName=productName,descriptorName=descriptorName,timeName=timeName,resName=resName,rata=rata,decreasingNumConcentrations=decreasingNumConcentrations,minConc=minConc,maxConc=maxConc)
       
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
        if(logY){thr2[,"score"]=log(thr2[,"score"]+1,base=10)}
        if(is.null(decreasingNumConcentrations))
        {
          p=p+geom_point(data=thr2,mapping=aes(x=concentration,y=score,col=subject),size=4,shape=4)+ scale_color_manual(values=colors_values)
        }
        if(!is.null(decreasingNumConcentrations))
        {
          thr2[,"concentration2"]=correspondance[as.character(thr2[,"concentration"])]
          thr2[,"avg"]=0
          p=p+geom_point(data=thr2,mapping=aes(x=thresholdNum,y=avg,col=subject),size=4,shape=4)+ scale_color_manual(values=colors_values)
        }
        
      }
     # 
    if(displayAFC)
    {
       wrongs=res$df
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
            
         }
       }
        
        if(!is.null(decreasingNumConcentrations))
       {
          if(representationAFC=="point")
          {
            y_add=0
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
         subtitle=paste0(subtitle, "R2n: ",round(summary(reslm)$r.squared,digits=2),"; RMSEn:", round(rmse,2),"; slope:",round(slopeNeg,digits=1))
         p=p + geom_abline(intercept = coef(reslm)[1], slope = coef(reslm)[2], col="green") + labs(subtitle=subtitle)
       }
     }
  }
  if(!is.null(decreasingNumConcentrations))
  {p=p+scale_x_continuous(breaks=rev(decreasingNumConcentrations))}
  
  if(is.null(y_add)){y_add=0}
  ysup=10+y_add

  p=p+scale_y_continuous(breaks=0:10,limits=c(0,10+y_add))+
    ylab("intensity")+ylim(0,ysup)+xlab("Concentrations")

  p=p+theme(panel.grid.minor=element_line(color="white"))
  if(displayBET&displayIBT)
  {
    p=p+labs(caption="x for BET, + for IBT")
    p=p+theme(legend.position="none")+ggtitle(subject)
  }

  listRes=list(p=p,ibt=ibt,thr2=thr2,r2=r2,rmse=rmse,r2n=r2n,rmsen=rmsen,slopeNeg=slopeNeg,slopePos=slopePos)
  return(listRes)
}
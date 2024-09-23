
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
#' data(triangular)

data(rata)
data(triangular)
p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular)

p=analyseScores(rata,subject="S002",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,representationAFC="point")


p=analyseScores(rata,subject="S002",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,representationAFC="label")

p=analyseScores(rata,subject="S002",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,representationAFC="label")

subject="S002"
p=analyseScores(rata,subject=subject,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,representationAFC="label",y_add=1,decreasingNumConcentrations=9:1,maxConc=10)
p=p$p+ylim(0,10)+scale_y_continuous(breaks=0:10,limits=c(0,10))+
  ylab("intensity")+xlab("log-concentrations")+ggtitle(subject)+scale_x_continuous(breaks=0:10)+theme(legend.position="none")
p=p+theme(panel.grid.minor=element_line(color="white"))
p

res_ibt=getIBT(rata,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
res_ibt_num=getIBT(rata,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,maxConc=10,minConc=0)




p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,revertX=TRUE)

p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=NULL,displayAFC=T,decreasingNumConcentrations=rev(c(1:9)),minConc=1,maxConc=9)

p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T,decreasingNumConcentrations=rev(c(1:9)),minConc=1,maxConc=9)

p=analyseScores(rata[rata[,"Paneliste"]=="S001",],decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",
                triangular=triangular[triangular[,"Paneliste"]=="S001",],displayAFC=T,decreasingNumConcentration=rev(c(1:9)),minConc=1,maxConc=9)

p=analyseScores(rata[rata[,"Paneliste"]=="S001",],decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",
                triangular=triangular[triangular[,"Paneliste"]=="S001",],displayAFC=T,decreasingNumConcentration=rev(c(1:9)),regression=T,minConc=1,maxConc=9)

p=analyseScores(rata[rata[,"Paneliste"]=="S002",],decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",
                triangular=triangular[triangular[,"Paneliste"]=="S002",],displayAFC=T,decreasingNumConcentration=rev(c(1:9)),regression=T,minConc=1,maxConc=9)

res=analyseScores(rata[rata[,"Paneliste"]=="S003",],decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",scoreName="Score",
                triangular=triangular[triangular[,"Paneliste"]=="S003",],displayAFC=T,decreasingNumConcentration=rev(c(1:9)),regression=T,minConc=1,maxConc=9)


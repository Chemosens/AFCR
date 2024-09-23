
data(rata)
rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001[,"Score"]=c(4, 6 , 7 , 8  ,5  ,9 , 9, 10,10)
res=getIBT(intensityData=rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==1))


rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001[,"Score"]=c(4, 10 , 7 , 8  ,5  ,9 , 9, 10,10)
res=getIBT(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==1))


rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001[,"Score"]=c(4, 5 , 4 , 8  ,5  ,9 , 9, 10,19)
res=getIBT(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==7))

rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001=rata_S001[1:8,]
rata_S001[,"Score"]=c(3,1,0,0,1,0,1,2)
res=getIBT(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==6))

rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001=rata_S001[1:8,]
rata_S001[,"Score"]=c(2,4,4,6,9,9,10,10)
res=getIBT(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==3))

rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001=rata_S001[1:8,]
rata_S001[,"Score"]=c(6,7,7,8,6,7,7,8)
res=getIBT(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test IBT", expect_true(res$threshold[1]==1))

data(triangular)
analyseScores(rata_S001,subject=NULL,decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"),subjectName="Paneliste",productName="Produit",descriptorName="Descripteur",timeName="Temps",resName="Res",scoreName="Score",
                       triangular=triangular,displayAFC=TRUE,representationAFC="label",displayIBT=TRUE,decreasingNumConcentrations=8:1,regression=FALSE, revertX=FALSE, logY=FALSE,minConc=0,maxConc=9,y_add=NULL,colors_values=NULL)
  
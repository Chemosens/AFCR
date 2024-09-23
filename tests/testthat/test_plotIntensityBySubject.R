
data(rata)
res=getIntensityThreshold(rata,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
p1=plotIntensityThresholdBySubject(res,subject="S003",
                                        displayAFC=FALSE,revertX=FALSE)

rata2=rata
rata2[rata2[,"Produit"]=="C9","Score"]=10
res2=getIntensityThreshold(rata2,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
p2=plotIntensityThresholdBySubject(res2,subject="S003",
                                displayAFC=FALSE,revertX=FALSE)+ggtitle("tol=0")

rata3=rata2
rata3[rata3[,"Produit"]=="C7","Score"]=2
res3=getIntensityThreshold(rata3,subjectName="Paneliste",
                           decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
plotIntensityThresholdBySubject(res3,subject="S003",
                               displayAFC=FALSE,revertX=FALSE)
rata4=rata3
rata4[rata4[,"Produit"]=="C7","Score"]=2
res4=getIntensityThreshold(rata4,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
plotIntensityThresholdBySubject(res4,subject="S003",
                                displayAFC=FALSE)

rata5=rata
rata5[,"Score"]=0
rata5[rata5[,"Produit"]=="C9","Score"]=1
res5=getIntensityThreshold(rata5,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
plotIntensityThresholdBySubject(res5,subject="S003",
                               displayAFC=FALSE)

rata6=rata
rata6[,"Score"]=0
res6=getIntensityThreshold(rata6,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
plotIntensityThresholdBySubject(res6,subject="S003",
                               displayAFC=FALSE)

# tol
rata7=rata
rata7[,"Score"]=2
res7=getIntensityThreshold(rata6,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10,tol=0.2)
plotIntensityThresholdBySubject(res7,subject="S003",
                               displayAFC=FALSE)

res8=getIntensityThreshold(rata2,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10,tol=0.2)
p8=plotIntensityThresholdBySubject(res8,subject="S003",
                                displayAFC=FALSE)+ggtitle("tol=0.2,scaleBySubject")
# seuils0_ini_int=getIntensityThreshold(intensityData=rata0,
#                                       decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations =c(8:1),minConc=0,maxConc=9
# )

res9=getIntensityThreshold(rata2,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10,tol=1,scaleBySubject = FALSE)
p9=plotIntensityThresholdBySubject(res9,subject="S003",
                                displayAFC=FALSE)+ggtitle("tol=1")

res10=getIntensityThreshold(rata2,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10,tol=0)
p10=plotIntensityThresholdBySubject(res10,subject="S003",
                                 displayAFC=FALSE)+ggtitle("tol=0")





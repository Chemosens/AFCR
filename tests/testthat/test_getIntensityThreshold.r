
data(rata)
data(triangular)
res=getIntensityThreshold(rata,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test intensity", expect_true(res[1]==9))

rata2=rata
rata2[rata2[,"Produit"]=="C9","Score"]=10
res2=getIntensityThreshold(rata2,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))

test_that("test intensity (2)", expect_true(res2[1]==7))

rata3=rata2
rata3[rata3[,"Produit"]=="C7","Score"]=2
res3=getIntensityThreshold(rata3,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test intensity (3)", expect_true(res3[1]==6))

rata4=rata3
rata4[rata4[,"Produit"]=="C7","Score"]=2
res4=getIntensityThreshold(rata4,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test intensity (4)", expect_true(res4[1]==6.5))
# 
# seuils0_ini_int=getIntensityThreshold(intensityData=rata0,
#                                       decreasingConcentrations=c("C8","C7","C6","C5","C4","C3","C2","C1"),decreasingNumConcentrations =c(8:1),minConc=0,maxConc=9
# )

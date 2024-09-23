rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001[,"Score"]=c(4, 6 , 7 , 8  ,5  ,9 , 9, 10,10)
res=getScreeThreshold(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test scree", expect_true(res[1]==5))


rata_S001=rata[rata[,"Paneliste"]=="S001",]
rata_S001[,"Score"]=c(1, 1 , 5 , 6  ,7  ,9 , 9, 10,10)
res=getScreeThreshold(rata_S001,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))
test_that("test Jump", expect_true(res[1]==2))


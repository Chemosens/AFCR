data(triangular)
df=keepLastOccurence(triangular,subjectName="Paneliste")
seuils=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))

test_that("test 1", all(expect_true(seuils==c(0,8,9))))



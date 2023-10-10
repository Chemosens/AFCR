data(triangular)
df=keepLastOccurence(triangular,subjectName="Paneliste")
seuils=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))

test_that("test 1", expect_true(all(seuils[,"threshold"]==c(0,8,9))))
test_that("test 2", expect_true(all(seuils[,"Paneliste"]==c("S001","S002","S003"))))

data(rata)
seuils=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
                      rata=rata)
test_that("test 3", expect_true(all(seuils[,"score"]==c(2,0,0))))

seuils=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
                    decreasingNumConcentrations=9:1)
test_that("test 4", expect_true(all(seuils[,"thresholdNum"]==c(1,8.5,9))))


seuils=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
                    decreasingNumConcentrations=9:1,rata=rata)
test_that("test 4", expect_true(all(seuils[,"thresholdNum"]==c(1,8.5,9))))



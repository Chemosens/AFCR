data(triangular)
df=keepLastOccurence(triangular,subjectName="Paneliste")
test_that("test number of clicks",expect_true(df[1,"nClicks"]==2))

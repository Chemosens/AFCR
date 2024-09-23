x=c(2,1,1,NA,4, NA, NA,1,1) ;
res=cumSumDroite(x)
y=c(4,2,1,NA,4,NA,NA,2,1)
test_that("test cumsum", expect_true(all(res==y,na.rm=T)))


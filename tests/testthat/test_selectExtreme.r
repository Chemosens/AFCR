hyper1=selectExtreme(N=200,n=40,p=2/3,probaS=rep(1/7,7),K=6)
hypo1=selectExtreme(N=200,n=40,p=2/3,probaS=rep(1/7,7),K=6,lower=FALSE)
hyper2=selectExtreme(N=200,n=40,p=1/2,probaS=rep(1/10,10),K=9)
hypo2=selectExtreme(N=200,n=40,p=1/2,probaS=rep(1/10,10),K=9,lower=FALSE)
test_that("result is a proba 1 (1)",expect_true(round(sum(hypo2$probaSSachantG),digits=12)==1))
test_that("result is a proba 1 (2)",expect_true(round(sum(hyper2$probaSSachantG),digits=12)==1))
test_that("result is a proba 1 (3)",expect_true(round(sum(hypo1$probaSSachantG),digits=12)==1))
test_that("result is a proba 1 (4)",expect_true(round(sum(hyper1$probaSSachantG),digits=12)==1))


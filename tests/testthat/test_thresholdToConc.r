
th1=thresholdToConcentration(0,decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test thresholdToConc (1)", expect_true(th1==0.5))

th2=thresholdToConcentration(1,decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test thresholdToConc (2)", expect_true(th2==1.5))


th3=thresholdToConcentration(5,decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test thresholdToConc (3)", expect_true(th3==5.5))


th4=thresholdToConcentration(8,decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test thresholdToConc (4)", expect_true(th4==8.5))

th5=thresholdToConcentration(9,decreasingNumConcentrations=9:1,minConc=0,maxConc=10)
test_that("test thresholdToConc (5)", expect_true(th5==9.5))


th6=thresholdToConcentration(9,decreasingNumConcentrations=9:1,minConc=0.5,maxConc=14)
test_that("test thresholdToConc (6)", expect_true(th6==11.5))

th7=thresholdToConcentration(0,decreasingNumConcentrations=9:1,minConc=0.5,maxConc=14)
test_that("test thresholdToConc (7)", expect_true(th7==0.75))
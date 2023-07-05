test_that("test 1",
          expect_true(
            probaTsachantS(Tk=0,Sj=0,p=7/8,K=3)==1))

test_that("test 2",  expect_true(probaTsachantS(Tk=1,Sj=0,p=7/8,K=3)==0))
test_that("test 3",  expect_true(probaTsachantS(Tk=2,Sj=0,p=7/8,K=3)==0))
test_that("test 4",  expect_true(probaTsachantS(Tk=0,Sj=1,p=7/8,K=3)== 1/8))
test_that("test 5",  expect_true(probaTsachantS(Tk=1,Sj=1,p=7/8,K=3)== 7/8))
test_that("test 6",  expect_true(probaTsachantS(Tk=2,Sj=1,p=7/8,K=3)== 0))
test_that("test 7",  expect_true(probaTsachantS(Tk=0,Sj=2,p=7/8,K=3)== 1/(8*8)))
test_that("test 8",  expect_true(probaTsachantS(Tk=1,Sj=2,p=7/8,K=3)==7/64))
test_that("test 9",  expect_true(probaTsachantS(Tk=2,Sj=2,p=7/8,K=3)==7/8))
test_that("test 10",  expect_true(probaTsachantS(Tk=0,Sj=3,p=7/8,K=3)== 1/(8^3)))
test_that("test 11",  expect_true(probaTsachantS(Tk=1,Sj=3,p=7/8,K=3)==1/(8^2)*(7/8)))
test_that("test 12",  expect_true(probaTsachantS(Tk=2,Sj=3,p=7/8,K=3)== 1/(8)*(7/8)))
test_that("test 13",  expect_true(probaTsachantS(Tk=3,Sj=3,p=7/8,K=3)== 7/8))
test_that("test 14", expect_true(probaTsachantS(Tk=3,Sj=0,p=7/8,K=3)== 0))
test_that("test 15", expect_true(probaTsachantS(Tk=4,Sj=4,p=7/8,K=3)== 7/8))
test_that("test 16", expect_true(probaTsachantS(Tk=3,Sj=1,p=7/8,K=3) == 0))
test_that("test 17", expect_true(probaTsachantS(Tk=3,Sj=2,p=7/8,K=3) == 0))
test_that("test 18", expect_true(probaTsachantS(Tk=0,Sj=8,p=7/8,K=3) == 1/(8^8)))
test_that("test 19", expect_true(probaTsachantS(Tk=8,Sj=8,p=7/8,K=3) ==7/8))
test_that("test 20", expect_true(probaTsachantS(Tk=8,Sj=9,p=7/8,K=3) ==7/8*(1/8)))
test_that("test 21", expect_true(probaTsachantS(Tk=9,Sj=8,p=7/8,K=3) ==0))

# verif modèle last
test_that("test 22", expect_true(probaTsachantS(Tk=0,Sj=0,p=7/8,eps=0.1,K=3,modelEps="last")==0.9))
test_that("test 23", expect_true(probaTsachantS(Tk=1,Sj=0,p=7/8,eps=0.1,K=3)==0.1))
test_that("test 24", expect_true(probaTsachantS(Tk=2,Sj=0,p=7/8,eps=0.1,K=3)==0))
test_that("test 25", expect_true(probaTsachantS(Tk=4,Sj=4,p=2/3,eps=0.1,modelEps="last",K=4)==2/3))


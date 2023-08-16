test_that("test 2", expect_true(probaTinterS(probaS=rep(1/4,4),Tk=3,Sj=3,p=7/8)== 7/8*0.25))


test_that("test 1", expect_true(probaTinterS(probaS=rep(1/4,4),Tk=0,Sj=3,p=7/8)+
  probaTinterS(probaS=rep(1/4,4),Tk=1,Sj=3,p=7/8)+
  probaTinterS(probaS=rep(1/4,4),Tk=2,Sj=3,p=7/8)+
  probaTinterS(probaS=rep(1/4,4),Tk=3,Sj=3,p=7/8)==0.25))

pits=matrix(NA,4,4)
for(i in 1:4)
{
  for(j in 1:4)
  {
    pits[i,j]=probaTinterS(probaS=rep(1/4,4),Tk=i-1,Sj=j-1,p=7/8)
  }
}

test_that("test 3", expect_true(sum(pits)==1))

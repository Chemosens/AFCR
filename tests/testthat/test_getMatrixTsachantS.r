mat=AFCR:::getMatrixTsachantS(p=2/3,K=3)
vecS=rep(1/4,4)
vecS%*%mat
all(AFCR:::probaT(p=2/3,probaS=vecS)==vecS%*%mat)

mat=AFCR:::getMatrixTsachantS(p=2/3,K=3,eps=0.1,modelEps="last")
library(MASS)

ginv(mat)

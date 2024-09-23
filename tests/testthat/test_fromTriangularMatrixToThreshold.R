data(triangular)
triangularMatrix=getTriangularMatrix(triangular,increasingConcentrations=paste0("C",1:9),subjectName="Paneliste", productName="Produit", descriptorName="Descripteur",timeName="Temps")
fromTriangularMatrixToThreshold(triangularMatrix)
  

triangularMatrix2=matrix(c(rep(1,5),c(0,rep(1,4)),c(0,0,1,1,1),c(0,0,0,1,1),c(0,0,0,0,1)),5,5,byrow=T)
rownames(triangularMatrix2)=paste0("S",1:5);colnames(triangularMatrix2)=paste0("C",1:5)
res=fromTriangularMatrixToThreshold(triangularMatrix2)
all(res==c("C1--","C1-C2", "C2-C3", "C3-C4", "C4-C5") )


data(triangular)
triangularMatrix=getTriangularMatrix(triangular,descriptorName="Descripteur",timeName="Temps",increasingConcentrations=paste0("C",1:9))
houghCorrection(triangularMatrix)
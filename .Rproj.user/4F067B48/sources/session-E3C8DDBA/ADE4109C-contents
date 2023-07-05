data(triangular)
colnames(triangular)=c("Séance","Répétition","Prise","Panéliste","Produit","Descripteur","Score","Temps")
df=keepLastOccurence(triangular)
test_that("test number of clicks",expect_true(df[1,"nClicks"]==2))

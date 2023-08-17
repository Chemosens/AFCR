data(triangular)
data(rata)
#' p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
#' subjectName="Paneliste",productName="Produit",scoreName="Score")
#' p
#' data(triangular)
p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular)

p=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=T)
test_that("result is a proba 1 (4)",expect_true(!is.null(p)))



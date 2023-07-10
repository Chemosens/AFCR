\name{AFCR-package}
\alias{AFCR-package}
\alias{AFCR}
\docType{package}
\title{
\packageTitle{AFCR}
}
\description{
\packageDescription{AFCR}
}

\author{
\packageAuthor{AFCR}

Maintainer: \packageMaintainer{AFCR}
}

\keyword{ package }
\seealso{\link{keepLastOccurence},\link{analyseScores},\link{getThreshold}
}
\examples{
# Create the dataset
data(triangular)
data(rata)
res=keepLastOccurence(triangular,subjectName="Paneliste")
seuils=getThreshold(res=res,
 decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),rata=rata)
p=analyseScores(rata,subjectName="Paneliste")
p
# library(plotly)
# ggplotly(p)
}

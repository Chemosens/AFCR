# AFCR

## Description
This package has two objectives: (i) to correct successive AFC tests (Best Estimation Threshold experiments) using probabilistic approach (ii) to deal with intensities data

## Installation
You can install the development version from GitHub using:

```R
devtools::install_github("https://github.com/ChemoSens/AFCR")
library(AFCR)
```
# Example of usage

## Getting BET results
```R
data(triangular)
head(triangular)
df=keepLastOccurence(triangular,subjectName="Paneliste")
res_bet=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
                     subjectName="Paneliste")
## Using real concentrations
bet1=getThreshold(res=df,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
                  decreasingNumConcentrations=9:1,maxConc=10, minConc=0,subjectName="Paneliste")
```
## Getting IBT results
```R
data(rata)
res_ibt=getIBT(intensityData=rata,subjectName="Paneliste",decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"))$threshold
# Using real concentrations
res_ibt=getIBT(intensityData=rata,subjectName="Paneliste",
               decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"),
               decreasingNumConcentrations=9:1,maxConc=10,minConc=0)$threshold
```

## Graphical results
```R
res_graph=analyseScores(rata,decreasingConcentrations=c("C9","C8","C7","C6","C5","C4","C3","C2","C1"), 
        subjectName="Paneliste",productName="Produit",scoreName="Score",triangular=triangular,displayAFC=TRUE,representationAFC = "label",subject="S001",decreasingNumConcentrations=9:1,maxConc=10,minConc=0)
```




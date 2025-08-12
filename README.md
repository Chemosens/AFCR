# AFCR

## Description
The objective of this package is to correct successive AFC tests (Best Estimation Threshold experiments) using probabilistic approach and simulations.

<strong>Important remark: the package presented in Martin et al. (2025) was initially included in this package. It is now part of the BETR package: https://github.com/Chemosens/BETR/</strong>
## Installation
You can install the development version from GitHub using:

```R
devtools::install_github("https://github.com/ChemoSens/AFCR")
library(AFCR)

```
## Example of usage
Here, we assume that 6 successive 3-AFC tests are conducted. The true threshold distribution can be stored in an vector S. 
```R
pS=rep(0,7);pS[4]=1
```
Then, assuming this true distribution, the distribution of the observed BET is given by the following commands. 
```R
matTassumingS=AFCR:::getMatrixTsachantS(2/3,6)
pT_estimated=pS%*%matTassumingS
```
The following commands allows graphs to be plotted for comparing the true threshold distribution and the related observed BET
```R
library(ggplot2)
library(gridExtra)
pT=as.vector(pT_estimated)
dfT=data.frame(t=pT,names=paste0("]c",0:6,";c",1:7,"]"))
dfS=data.frame(t=pS,names=paste0("]c",0:6,";c",1:7,"]"))
dfT[,"t_lab"]=round(dfT[,"t"],digits=2)
dfS[,"t_lab"]=round(dfS[,"t"],digits=2)
real=ggplot(dfS,aes(x=names,y=t))+geom_col()+ylim(0,1)+ggtitle("a. Distribution of the true threshold")+theme_bw()+ylab("Probability")+xlab("Threshold")+ geom_text(aes(label = t_lab), vjust = -0.5)
obs=ggplot(dfT,aes(x=names,y=t))+geom_col()+ylim(0,1)+ggtitle("b. Distribution of BET threshold")+theme_bw()+xlab("Threshold")+ylab("Probability")+ geom_text(aes(label = t_lab), vjust = -0.5)
grid.arrange(real,obs,nrow=1)
```

Other examples are available in https://github.com/ChemoSens/ExternalCode/AFCR_Results/script_paper.r

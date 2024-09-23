difference1=c(-1,-3,-5,-2,-1,0,1)
all(isSignif(difference1,tol=0)==c("green", "green", "green", "green", "green", "black", "red"  ))
all(isSignif(difference1,tol=2)== c("black" ,"green", "green", "black", "black", "black", "black"))

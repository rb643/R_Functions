# function to calculate model fits for granger causality tests across a range of lag
# mostly useful if you want to optimize the lag while testing granger causality 

calcLag<-function(x,y,max.lag) {
  y<-as.numeric(y)
  y.lag<-embed(y,max.lag+1)[,-1,drop=FALSE]
  x.lag<-embed(x,max.lag+1)[,-1,drop=FALSE]

  t<-tail(seq_along(y),nrow(y.lag))

  ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))

  pvals<-mapply(function(i) anova(ms[[i]],ms[[i-1]])[2,"Pr(>F)"],max.lag:2)
  ind<-which(pvals<0.05)[1]
  ftest<-ifelse(is.na(ind),1,max.lag-ind+1)

  aic<-as.numeric(lapply(ms,AIC))
  bic<-as.numeric(lapply(ms,BIC))
  structure(list(ic=cbind(aic=aic,bic=bic),pvals=pvals,
                 selection=list(aic=which.min(aic),bic=which.min(bic),ftest=ftest)))
}

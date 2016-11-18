

tickers <- c("SPY", "EEM", "TLT")
getSymbols(tickers)

install.packages("quantmod")

spy <- Ad(SPY)
eem <- Ad(EEM)
tlt <- Ad(TLT)

pr <- merge.xts(spy, eem, tlt)
rs <- na.omit(diff(log(pr)))

mycov <- cov(rs)

mycov


mc <- mrc(w, mycov)

w <- PERC(mycov)
ww <- Weights(w)

mrc(ww, mycov)

PortOptimMRC = function(Sigma){
  
  w = rep(1/ncol(Sigma),ncol(Sigma))
  
  Opt = function(w,Sigma){
    
    Sigma <- as.matrix(Sigma)
    
    w <- as.vector(w)
    
    sigma <- c(sqrt(t(w) %*% Sigma %*% w))
    
    sw <- Sigma %*% w
    
    dw <- c(w * sw/sigma)
    
    return(var(dw))
    
  }
  
  
  #Veränderbare Zeile ist "w", und diese wird nicht angezeigt
  OptimResult = nlminb(start = rep(1/ncol(Sigma),ncol(Sigma)), objective = Opt, Sigma = Sigma)
  
  return (OptimResult$par/sum(OptimResult$par))
  
  
  
}
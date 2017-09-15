# define function that returns the SSE for different local regression smoothing kernels
calcSSE <- function(z,x,y){
  loessMod <- try(loess.sd(x ~ y, span=z), silent=T)
  res <- try(loessMod$model$residuals, silent=T)
  if(class(res)!="try-error"){
    if(sum(res, na.rm=T) > 0){
      sse <- sum(res^2)
    }
    else{
      sse <- 99999999 # assuming your actual SSE will be lower than this value
    }
  }else{
    sse <- 99999999
  }
  return(sse)
}

bias_var <- function(var.future,data){
  width = ncol(ref)
  height = nrow(ref)
  
  bias_future <- array(0,c(nrow = height,ncol = width,(length(model_names)-1)))
  
  for(i in 1:(length(model_names)-1)){
    bias_future[,,i] <- var_future[,,i]- ref_future
  }
  
  var_gc <- bias_gc <- matrix(0,nrow = height,ncol = width)
  
  for(l in 0:(length(model_names)-2)){
    islabel <- which(data == l)
    var_gc[islabel] <- var_future[,,(l+1)][islabel]
    bias_gc[islabel] <- bias_future[,,(l+1)][islabel]
  }
  
  bias_var <- vector("list",length=2)
  
  bias_var <- list("Var" = var_gc,"Bias" = bias_gc)
  
  return(bias_var)
}

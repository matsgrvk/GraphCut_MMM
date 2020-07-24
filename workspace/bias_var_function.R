bias_var <- function(var.future,gc.data){
  nlabs = length(model_names)-1 ### Number of labels used in the GC. Needs to be total number -1 since C++ index starts from 0
  width = ncol(ref)
  height = nrow(ref)
  
  bias_future <- array(0,c(nrow = height,ncol = width,(length(model_names)-1)))
  
  for(i in 1:(length(model_names)-1)){
    bias_future[,,i] <- var_future[,,i]- ref_future
  }
  
  var_gc <- bias_gc <- array(0,c(nrow = height,ncol = width,length(model_names)))
  
  for(l in 0:(length(model_names)-2)){
    islabel <- which(gc.data[,,m] == l)
    var_gc[,,m][islabel] <- var_future[,,(l+1)][islabel]
    bias_gc[,,m][islabel] <- bias_future[,,(l+1)][islabel]
  }
  
  bias_var_result <- vector("list",length=2)
  
  bias_var_result <- list(var_gc,bias_gc)
  
  return(bias_var_result)
}


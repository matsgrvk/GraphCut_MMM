list.of.packages <- c("RcppXPtrUtils","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
# install_github("thaos/gcoWrapR")
library(gcoWrapR)


mmm <- function(var.present,var.future){
  
  ### var_present and var_future : arrays of models outputs used in the GraphCut
  
  width = ncol(ref) ### Number of longitudes
  height = nrow(ref) ### Number of latitudes
  
  
  # Preparing the data to perform the MMM
  
  bias <- bias_future <- array(0,c(nrow = height,ncol = width,(length(model_names)-1))) ### Creating the bias arrays to set the data and smooth cost functions
  
  for(i in 1:(length(model_names)-1)){
    bias[,,i] <- var_present[,,i] - ref
    bias_future[,,i] <- var_future[,,i] - ref_future
  }
  
  tmp_var_mmm <- tmp_bias_mmm <- array(0,c(nrow = height,ncol = width,length(model_names)))
  var_mmm <- bias_mmm <-  matrix(0, nrow = height, ncol = width)
  
  for(i in 1:length(var_list_future)){
    tmp_tas_mmm[,,i] <- var_future[[i]]
  }
  
  var_mmm <- apply(tmp_tas_mmm, 1:2, mean)
  
  for(i in 1:length(bias_list_future)){
    tmp_bias_mmm[,,i] <- bias_future[[i]]
  }
  
  bias_mmm <- apply(tmp_bias_mmm, 1:2, mean)
  
  
  bias_var_mmm <- vector("list",length=2)
  
  bias_var_mmm <- list("Var" = var_mmm, "Bias" = bias_mmm)
  
  return(bias_var_mmm)
  
}
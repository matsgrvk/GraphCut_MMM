list.of.packages <- c("RcppXPtrUtils","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
# install_github("thaos/gcoWrapR")
library(gcoWrapR)


min_bias <- function(var.present,var.future){
  
  ### var_present and var_future : arrays of models outputs used in the GraphCut
  
  width = ncol(ref) ### Number of longitudes
  height = nrow(ref) ### Number of latitudes
  
  
  # Preparing the data to perform min bias
  
  bias <- bias_future <- array(0,c(nrow = height,ncol = width,(length(model_names)-1))) ### Creating the bias arrays to set the data and smooth cost functions
  
  for(i in 1:(length(model_names)-1)){
    bias[,,i] <- var_present[,,i] - ref
    bias_future[,,i] <- var_future[,,i] - ref_future
  }
  
  label_attribution <- matrix(0,nrow(ref),ncol(ref)) ### Creating the initialization matrix based on the best model from the previous list
  
  for(x in 1:nrow(ref)){
    for(y in 1:ncol(ref)){
      label_attribution[x,y] = which.min(abs(bias[x,y,]))-1
    }
  }
  
  return(label_attribution)
}
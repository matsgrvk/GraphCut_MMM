list.of.packages <- c("RcppXPtrUtils","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
# install_github("thaos/gcoWrapR")
library(gcoWrapR)

graphcut <- function(var.present,var.future,gc.type,weight.data,weight.smooth){
  
  ### var_present and var_future : arrays of models outputs used in the GraphCut
  ### name : what type of graph cut will be created : present, hybrid, future.
  ### weight.data and weight.smooth : number (1 for both is approximatly the optimal choice)
  
  nlabs = length(model_names)-1 ### Number of labels used in the GC. Needs to be total number -1 since C++ index starts from 0
  width = ncol(ref) ### Number of longitudes
  height = nrow(ref) ### Number of latitudes
  
  # Instanciation of the GraphCut environment
  
  gco <- new(GCoptimizationGridGraph, width, height, nlabs)
  
  # Preparing the DataCost and SmoothCost functions of the GraphCut in C++
  
  cat("Creating DataCost function...  ")
  
  ptrDataCost <- cppXPtr(
    code = 'float dataFn(int p, int l, Rcpp::List extraData)
{
  int numPix = extraData["numPix"];
  float weight = extraData["weight"];
  NumericVector data = extraData["data"];
  return(weight * std::abs(data[p + numPix * l]) );
}',
  includes = c("#include <math.h>", "#include <Rcpp.h>"),
  rebuild = TRUE, showOutput = FALSE, verbose = FALSE
  )
  
  cat("Creating SmoothCost function...  ")
  
  ptrSmoothCost <- cppXPtr(
    code = 'float smoothFn(int p1, int p2, int l1, int l2, Rcpp::List extraData)
{
  int numPix = extraData["numPix"];
  float weight = extraData["weight"];
  NumericVector data = extraData["data"];
  float cost = std::abs(data[p1 + numPix * l1]-data[p1 + numPix * l2])
  + std::abs(data[p2 + numPix * l1] - data[p2 + numPix * l2]) ;
  return(weight * cost);
}',
  includes = c("#include <math.h>", "#include <Rcpp.h>"),
  rebuild = TRUE, showOutput = FALSE, verbose = FALSE
  )
  
  # Preparing the data to perform GraphCut
  
  bias <- bias_future <- array(0,c(nrow = height,ncol = width,(length(model_names)-1))) ### Creating the bias arrays to set the data and smooth cost functions
  
  for(i in 1:(length(model_names)-1)){
    bias[,,i] <- var_present[,,i] - ref
    bias_future[,,i] <- var_future[,,i] - ref_future
  }
  
  bias_cpp = c(aperm(bias, c(2, 1, 3))) ### Permuting longitude and latitude since the indexing isn't the same in R and in C++
  bias_future_cpp = c(aperm(bias_future, c(2, 1, 3)))
  
  if(gc.type == "present"){ ### Data and smooth cost calibrated on present data
    gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias_cpp, weight = weight.data))
    gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias_cpp, weight = weight.smooth))
  }
  
  if(gc.type == "hybrid"){ ### Data cost calibrated on present data and smooth cost calibrated on future data
    gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias_cpp, weight = weight.data))
    gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias_future_cpp, weight = weight.smooth))
  }
  
  if(gc.type == "future"){ ### Data and smooth cost calibrated on future data
    gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias_future_cpp, weight = weight.data))
    gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias_future_cpp, weight = weight.smooth))
  }
  
  mae_list <- vector("list",(length(model_names)-1)) ### Creating a list to select the best model among the ensemble in terms of mean absolute bias
  
  for(i in 1:length(mae_list)){
    mae_list[[i]] <- mean(abs(bias[,,i]))
  }
  
  mat_init <- matrix(which.min(mae_list),nrow(ref),ncol(ref)) ### Creating the initialization matrix based on the best model from the previous list
  
  vec_init <- as.vector(t(mat_init)) ### Permuting the indexing from R to C++
  
  for(z in 0:(length(mat_init)-1)){ ### Setting the labels for the alpha-beta swap algorithm
    gco$setLabel(z,vec_init[z+1]) 
  }
  
  # Optimizing the MRF energy with alpha-beta swap (-1 refers to the optimization until convergence)
  
  cat("Starting GraphCut optimization...  ")
  
  begin <- Sys.time()
  
  gco$swap(-1) ### Optimization with alpha-beta swap algorithm
  
  time_spent <- Sys.time()-begin
  
  cat("GraphCut optimization done :  ")
  print(time_spent)
  
  label_attribution <- matrix(0,nrow = height,ncol = width) ### Creating the label attribution matrix
  data_smooth_list <- vector("list",length(model_names)) ### Values of data and smooth energy for each GraphCut 
  
  data_cost <- gco$giveDataEnergy() ### Storing the energies for the GraphCut
  smooth_cost <- gco$giveSmoothEnergy()
  
  data_smooth_list <- list("Data cost" = data_cost, "Smooth cost" = smooth_cost)
  
  for(j in 1:height){
    for(i in 1:width){
      label_attribution[j,i] <- gco$whatLabel((i - 1) + width * (j - 1)) ### Permuting from the C++ indexing to the R indexing
    }
  }
  
  gc_result <- vector("list",length=2)
  
  gc_result <- list("Label attribution" = label_attribution, "Data and smooth cost" = data_smooth_list)
  
  return(gc_result)
}
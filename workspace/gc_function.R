list.of.packages <- c("ncdf4","RcppXPtrUtils","paramtest","reshape2","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

graphcut <- function(var_present,var_future,weight_data,weight_smooth){
  
  ### var_present and var_future : arrays of models outputs used in the GraphCut
  ### weight_data and weight_smooth : number (1 for both is approximatly the optimal choice)
  
  nlabs = length(model_names)-1 ### Number of labels used in the GC. Needs to be total number -1 since C++ index starts from 0
  width = ncol(ref)
  height = nrow(ref)
  
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
  
  label_attribution <- array(0,c(nrow = height,ncol = width,length(model_names))) ### Each dimension contains the GraphCut result for a given reference
  data_smooth_list <- vector("list",length(model_names)) ### Values of data and smooth energy for each GraphCut 
  
  t=0
  
  bias <- bias_future <- array(0,c(nrow = height,ncol = width,length(model_names))) ### Creating the bias arrays to set the data and smooth cost functions
  
  for(i in 1:length(model_names)){
    bias[,,i] <- var_present[,,i] - ref
    bias_future[,,i] <- var_future[,,i] - ref_future
  }
  
  bias_cpp = c(aperm(bias, c(2, 1, 3))) ### Permuting longitude and latitude since the indexing isn't the same in R and in C++
  bias_future_cpp = c(aperm(bias_future, c(2, 1, 3)))
  
  gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias_cpp, weight = weight_data))
  gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias_cpp, weight = weight_smooth))
  
  mat_init <- matrix(0,height,width) ### Creating an initialization matrix
  
  vec_init <- as.vector(t(mat_init)) ### Permuting the indexing from R to C++
  
  for(z in 0:(length(mat_init)-1)){ 
    gco$setLabel(z,vec_init[z+1]) ### Setting the labels for the alpha-beta swap algorithm
  }
  
  # Optimizing the MRF energy with alpha-beta swap (-1 refers to the optimization until convergence)
  
  cat("Starting GraphCut optimization...  ")
  
  begin <- Sys.time()
  
  gco$swap(-1)
  
  time_spent <- Sys.time()-begin
  
  cat("GraphCut optimization done :  ")
  print(time_spent)
  
  data_cost <- gco$giveDataEnergy() ### Storing the energies for the GraphCut
  smooth_cost <- gco$giveSmoothEnergy()
  
  t=t+1
  
  data_smooth_list[[t]] <- list("Data cost" = data_cost, "Smooth cost" = smooth_cost)
  
  tmp_label_attribution <- matrix(0, nrow=height, ncol=width) ### Creating the label attribution matrix
  
  for(j in 1:height){
    for(i in 1:width){
      tmp_label_attribution[j,i] <- gco$whatLabel((i - 1) + width * (j - 1)) ### Permuting from the C++ indexing to the R indexing
    }
  }
  
  label_attribution[,,t] <- tmp_label_attribution
}


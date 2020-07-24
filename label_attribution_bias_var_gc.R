var="tas" #### Variable utilisée : "tas" ou "pr"

setwd("/homel/mgarvik/Bureau/Stage_M2/Codes/clean_files/workspace/")
load("models_list.rdata")
load(paste0("load_",var,"_cv.rdata"))

##### GraphCut ######

model_names <- TAS_MODELS[c(1,3,4,5,7,8,11,12,13,16,17,20,22,24,26,30,31,33,34,37),2] #### Names of models used in the ensemble

gc_result <- list("vector",length(model_names)) ### List of label attribution matrices obtained with GraphCut

source("gc_function.R")

for(m in 1:2){
  
  ref <- get(paste0("tas_",model_names[[m]]))
  ref_future <- get(paste0("tas_",model_names[[m]],"_2100"))
  
  var_present <- var_future <- array(0,c(nrow = nrow(ref),ncol = ncol(ref),length(model_names)))
  
  for(i in 1:length(model_names)){
    var_present[,,i] <- get(paste0(var,"_",model_names[[i]]))
    var_future[,,i] <- get(paste0(var,"_",model_names[[i]],"_2100"))
  }
  
  var_present <- var_present[,,-m]
  var_future <- var_future[,,-m]
  
  gc_result[[m]] <- graphcut(var.present = var_present,
                      var.future = var_future,
                      gc.type = "present",
                      weight.data = 1,
                      weight.smooth = 1)
}



##### Bias and var reconstruction for GraphCut result #####

bias_var_result <- list("vector",length(gc_result)) ### List of variable and variable biases obtained with GraphCut

for(m in 1:length(gc_result)){
  
  source("bias_var_function.R")
  
  ref_future <- get(paste0("tas_",model_names[[m]],"_2100"))
  var_future <- array(0,c(nrow = nrow(ref),ncol = ncol(ref),length(model_names)))
  
  for(i in 1:length(model_names)){
    var_future[,,i] <- get(paste0(var,"_",model_names[[i]],"_2100"))
  }
  
  var_future <- var_future[,,-m]
  
  bias_var_result[[m]] <- bias_var(var.future = var_future,
                              gc.data = gc_result[[m]])
}

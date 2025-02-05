var = "tas"

load("input/models_list.rdata")
load(paste0("input/load_",var,"_cv.rdata"))
load("input/dimensions.rdata")
setwd("output/")
load(paste0("label_attrib_bias_",var,"_gc_present.rdata"))
load(paste0("label_attrib_bias_",var,"_gc_hybrid.rdata"))
load(paste0("label_attrib_bias_",var,"_gc_hybrid_light.rdata"))
load(paste0("label_attrib_bias_",var,"_gc_hybrid_strong.rdata"))
load(paste0("label_attrib_bias_",var,"_gc_future.rdata"))
load(paste0("bias_",var,"_mmm.rdata"))
load(paste0("label_attrib_bias_",var,"_min_bias.rdata"))
setwd('..')

model_names <- TAS_MODELS[c(1,3,4,5,7,8,11,12,13,16,17,20,22,24,26,30,31,33,34,37),2] #### Names of models used in the ensemble


##### Bias var #####


mae_gc_present <- mae_gc_hybrid <- mae_gc_hybrid_light <- mae_gc_hybrid_strong <- mae_gc_future <- mae_mmm <- mae_min_bias <- vector("list",length(model_names))

for(i in 1:length(mae_gc_present)){
  mae_gc_present[[i]] <- mean(abs(label_attribution_bias_present[,,i]))
  mae_gc_hybrid[[i]] <- mean(abs(label_attribution_bias_hybrid[,,i]))
  mae_gc_hybrid_light[[i]] <- mean(abs(label_attribution_bias_hybrid_light[,,i]))
  mae_gc_hybrid_strong[[i]] <- mean(abs(label_attribution_bias_hybrid_strong[,,i]))
  mae_gc_future[[i]] <- mean(abs(label_attribution_bias_future[,,i]))
  mae_mmm[[i]] <- mean(abs(bias_mmm[,,i]))
  mae_min_bias[[i]] <- mean(abs(label_attribution_bias_min_bias[,,i]))
}

mae_list <- list(mae_gc_present,mae_gc_hybrid,mae_gc_hybrid_light,mae_gc_hybrid_strong,mae_gc_future,mae_mmm,mae_min_bias)


median_mae_gc_present <- median_mae_gc_hybrid <- median_mae_gc_hybrid_light <- median_mae_gc_hybrid_strong <- median_mae_gc_future <- median_mae_mmm <- median_mae_min_bias <- vector("list",length(model_names))

for(i in 1:length(median_mae_gc_present)){
  median_mae_gc_present[[i]] <- quantile(abs(label_attribution_bias_present[,,i]),probs=0.5)
  median_mae_gc_hybrid[[i]] <- quantile(abs(label_attribution_bias_hybrid[,,i]),probs=0.5)
  median_mae_gc_hybrid_light[[i]] <- quantile(abs(label_attribution_bias_hybrid_light[,,i]),probs=0.5)
  median_mae_gc_hybrid_strong[[i]] <- quantile(abs(label_attribution_bias_hybrid_strong[,,i]),probs=0.5)
  median_mae_gc_future[[i]] <- quantile(abs(label_attribution_bias_future[,,i]),probs=0.5)
  median_mae_mmm[[i]] <- quantile(abs(bias_mmm[,,i]),probs=0.5)
  median_mae_min_bias[[i]] <- quantile(abs(label_attribution_bias_min_bias[,,i]),probs=0.5)
}

median_mae_list <- list(median_mae_gc_present,median_mae_gc_hybrid,median_mae_gc_hybrid_light,median_mae_gc_hybrid_strong,median_mae_gc_future,median_mae_mmm,median_mae_min_bias)

##### Gradient bias #####

ref <- gc_present <- gc_hybrid <- gc_hybrid_light <- gc_hybrid_strong <- gc_future <- mmm <- min_bias <- array(0, c(nrow = height, ncol = width,length(model_names)))

for(m in 1:length(model_names)){
  ref[,,m] <- get(paste0("tas_",model_names[[m]],"_2100"))
  gc_present[,,m] <- label_attribution_tas_present[,,m]
  gc_hybrid[,,m] <- label_attribution_tas_hybrid[,,m]
  gc_hybrid_light[,,m] <- label_attribution_tas_hybrid_light[,,m]
  gc_hybrid_strong[,,m] <- label_attribution_tas_hybrid_strong[,,m]
  gc_future[,,m] <- label_attribution_tas_future[,,m]
  mmm[,,m] <- tas_mmm[,,m]
  min_bias[,,m] <- label_attribution_tas_min_bias[,,m]
}

gc_present_grad <- gc_hybrid_grad <- gc_future_grad <- mmm_grad <- min_bias_grad <- array(0, c(nrow(ref),ncol(ref),length(model_names)))

ref_centered <- ref[2:(nrow(ref)-1),2:(ncol(ref)-1),]
ref_up <- ref[1:(nrow(ref)-2),2:(ncol(ref)-1),]
ref_down <- ref[3:nrow(ref),2:(ncol(ref)-1),]
ref_left <- ref[2:(nrow(ref)-1),1:(ncol(ref)-2),]
ref_right <- ref[2:(nrow(ref)-1),3:ncol(ref),]

gc_present_centered <- gc_present[2:(nrow(gc_present)-1),2:(ncol(gc_present)-1),]
gc_present_up <- gc_present[1:(nrow(gc_present)-2),2:(ncol(gc_present)-1),]
gc_present_down <- gc_present[3:nrow(gc_present),2:(ncol(gc_present)-1),]
gc_present_left <- gc_present[2:(nrow(gc_present)-1),1:(ncol(gc_present)-2),]
gc_present_right <- gc_present[2:(nrow(gc_present)-1),3:ncol(gc_present),]

gc_hybrid_centered <- gc_hybrid[2:(nrow(gc_hybrid)-1),2:(ncol(gc_hybrid)-1),]
gc_hybrid_up <- gc_hybrid[1:(nrow(gc_hybrid)-2),2:(ncol(gc_hybrid)-1),]
gc_hybrid_down <- gc_hybrid[3:nrow(gc_hybrid),2:(ncol(gc_hybrid)-1),]
gc_hybrid_left <- gc_hybrid[2:(nrow(gc_hybrid)-1),1:(ncol(gc_hybrid)-2),]
gc_hybrid_right <- gc_hybrid[2:(nrow(gc_hybrid)-1),3:ncol(gc_hybrid),]

gc_hybrid_light_centered <- gc_hybrid_light[2:(nrow(gc_hybrid_light)-1),2:(ncol(gc_hybrid_light)-1),]
gc_hybrid_light_up <- gc_hybrid_light[1:(nrow(gc_hybrid_light)-2),2:(ncol(gc_hybrid_light)-1),]
gc_hybrid_light_down <- gc_hybrid_light[3:nrow(gc_hybrid_light),2:(ncol(gc_hybrid_light)-1),]
gc_hybrid_light_left <- gc_hybrid_light[2:(nrow(gc_hybrid_light)-1),1:(ncol(gc_hybrid_light)-2),]
gc_hybrid_light_right <- gc_hybrid_light[2:(nrow(gc_hybrid_light)-1),3:ncol(gc_hybrid_light),]

gc_hybrid_strong_centered <- gc_hybrid_strong[2:(nrow(gc_hybrid_strong)-1),2:(ncol(gc_hybrid_strong)-1),]
gc_hybrid_strong_up <- gc_hybrid_strong[1:(nrow(gc_hybrid_strong)-2),2:(ncol(gc_hybrid_strong)-1),]
gc_hybrid_strong_down <- gc_hybrid_strong[3:nrow(gc_hybrid_strong),2:(ncol(gc_hybrid_strong)-1),]
gc_hybrid_strong_left <- gc_hybrid_strong[2:(nrow(gc_hybrid_strong)-1),1:(ncol(gc_hybrid_strong)-2),]
gc_hybrid_strong_right <- gc_hybrid_strong[2:(nrow(gc_hybrid_strong)-1),3:ncol(gc_hybrid_strong),]

gc_future_centered <- gc_future[2:(nrow(gc_future)-1),2:(ncol(gc_future)-1),]
gc_future_up <- gc_future[1:(nrow(gc_future)-2),2:(ncol(gc_future)-1),]
gc_future_down <- gc_future[3:nrow(gc_future),2:(ncol(gc_future)-1),]
gc_future_left <- gc_future[2:(nrow(gc_future)-1),1:(ncol(gc_future)-2),]
gc_future_right <- gc_future[2:(nrow(gc_future)-1),3:ncol(gc_future),]

mmm_centered <- mmm[2:(nrow(mmm)-1),2:(ncol(mmm)-1),]
mmm_up <- mmm[1:(nrow(mmm)-2),2:(ncol(mmm)-1),]
mmm_down <- mmm[3:nrow(mmm),2:(ncol(mmm)-1),]
mmm_left <- mmm[2:(nrow(mmm)-1),1:(ncol(mmm)-2),]
mmm_right <- mmm[2:(nrow(mmm)-1),3:ncol(mmm),]

min_bias_centered <- min_bias[2:(nrow(min_bias)-1),2:(ncol(min_bias)-1),]
min_bias_up <- min_bias[1:(nrow(min_bias)-2),2:(ncol(min_bias)-1),]
min_bias_down <- min_bias[3:nrow(min_bias),2:(ncol(min_bias)-1),]
min_bias_left <- min_bias[2:(nrow(min_bias)-1),1:(ncol(min_bias)-2),]
min_bias_right <- min_bias[2:(nrow(min_bias)-1),3:ncol(min_bias),]

gc_present_grad <- gc_hybrid_grad <- gc_hybrid_light_grad <- gc_hybrid_strong_grad <- gc_future_grad <- mmm_grad <- min_bias_grad <- array(0,c(nrow(ref_centered),ncol(ref_centered),length(model_names)))

for(m in 1:length(model_names)){
  gc_present_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_present_up[,,m]-gc_present_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_present_down[,,m]-gc_present_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_present_left[,,m]-gc_present_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_present_right[,,m]-gc_present_centered[,,m]))^2)/4
  gc_hybrid_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_hybrid_up[,,m]-gc_hybrid_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_hybrid_down[,,m]-gc_hybrid_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_hybrid_left[,,m]-gc_hybrid_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_hybrid_right[,,m]-gc_hybrid_centered[,,m]))^2)/4
  gc_hybrid_light_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_hybrid_light_up[,,m]-gc_hybrid_light_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_hybrid_light_down[,,m]-gc_hybrid_light_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_hybrid_light_left[,,m]-gc_hybrid_light_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_hybrid_light_right[,,m]-gc_hybrid_light_centered[,,m]))^2)/4
  gc_hybrid_strong_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_hybrid_strong_up[,,m]-gc_hybrid_strong_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_hybrid_strong_down[,,m]-gc_hybrid_strong_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_hybrid_strong_left[,,m]-gc_hybrid_strong_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_hybrid_strong_right[,,m]-gc_hybrid_strong_centered[,,m]))^2)/4
  gc_future_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_future_up[,,m]-gc_future_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_future_down[,,m]-gc_future_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_future_left[,,m]-gc_future_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_future_right[,,m]-gc_future_centered[,,m]))^2)/4
  mmm_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(mmm_up[,,m]-mmm_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(mmm_down[,,m]-mmm_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(mmm_left[,,m]-mmm_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(mmm_right[,,m]-mmm_centered[,,m]))^2)/4
  min_bias_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(min_bias_up[,,m]-min_bias_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(min_bias_down[,,m]-min_bias_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(min_bias_left[,,m]-min_bias_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(min_bias_right[,,m]-min_bias_centered[,,m]))^2)/4
}

lon_grad <- 1:nrow(ref_centered)
lat_grad <- (1:ncol(ref_centered))-90

mae_grad_gc_present <- mae_grad_gc_hybrid <- mae_grad_gc_hybrid_light <- mae_grad_gc_hybrid_strong <- mae_grad_gc_future <- mae_grad_mmm <- mae_grad_min_bias <- vector("list",length(model_names))

for(i in 1:length(mae_grad_gc_present)){
  mae_grad_gc_present[[i]] <- mean(abs(gc_present_grad[,,i]))
  mae_grad_gc_hybrid[[i]] <- mean(abs(gc_hybrid_grad[,,i]))
  mae_grad_gc_hybrid_light[[i]] <- mean(abs(gc_hybrid_light_grad[,,i]))
  mae_grad_gc_hybrid_strong[[i]] <- mean(abs(gc_hybrid_strong_grad[,,i]))
  mae_grad_gc_future[[i]] <- mean(abs(gc_future_grad[,,i]))
  mae_grad_mmm[[i]] <- mean(abs(mmm_grad[,,i]))
  mae_grad_min_bias[[i]] <- mean(abs(min_bias_grad[,,i]))
}

mae_grad_list <- list(mae_grad_gc_present,mae_grad_gc_hybrid,mae_grad_gc_hybrid_light,mae_grad_gc_hybrid_strong,mae_grad_gc_future,mae_grad_mmm,mae_grad_min_bias)



median_mae_grad_gc_present <- median_mae_grad_gc_hybrid <- median_mae_grad_gc_hybrid_light <- median_mae_grad_gc_hybrid_strong <- median_mae_grad_gc_future <- median_mae_grad_mmm <- median_mae_grad_min_bias <- vector("list",length(model_names))

for(i in 1:length(median_mae_grad_gc_present)){
  median_mae_grad_gc_present[[i]] <- quantile(abs(gc_present_grad[,,i]),probs=0.5)
  median_mae_grad_gc_hybrid[[i]] <- quantile(abs(gc_hybrid_grad[,,i]),probs=0.5)
  median_mae_grad_gc_hybrid_light[[i]] <- quantile(abs(gc_hybrid_light_grad[,,i]),probs=0.5)
  median_mae_grad_gc_hybrid_strong[[i]] <- quantile(abs(gc_hybrid_strong_grad[,,i]),probs=0.5)
  median_mae_grad_gc_future[[i]] <- quantile(abs(gc_future_grad[,,i]),probs=0.5)
  median_mae_grad_mmm[[i]] <- quantile(abs(mmm_grad[,,i]),probs=0.5)
  median_mae_grad_min_bias[[i]] <- quantile(abs(min_bias_grad[,,i]),probs=0.5)
}

median_mae_grad_list <- list(median_mae_grad_gc_present,median_mae_grad_gc_hybrid,median_mae_grad_gc_hybrid_light,median_mae_grad_gc_hybrid_strong,median_mae_grad_gc_future,median_mae_grad_mmm,median_mae_grad_min_bias)

##### Gradient bias mean #####

mean_grad_gc_present <- mean_grad_gc_hybrid <- mean_grad_gc_hybrid_light <- mean_grad_gc_hybrid_strong <- mean_grad_gc_future <- mean_grad_mmm <- mean_grad_min_bias <- matrix(0,nrow = height - 2, ncol = width - 2)

mean_grad_gc_present[,] <- apply(gc_present_grad, 1:2, mean)
mean_grad_gc_hybrid[,] <- apply(gc_hybrid_grad, 1:2, mean)
mean_grad_gc_hybrid_light[,] <- apply(gc_hybrid_light_grad, 1:2, mean)
mean_grad_gc_hybrid_strong[,] <- apply(gc_hybrid_strong_grad, 1:2, mean)
mean_grad_gc_future[,] <- apply(gc_future_grad, 1:2, mean)
mean_grad_mmm[,] <- apply(mmm_grad, 1:2, mean)
mean_grad_min_bias[,] <- apply(min_bias_grad, 1:2, mean)

mean_grad_list <- list(mean_grad_gc_present,mean_grad_gc_hybrid,mean_grad_gc_hybrid_light,mean_grad_gc_hybrid_strong,mean_grad_gc_future,mean_grad_mmm,mean_grad_min_bias)
text_list <- list("GC present","GC hybrid","GC hybrid light","GC hybrid strong","GC future","MMM","Min bias")

colorBreaks = 10^(seq(-3, 1, by = 1))

mean_grad_gc_present_label <- mean_grad_gc_present
mean_grad_gc_hybrid_label <- mean_grad_gc_hybrid
mean_grad_gc_hybrid_light_label <- mean_grad_gc_hybrid_light
mean_grad_gc_hybrid_strong_label <- mean_grad_gc_hybrid_strong
mean_grad_gc_future_label <- mean_grad_gc_future
mean_grad_mmm_label <- mean_grad_mmm
mean_grad_min_bias_label <- matrix(1,nrow(min_bias_grad),ncol(min_bias_grad))

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_gc_present)
  mean_grad_gc_present_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid)
  mean_grad_gc_hybrid_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid_light)
  mean_grad_gc_hybrid_light_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid_strong)
  mean_grad_gc_hybrid_strong_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_gc_future)
  mean_grad_gc_future_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_mmm)
  mean_grad_mmm_label[islabel] = i
}

for(i in 1:length(colorBreaks)){
  islabel <- which(colorBreaks[i] < mean_grad_min_bias)
  mean_grad_min_bias_label[islabel] = i
}


mean_grad_label_list <- list(mean_grad_gc_present_label,
                             mean_grad_gc_hybrid_label,
                             mean_grad_gc_hybrid_light_label,
                             mean_grad_gc_hybrid_strong_label,
                             mean_grad_gc_future_label,
                             mean_grad_mmm_label,
                             mean_grad_min_bias_label)

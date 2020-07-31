test_ref <- matrix(0,nrow=10,ncol=10)
test_data <- matrix(1,nrow=10,ncol=10)

test_ref_centered <- test_ref[2:(nrow(test_ref)-1),2:(ncol(test_ref)-1)]
test_ref_up <- test_ref[1:(nrow(test_ref)-2),2:(ncol(test_ref)-1)]
test_ref_down <- test_ref[3:nrow(test_ref),2:(ncol(test_ref)-1)]
test_ref_left <- test_ref[2:(nrow(test_ref)-1),1:(ncol(test_ref)-2)]
test_ref_right <- test_ref[2:(nrow(test_ref)-1),3:ncol(test_ref)]

test_data_centered <- test_data[2:(nrow(test_data)-1),2:(ncol(test_data)-1)]
test_data_up <- test_data[1:(nrow(test_data)-2),2:(ncol(test_data)-1)]
test_data_down <- test_data[3:nrow(test_data),2:(ncol(test_data)-1)]
test_data_left <- test_data[2:(nrow(test_data)-1),1:(ncol(test_data)-2)]
test_data_right <- test_data[2:(nrow(test_data)-1),3:ncol(test_data)]

grad_test <- (((test_ref_up[,]-test_ref_centered[,])-(test_data_up[,]-test_data_centered[,]))^2+((test_ref_down[,]-test_ref_centered[,])-(test_data_down[,]-test_data_centered[,]))^2+((test_ref_left[,]-test_ref_centered[,])-(test_data_left[,]-test_data_centered[,]))^2+((test_ref_right[,]-test_ref_centered[,])-(test_data_right[,]-test_data_centered[,]))^2)/4





par(mfrow=c(4,2))

grad_list <- list(gc_present_grad,gc_hybrid_grad,gc_hybrid_light_grad,gc_hybrid_strong_grad,gc_future_grad,mmm_grad,min_bias_grad)

grad_list_pr <- list(gc_present_pr_grad , gc_hybrid_pr_grad , gc_hybrid_light_pr_grad , gc_hybrid_strong_pr_grad , gc_future_pr_grad , mmm_pr_grad , min_bias_pr_grad)
  
names_grad <- list("GC present", "GC hybrid", "GC hybrid light", "GC hybrid strong", "GC future", "MMM", "Min bias")

for(i in 1:length(grad_list)){
  hist(grad_list[[i]],
       breaks=20000,
       xlim=c(0,0.1),
       xlab="Gradient bias (°C)",
       ylim=c(0,350000),
       main=names_grad[[i]])
}

par(op)

par(mfrow=c(4,2))

for(i in 1:length(grad_list_pr)){
  hist(grad_list_pr[[i]],
       breaks="FD",
       xlim=c(0,1e-12),
       xlab="Gradient bias (mm/d)",
       ylim=c(0,250000),
       main=names_grad[[i]])
}

par(op)

var_list <- sapply(var_list,identity,simplify = "array")

var_list_centered <- var_list[2:(nrow(var_list)-1),2:(ncol(var_list)-1),]
var_list_up <- var_list[1:(nrow(var_list)-2),2:(ncol(var_list)-1),]
var_list_down <- var_list[3:nrow(var_list),2:(ncol(var_list)-1),]
var_list_left <- var_list[2:(nrow(var_list)-1),1:(ncol(var_list)-2),]
var_list_right <- var_list[2:(nrow(var_list)-1),3:ncol(var_list),]

var_list_grad <- array(0,c(nrow(ref),ncol(ref),length(model_version_20mod)))

for(m in 1:length(model_version_20mod)){
  var_list_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(var_list_up[,,m]-var_list_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(var_list_down[,,m]-var_list_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(var_list_left[,,m]-var_list_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(var_list_right[,,m]-var_list_centered[,,m]))^2)/4
}


colorBreaks = 10^(seq(-8, 1, by = 1))

image.plot(lon_grad,lat_grad,min_bias_grad[,,1],
           breaks=colorBreaks,
           col=rainbow(length(colorBreaks)-1L),
           zlim=c(min(colorBreaks),max(colorBreaks)),
           axis.args=list( at=1:length(colorBreaks), labels=colorBreaks),
           lab.breaks = names(colorBreaks))



mean_grad_gc_present_label <- mean_grad_gc_present
mean_grad_gc_hybrid_label <- mean_grad_gc_hybrid
mean_grad_gc_hybrid_light_label <- mean_grad_gc_hybrid_light
mean_grad_gc_hybrid_strong_label <- mean_grad_gc_hybrid_strong
mean_grad_gc_future_label <- mean_grad_gc_future
mean_grad_mmm_label <- mean_grad_mmm
mean_grad_min_bias_label <- mean_grad_min_bias

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_gc_present)
  mean_grad_gc_present_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid)
  mean_grad_gc_hybrid_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid_light)
  mean_grad_gc_hybrid_light_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_gc_hybrid_strong)
  mean_grad_gc_hybrid_strong_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_gc_future)
  mean_grad_gc_future_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < mean_grad_mmm)
  mean_grad_mmm_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
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


for(i in 1:length(mean_grad_label_list)){
  image.plot(lon_grad,lat_grad,mean_grad_label_list[[i]],
             col=rainbow(4),
             axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
             lab.breaks = names(colorBreaks))
  mtext(text_list[[i]],
        side = 4)
  map("world2",add=T)
}

colorBreaks = 10^(seq(-15, -5, by = 1))


mean_grad_gc_present_label_pr <- mean_grad_gc_present_pr
mean_grad_gc_hybrid_label_pr <- mean_grad_gc_hybrid_pr
mean_grad_gc_hybrid_light_label_pr <- mean_grad_gc_hybrid_light_pr
mean_grad_gc_hybrid_strong_label_pr <- mean_grad_gc_hybrid_strong_pr
mean_grad_gc_future_label_pr <- mean_grad_gc_future_pr
mean_grad_mmm_label_pr <- mean_grad_mmm_pr
mean_grad_min_bias_label_pr <- mean_grad_min_bias_pr

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_gc_present_pr)
  mean_grad_gc_present_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_gc_hybrid_pr)
  mean_grad_gc_hybrid_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_gc_hybrid_light_pr)
  mean_grad_gc_hybrid_light_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_gc_hybrid_strong_pr)
  mean_grad_gc_hybrid_strong_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_gc_future_pr)
  mean_grad_gc_future_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_mmm_pr)
  mean_grad_mmm_label_pr[islabel_pr] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel_pr <- which(colorBreaks[i] < mean_grad_min_bias_pr)
  mean_grad_min_bias_label_pr[islabel_pr] = i
}


mean_grad_label_list_pr <- list(mean_grad_gc_present_label_pr,
                             mean_grad_gc_hybrid_label_pr,
                             mean_grad_gc_hybrid_light_label_pr,
                             mean_grad_gc_hybrid_strong_label_pr,
                             mean_grad_gc_future_label_pr,
                             mean_grad_mmm_label_pr,
                             mean_grad_min_bias_label_pr)


for(i in 1:length(mean_grad_label_list)){
  image.plot(lon_grad,lat_grad,mean_grad_label_list_pr[[i]],
             col=rainbow(6),
             axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
             lab.breaks = names(colorBreaks))
  mtext(text_list[[i]],
        side = 4)
  map("world2",add=T)
}

data_runif <- smooth_runif <- vector("list",length(data_smooth_list_present_runif))
  
for(i in 1:length(data_smooth_list_present_runif)){
  data_runif[[i]] <- data_smooth_list_present_runif[[i]]$`Data cost`
  smooth_runif[[i]] <- data_smooth_list_present_runif[[i]]$`Smooth cost`
}

plot(1:100,unlist(data_runif))


mae_gc_present_0 <- mae_gc_present_best_model <- vector("list",length(model_names))

for(i in 1:length(mae_gc_present_0)){
  mae_gc_present_0[[i]] <- mean(abs(label_attribution_bias_present[,,i]))
  mae_gc_present_best_model[[i]] <- mean(abs(bias_var_result[[i]]$Bias))
}

mae_list_test <- list(mae_gc_present_0,mae_gc_present_best_model)



op <- par(no.readonly = T)

par(mar=c(6,4,2,2) + 0.1)

plot(1,mae_gc_present_0[[1]],  ### Plot the mean absolute bias for each reference with each test
     xlim=c(1,length(model_names)),
     main="Mean",
     type="p",
     ylim=c(min(unlist(mae_gc_present_best_model)),max(unlist(mae_gc_present_0))),
     xlab ="",
     ylab="MAE (°C)",
     col="red",
     xaxt="n",
     yaxt='n')

axis(1,
     at=c(1:length(model_names)),
     labels = model_names,
     las=3,
     cex.axis=0.75,
     srt=45)
axis(2,
     las=1)

for(i in 2:length(model_names)){
  points(i,mae_gc_present_0[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,mae_gc_present_best_model[[i]],col="blue")
}

colors <- list("red", "blue")

for(i in 1:length(mae_list_test)){
  lines(1:length(model_names),
        unlist(mae_list_test[[i]]),
        xlim=range(unlist(mae_list_test[[i]])),
        ylim=range(unlist(mae_list_test[[i]])),
        lty=2,
        col=colors[[i]])
}

legend("topright",legend=c("GC present with 0", "GC present with best model"),
       col=c("red", "blue"),pch=1,cex=0.8)

par(op)



ref <- gc_present_0 <- gc_present_best_model <- array(0, c(nrow(ref),ncol(ref),length(model_names)))

for(m in 1:length(model_names)){
  ref[,,m] <- get(paste0("tas_",model_names[[m]],"_2100"))
  gc_present_0[,,m] <- label_attribution_tas_present[,,m]
  gc_present_best_model[,,m] <- bias_var_result[[m]]$Var
}

gc_present_0_grad <- gc_present_best_model_grad <- array(0, c(nrow(ref),ncol(ref),length(model_names)))

ref_centered <- ref[2:(nrow(ref)-1),2:(ncol(ref)-1),]
ref_up <- ref[1:(nrow(ref)-2),2:(ncol(ref)-1),]
ref_down <- ref[3:nrow(ref),2:(ncol(ref)-1),]
ref_left <- ref[2:(nrow(ref)-1),1:(ncol(ref)-2),]
ref_right <- ref[2:(nrow(ref)-1),3:ncol(ref),]

gc_present_0_centered <- gc_present_0[2:(nrow(gc_present_0)-1),2:(ncol(gc_present_0)-1),]
gc_present_0_up <- gc_present_0[1:(nrow(gc_present_0)-2),2:(ncol(gc_present_0)-1),]
gc_present_0_down <- gc_present_0[3:nrow(gc_present_0),2:(ncol(gc_present_0)-1),]
gc_present_0_left <- gc_present_0[2:(nrow(gc_present_0)-1),1:(ncol(gc_present_0)-2),]
gc_present_0_right <- gc_present_0[2:(nrow(gc_present_0)-1),3:ncol(gc_present_0),]

gc_present_best_model_centered <- gc_present_best_model[2:(nrow(gc_present_best_model)-1),2:(ncol(gc_present_best_model)-1),]
gc_present_best_model_up <- gc_present_best_model[1:(nrow(gc_present_best_model)-2),2:(ncol(gc_present_best_model)-1),]
gc_present_best_model_down <- gc_present_best_model[3:nrow(gc_present_best_model),2:(ncol(gc_present_best_model)-1),]
gc_present_best_model_left <- gc_present_best_model[2:(nrow(gc_present_best_model)-1),1:(ncol(gc_present_best_model)-2),]
gc_present_best_model_right <- gc_present_best_model[2:(nrow(gc_present_best_model)-1),3:ncol(gc_present_best_model),]


gc_present_0_grad <- gc_present_best_model_grad <- array(0,c(nrow(ref_centered),ncol(ref_centered),length(model_names)))

for(m in 1:length(model_names)){
  gc_present_0_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_present_0_up[,,m]-gc_present_0_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_present_0_down[,,m]-gc_present_0_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_present_0_left[,,m]-gc_present_0_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_present_0_right[,,m]-gc_present_0_centered[,,m]))^2)/4
  gc_present_best_model_grad[,,m] = (((ref_up[,,m]-ref_centered[,,m])-(gc_present_best_model_up[,,m]-gc_present_best_model_centered[,,m]))^2+((ref_down[,,m]-ref_centered[,,m])-(gc_present_best_model_down[,,m]-gc_present_best_model_centered[,,m]))^2+((ref_left[,,m]-ref_centered[,,m])-(gc_present_best_model_left[,,m]-gc_present_best_model_centered[,,m]))^2+((ref_right[,,m]-ref_centered[,,m])-(gc_present_best_model_right[,,m]-gc_present_best_model_centered[,,m]))^2)/4
  }

lon_grad <- 1:nrow(ref_centered)
lat_grad <- (1:ncol(ref_centered))-90

mae_grad_gc_present_0 <- mae_grad_gc_present_best_model <- vector("list",length(model_names))

for(i in 1:length(mae_grad_gc_present_0)){
  mae_grad_gc_present_0[[i]] <- mean(abs(gc_present_0_grad[,,i]))
  mae_grad_gc_present_best_model[[i]] <- mean(abs(gc_present_best_model_grad[,,i]))
}

mae_grad_list <- list(mae_grad_gc_present_0,mae_grad_gc_present_best_model)

par(mar=c(6,4,2,2) + 0.1)


plot(1,mae_grad_gc_present_0[[1]],  ### Plot the mean absolute bias for each reference with each test
     xlim=c(1,length(model_names)),
     main="Mean",
     type="p",
     ylim=c(min(unlist(mae_grad_gc_present_best_model)),max(unlist(mae_grad_gc_present_best_model))),
     xlab ="",
     ylab="MAE (°C)",
     col="red",
     xaxt="n",
     yaxt='n')

axis(1,
     at=c(1:length(model_names)),
     labels = model_names,
     las=3,
     cex.axis=0.75,
     srt=45)
axis(2,
     las=1)

for(i in 2:length(model_names)){
  points(i,mae_grad_gc_present_0[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,mae_grad_gc_present_best_model[[i]],col="blue")
}

colors <- list("red", "blue")

for(i in 1:length(mae_grad_list)){
  lines(1:length(model_names),
        unlist(mae_grad_list[[i]]),
        xlim=range(unlist(mae_grad_list[[i]])),
        ylim=range(unlist(mae_grad_list[[i]])),
        lty=2,
        col=colors[[i]])
}


legend("topright",legend=c("GC present with 0", "GC present with best model"),
       col=c("red", "blue"),pch=1,cex=0.8)

par(op)


tests <- list(1,4,9,19)


colorBreaks = 10^(seq(-8, 1, by = 1))

image.plot(lon_grad,lat_grad,min_bias_grad[,,1],
           breaks=colorBreaks,
           col=rainbow(length(colorBreaks)-1L),
           zlim=c(min(colorBreaks),max(colorBreaks)),
           axis.args=list( at=1:length(colorBreaks), labels=colorBreaks),
           lab.breaks = names(colorBreaks))



grad_gc_present_label <- gc_present_grad
grad_gc_hybrid_label <- gc_hybrid_grad

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < gc_present_grad)
  grad_gc_present_label[islabel] = i
}

for(i in 1:length(colorBreaks)-1){
  islabel <- which(colorBreaks[i] < gc_hybrid_grad)
  grad_gc_hybrid_label[islabel] = i
}


mean_grad_label_list <- list(mean_grad_gc_present_label,
                             mean_grad_gc_hybrid_label,
                             mean_grad_gc_hybrid_light_label,
                             mean_grad_gc_hybrid_strong_label,
                             mean_grad_gc_future_label,
                             mean_grad_mmm_label,
                             mean_grad_min_bias_label)


for(i in 1:length(mean_grad_label_list)){
  image.plot(lon_grad,lat_grad,mean_grad_label_list[[i]],
             col=rainbow(4),
             axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
             lab.breaks = names(colorBreaks))
  mtext(text_list[[i]],
        side = 4)
  map("world2",add=T)
}

t=1

for(i in tests){
  image.plot(lon,lat,label_attribution_present[,,i],main=paste0("Label attrib present with ref",tests[[t]]))
  map("world2",add=T)
  image.plot(lon,lat,label_attribution_hybrid[,,i],main=paste0("Label attrib hybrid with ref",tests[[t]]))
  map("world2",add=T)
  image.plot(lon_grad,lat_grad,grad_gc_present_label[,,i],main=paste0("Gradient bias present with ref",tests[[t]]),
             col=rainbow(7),
             axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
             lab.breaks = names(colorBreaks))
  map("world2",add=T)
  image.plot(lon_grad,lat_grad,grad_gc_hybrid_label[,,i],main=paste0("Gradient bias hybrid with ref",tests[[t]]),
             col=rainbow(7),
             axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
             lab.breaks = names(colorBreaks))
  map("world2",add=T)
  t=t+1
}


image.plot(lon_grad,lat_grad,grad_gc_present_label[,,19],main=paste0("Gradient bias present with ref19"),
           col=rainbow(8),
           axis.args=list( at=1:length(colorBreaks)-1, labels=colorBreaks),
           lab.breaks = names(colorBreaks))
map("world2",add=T)

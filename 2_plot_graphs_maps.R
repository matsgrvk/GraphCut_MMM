list.of.packages <- c("fields")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

var="tas"

source("data_plot_graph_map.R")

model_names <- TAS_MODELS[c(1,3,4,5,7,8,11,12,13,16,17,20,22,24,26,30,31,33,34,37),2] #### Names of models used in the ensemble

##### Graphs GC/MMM/Min bias #####

op <- par(no.readonly = T)

par(mar=c(6,4,2,2) + 0.1)

plot(1,mae_gc_present[[1]],  ### Plot the mean absolute bias for each reference with each test
     xlim=c(1,length(model_names)),
     main="Mean",
     type="p",
     ylim=c(min(unlist(mae_gc_future)),max(unlist(mae_mmm))),
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
  points(i,mae_gc_present[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,mae_gc_hybrid[[i]],col="darkolivegreen1")
  points(i,mae_gc_hybrid_light[[i]],col="green")
  points(i,mae_gc_hybrid_strong[[i]],col="darkgreen")
  points(i,mae_gc_future[[i]],col="blue")
  points(i,mae_mmm[[i]],col="black")
  points(i,mae_min_bias[[i]],col="purple")
}

colors <- list("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple")

for(i in 1:length(mae_list)){
  lines(1:length(model_names),
        unlist(mae_list[[i]]),
        xlim=range(unlist(mae_list[[i]])),
        ylim=range(unlist(mae_list[[i]])),
        lty=2,
        col=colors[[i]])
}

legend("topright",legend=c("GC present", "GC hybrid", "GC hybrid light", "GC hybrid strong", "GC future", "MMM", "Min bias"),
       col=c("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple"),pch=1,cex=0.8)

par(op)



par(mar=c(6,4,2,2) + 0.1)

plot(1,median_mae_gc_present[[1]],  ### Plot the mean absolute bias for each reference with each test
     xlim=c(1,length(model_names)),
     main="Median",
     type="p",
     ylim=c(min(unlist(median_mae_gc_future)),max(unlist(median_mae_mmm))),
     xlab ="",
     ylab="median_mae (°C)",
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
  points(i,median_mae_gc_present[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,median_mae_gc_hybrid[[i]],col="darkolivegreen1")
  points(i,median_mae_gc_hybrid_light[[i]],col="green")
  points(i,median_mae_gc_hybrid_strong[[i]],col="darkgreen")
  points(i,median_mae_gc_future[[i]],col="blue")
  points(i,median_mae_mmm[[i]],col="black")
  points(i,median_mae_min_bias[[i]],col="purple")
}

colors <- list("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple")

for(i in 1:length(median_mae_list)){
  lines(1:length(model_names),
        unlist(median_mae_list[[i]]),
        xlim=range(unlist(median_mae_list[[i]])),
        ylim=range(unlist(median_mae_list[[i]])),
        lty=2,
        col=colors[[i]])
}

legend("topright",legend=c("GC present", "GC hybrid", "GC hybrid light", "GC hybrid strong", "GC future", "MMM", "Min bias"),
       col=c("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple"),pch=1,cex=0.8)

par(op)


par(mar=c(6,4,2,2) + 0.1)

plot(1,mae_grad_gc_present[[1]], ### Plot the gradient bias for each reference and for each test
     xlim=c(1,length(model_names)),
     main="Mean",
     type="p",
     ylim=c(min(unlist(mae_grad_gc_future)),max(unlist(mae_grad_min_bias))),
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
  points(i,mae_grad_gc_present[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,mae_grad_gc_hybrid[[i]],col="darkolivegreen1")
  points(i,mae_grad_gc_hybrid_light[[i]],col="green")
  points(i,mae_grad_gc_hybrid_strong[[i]],col="darkgreen")
  points(i,mae_grad_gc_future[[i]],col="blue")
  points(i,mae_grad_mmm[[i]],col="black")
  points(i,mae_grad_min_bias[[i]],col="purple")
}

for(i in 1:length(mae_grad_list)){
  lines(1:length(model_names),
        unlist(mae_grad_list[[i]]),
        xlim=range(unlist(mae_grad_list[[i]])),
        ylim=range(unlist(mae_grad_list[[i]])),
        lty=2,
        col=colors[[i]])
}

legend("topright",legend=c("GC present", "GC hybrid", "GC hybrid light", "GC hybrid strong", "GC future", "MMM", "Min bias"),
       col=c("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple"),pch=1,cex=0.8)

par(op)


par(mar=c(6,4,2,2) + 0.1)

plot(1,median_mae_grad_gc_present[[1]], ### Plot the gradient bias for each reference and for each test
     xlim=c(1,length(model_names)),
     main="Median",
     type="p",
     ylim=c(min(unlist(median_mae_grad_gc_future)),max(unlist(median_mae_grad_min_bias))),
     xlab ="",
     ylab="median_mae (°C)",
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
  points(i,median_mae_grad_gc_present[[i]],col="red")
}

for(i in 1:length(model_names)){
  points(i,median_mae_grad_gc_hybrid[[i]],col="darkolivegreen1")
  points(i,median_mae_grad_gc_hybrid_light[[i]],col="green")
  points(i,median_mae_grad_gc_hybrid_strong[[i]],col="darkgreen")
  points(i,median_mae_grad_gc_future[[i]],col="blue")
  points(i,median_mae_grad_mmm[[i]],col="black")
  points(i,median_mae_grad_min_bias[[i]],col="purple")
}

for(i in 1:length(median_mae_grad_list)){
  lines(1:length(model_names),
        unlist(median_mae_grad_list[[i]]),
        xlim=range(unlist(median_mae_grad_list[[i]])),
        ylim=range(unlist(median_mae_grad_list[[i]])),
        lty=2,
        col=colors[[i]])
}

legend("topright",legend=c("GC present", "GC hybrid", "GC hybrid light", "GC hybrid strong", "GC future", "MMM", "Min bias"),
       col=c("red", "darkolivegreen1", "green", "darkgreen", "blue", "black", "purple"),pch=1,cex=0.8)

par(op)

##### Maps #####


for(i in 1:length(mean_grad_label_list)){ ### Plot the gradient bias mean for each test
  image.plot(lon_grad,lat_grad,mean_grad_label_list[[i]],
             xlab='',
             xaxt='n',
             ylab='',
             yaxt='n',
             col=rainbow(5),
             breaks= seq(0.5, 5.5, 1),
             axis.args=list( at=1:length(colorBreaks), labels=paste0("> ",colorBreaks)))
             # lab.breaks = names(colorBreaks))
  mtext(text_list[[i]],
        side = 4)
  map("world2",add=T)
}

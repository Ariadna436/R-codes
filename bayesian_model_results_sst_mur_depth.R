#### Clean everything: ####
rm(list=ls())  # clear all;
graphics.off() # close all;
gc()           # clear cache.. (?)
# Load model results:
load("model_s_atenuatta_colima_0075deg_sst_mur_depth_2do.RData")


require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics

# Densities:

# x11()
# dens<-densityplot(mcmc_list,nrow=6,ncol=3,legend.title=NULL)
# image<-print(dens, nrow=6, ncol=3)
# tiff(filename="posteriores_sst_mur_depth_2do.tiff",
#      width=16.9,height=22.5,
#      units="cm",
#      bg="white",
#      res=300,
#      compression = c("lzw"))
# print(image)
# dev.off()

### Bayesian model results:





# # Chains hystories:
# traceplot(mcmc_list)
# 
# # Autocorrelation plot:
# autocorr.plot(mcmc_list)
# acfplot(mcmc_list)
# # 
# # # Gelman plot:
# gelman.plot(mcmc_list)
# gelman.diag(mcmc_list)
# # 
# #Posterior correlations:
# chains_res <- ggs(mcmc_list)
# ggs_pairs(chains_res,
#           lower = list(continuous = "density"))



# Plot predictions:

# Read cell data:
cell_data <- read.csv("cell_data_stenella_attenuata_0075deg.csv")


# ##Filter by area: Only within the Gulf of California:
# # Read KML file (polygon):
# require(maptools)
# polygon_gulf <- data.frame(getKMLcoordinates("~/My_R/data_r/polygons/polyg_gulf_ca_very_rough.kml",ignoreAltitude=TRUE))
# 
# # Index of data within polygon:
# require(SDMTools)
# i2  <- pnt.in.poly(cbind(cell_data$lon.cell,
#                          cell_data$lat.cell),
#                    polygon_gulf)
# cell_data <- cell_data[i2$pip == 1,]

# Scale predictors:
sst<- (cell_data$sst_mur-mean(cell_data$sst_mur,na.rm=T))/100          
depth<-(cell_data$depth-mean(cell_data$depth,na.rm=T))/1000
# Dummy vectors of predictors:
sst <- seq(from=min(sst,na.rm=T),
           to=max(sst,na.rm=T),
           length.out=500)
depth <- seq(from=min(depth,na.rm=T),
           to=max(depth,na.rm=T),
           length.out=500)

# # Meshgrid of 100x100 matrix:

 depth_mtx <- c(matrix(depth,
                  ncol=length(depth),
                   nrow=length(depth)))
 sst_mtx <- c(matrix(sst,
                     ncol=length(sst),
                     nrow=length(sst),
                     byrow = T))



prediction_d_frame <- data.frame(
  dens_median = exp(model_summary$summary["w0","50%"]+
                      (model_summary$summary["w1","50%"]*sst_mtx)+
                      (model_summary$summary["w2","50%"]*depth_mtx)+
                      (model_summary$summary["w3","50%"]*sst_mtx^2)+
                      (model_summary$summary["w4","50%"]*depth_mtx^2)),
  
  dens_lwr = exp(model_summary$summary["w0","2.5%"]+
                   (model_summary$summary["w1","2.5%"]*sst_mtx)+
                   (model_summary$summary["w2","2.5%"]*depth_mtx)+
                   (model_summary$summary["w3","2.5%"]*sst_mtx^2)+
                   (model_summary$summary["w4","2.5%"]*depth_mtx^2)),
  
  dens_upr = exp(model_summary$summary["w0","97.5%"]+
                   (model_summary$summary["w1","97.5%"]*sst_mtx)+
                   (model_summary$summary["w2","97.5%"]*depth_mtx)+
                   (model_summary$summary["w3","97.5%"]*sst_mtx^2)+
                   (model_summary$summary["w4","97.5%"]*depth_mtx^2)),
  
  dens_50_upr = exp(model_summary$summary["w0","97.5%"]+
                      (model_summary$summary["w1","75%"]*sst_mtx)+
                      (model_summary$summary["w2","75%"]*depth_mtx)+
                      (model_summary$summary["w3","75%"]*sst_mtx^2)+
                      (model_summary$summary["w4","75%"]*depth_mtx^2)),
  
  dens_50_lwr = exp(model_summary$summary["w0","25%"]+
                      (model_summary$summary["w1","25%"]*sst_mtx)+
                      (model_summary$summary["w2","25%"]*depth_mtx)+
                      (model_summary$summary["w3","25%"]*sst_mtx^2)+
                      (model_summary$summary["w4","25%"]*depth_mtx^2)),
  
  sst_rescaled = (sst_mtx*100)+mean(cell_data$sst_mur,na.rm=T),
  depth_rescaled = (depth_mtx*1000)+mean(cell_data$depth,na.rm=T))


cut_dens<-quantile(prediction_d_frame$dens_median, probs = 0.975, na.rm = FALSE,
         names = TRUE, type = 7)
dens_mtx<- prediction_d_frame$dens_median
dens_mtx[prediction_d_frame$dens_median>(cut_dens)]<-round(cut_dens,dig=1)
# Filter cells wher there were group counts of the species:
cells_with_groups <- cell_data[cell_data$n.groups.cell > 0,]





# Plot prediction:
# require(ggplot2)
# plot_pred_from_sst <-
#   ggplot()+
#   geom_ribbon(data=prediction_d_frame,
#               aes(x=sst_rescaled,
#                   ymin=dens_lwr,
#                   ymax=dens_upr,
#                   fill="red",
#                   alpha=0.5))+
#   geom_ribbon(data=prediction_d_frame,
#               aes(x=sst_rescaled,
#                   ymin=dens_50_lwr,
#                   ymax=dens_50_upr,
#                   fill="red",
#                   alpha=0.5))+
#   geom_path(data=prediction_d_frame,
#             aes(x=sst_rescaled,
#                 y=dens_median),
#             color="red",
#             size=1)+
#   geom_rug(data=cells_with_groups,
#            aes(x=sst_mur))+
#   scale_x_continuous(limits=c(min(prediction_d_frame$sst_rescaled),
#                               max(prediction_d_frame$sst_rescaled)),
#                      expand=c(0,0))+
#   scale_y_continuous(limits=c(min(prediction_d_frame$dens_lwr),
#                               max(prediction_d_frame$dens_upr)),
#                      expand=c(0.11,0))+
#   labs(x=expression("Sea Surface Temperature (°C)"),
#        y=expression("Predicted population density (ind.km^-2)"))+
#   theme_bw()+
#   theme(legend.position="none")

  # tiff(filename="dens_curve_sst.tiff",
#      width=8.1,height=8.1,
#      units="cm",
#      bg="white",
#      res=300,
#      compression = c("lzw"))
# print(plot_pred_from_sst)
# dev.off()
#  
# 
# # 
 
# 
# Histogram to explore the variable:
pred_df <- data.frame(SST=(sst_mtx*100)+mean(cell_data$sst_mur,na.rm=T),
                      Depth=(depth_mtx*1000)+mean(cell_data$depth,na.rm=T),
                      Density=dens_mtx*100)

hist(pred_df$Density,100)
# 
# Ranges:
require(reshape)
require(plyr)

quant_dens <- seq(0,
                  max(pred_df$Density,na.rm=T),
                  round_any(max(pred_df$Density,na.rm=T)/9,
                            20)
)

abline(v=quant_dens,col="red")

# round values higher than the desired resolution:
ind_high <- which(pred_df$Density > max(quant_dens))
pred_df$Density[ind_high] <- max(quant_dens)

# Convert so ColorBrewer understands:
pred_df$quant_dens <- cut(pred_df$Density,
                               breaks=quant_dens,
                               include.lowest=TRUE,
                               right=FALSE)

# Labels:
labels_dens <- paste(quant_dens[-length(quant_dens)],
                     quant_dens[-1],
                     sep="-")


pred_df$quant_dens[which.min(pred_df$Density)]<- "[0,20)"

### The observations:
dframe_obs_detect <- cell_data[cell_data$n.groups.cell > 0,]
dframe_obs_detect$enc_rate_empirical <- dframe_obs_detect$n.groups.cell/dframe_obs_detect$eff.cell

dframe_obs_detect<- dframe_obs_detect[!is.na(dframe_obs_detect$depth),]
dframe_obs_detect<- dframe_obs_detect[!is.na(dframe_obs_detect$sst_mur),]

# 
# 
# Color palettes:
require(RColorBrewer) # Color palletes
# For map units:
require(grid)
# For function alpha in label backgrounds:
require(scales)
# For north symbol and scale:
require(ggsn)
# 
# 
# ### FOR JUST ONE PREDICTOR:
# 
 require(GGally)
     
##### FOR TWO PREDICTORS:
# Map object:
require(ggplot2)
# Map object:
prediction <-
  ggplot()+
  # Heat map:
  geom_tile(data=pred_df,
            aes(x=SST,
                y=Depth,
                fill=quant_dens),
            alpha=1)+
  geom_point (data=dframe_obs_detect,
              aes(x=sst_mur,
                  y=depth),
                  #size= enc_rate_empirical*10),
              color="black",
              alpha=0.2,
              size= 1.5)+
  
  
 
  scale_fill_brewer(palette="YlOrRd",
                    na.value=NA,
                    labels=labels_dens,
                    position= "bottom",
                    guide=guide_legend(title=expression(paste("Predicted population density"~(ind.~100~km^-2))),
                                       title.position="top",
                                       title.hjust=0.5,
                                       label.position="bottom",
                                       label.hjust=0.5,
                                       label.vjust=0.5,
                                       keywidth=1.5,
                                       keyheight=0.4,
                                       nrow=1))+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Depth (m)",x="Sea Surface Temperature (°C)")+
  
  

  
  #     # Makeup:
  theme_bw()+
  theme(
    legend.position=c(0.45,-0.3) , # c(left,bottom)
    legend.direction="horizontal",
    
    legend.box="vertical",
    legend.box.just="right",
    legend.title=element_text(size=8,
                              vjust=0),
    legend.text=element_text(size=8.5),
    
    legend.background=element_rect(fill=alpha('white',0)),
    
    axis.text=element_text(size=8,
                           colour="black"),
    
    axis.ticks.length=unit(0.09,
                           units="cm"),
    panel.border=element_rect(colour="black",
                              fill=NA,
                              size=0.4),
    panel.grid=element_line(colour="black"),
    panel.grid.minor=element_line(size=0.2),
    panel.grid.major=element_line(size=0.25),
    
    plot.margin=unit(c(0.5,0.1, 15, -0.01),units="cm") #(top, right, bottom, and left distance from the device borders))
  ) 


tiff(filename="density_sst_mur_depth_2do.tiff",
     width=13,height=22,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(prediction)
dev.off()

rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?)


# # Libraries:
require(R.utils) # To uncompress the gz files
# require(RNetCDF) # Read .nc files
require(plyr) # round coordinates at a desired resolution
require(stringr) # for manipulating strings
require(ncdf4)
# 

#### sst ###


# Open .nc:
nc_base <- nc_open("C:/Users/Ariadna/Documents/variables/batimetria_pcm.nc")

lon_z <- ncvar_get(nc_base,
                   varid = "x")

lat_z <- ncvar_get(nc_base,
                   varid = "y")

# Take out cells outside the species' range:
# Read KML file (polygon): ### this can be improved with a procedure similar to the one of the islads below...
require(maptools) # To read and process .kml polygons
polyg_col<- data.frame(getKMLcoordinates("~/My_R/polygon_colima.kml",ignoreAltitude=TRUE))

index_in_lon_z <- sort(which(lon_z >= min(polyg_col$X1) & lon_z <= max(polyg_col$X1)))
index_in_lat_z <- sort(which(lat_z >= min(polyg_col$X2) & lat_z <= max(polyg_col$X2)))

z <- ncvar_get(nc_base,
                  varid = "z",
                 start=c(index_in_lon_z[1],
                         index_in_lat_z[1]),
                 count=c(length(index_in_lon_z),
                         length(index_in_lat_z)))


# Close the .nc dataset:
nc_close(nc_base)

# Re-scale variable:
lonz_rs <- round_any(lon_z,0.25)
latz_rs <- round_any(lat_z,0.25)

lon_res<-c(lon_Z)

















lon_mtx <- c(matrix(lon_z,
                    ncol=length(lat_z),
                    nrow=length(lon_z),
                    byrow = T))
lat_mtx <- c(matrix(lat_z,
                    ncol=length(lat_z),
                    nrow=length(lon_z),
                    byrow = T))


pred_df <- data.frame(lon_mtx,lat_mtx,z)

# 
# # pred_df <- data.frame(SST=(sst_mtx*100)+mean(cell_data$sst.cells,na.rm=T),
# #                       Density=dens_mtx*1000)
# 
# 
# 
# # Histogram to explore the variable:
hist(pred_df$Density,100)
# 
# # Ranges:
require(reshape)
quant_dens <- seq(0,
                  max(pred_df$Density,na.rm=T),
                  round_any(max(pred_df$Density,na.rm=T)/8,
                            0.1)
)

abline(v=quant_dens,col="red")

# # round values higher than the desired resolution:
ind_high <- which(pred_df$Density > max(quant_dens))
pred_df$Density[ind_high] <- max(quant_dens)

# # Convert so ColorBrewer understands:
pred_df$quant_dens <- cut(pred_df$Density,
                          breaks=quant_dens,
                          include.lowest=TRUE,
                          right=FALSE)

# # Labels:
labels_dens <- paste(quant_dens[-length(quant_dens)],
                     quant_dens[-1],
                     sep="-")

# 
# 
# 
# ### The observations:
dframe_obs_detect <- cell_data[cell_data$n.groups.cell > 0,]
dframe_obs_detect$enc_rate_empirical <- dframe_obs_detect$n.groups.cell/dframe_obs_detect$eff.cell
# 
# 
# 
# 
# 
# # Color palettes:
require(RColorBrewer) # Color palletes
# # For map units:
require(grid)
# # For function alpha in label backgrounds:
require(scales)
# # For north symbol and scale:
require(ggsn)
# 
# 
# ### FOR JUST ONE PREDICTOR:
# 
# require(GGally)
# # pred_sst <- 
# ggplot()+
#   # geom_area(data=dens_pred_df,
#   #           aes(x=sst_pred,
#   #               y=dens_pred_upr),
#   #           linetype="dashed")+
#   geom_line(data=dens_pred_df,
#             aes(x=adt_pred,
#                 y=dens_pred_upr),
#             linetype="dashed")+
#   geom_line(data=dens_pred_df,
#             aes(x=adt_pred,
#                 y=dens_pred_lwr),
#             linetype="dashed")+
#   geom_line(data=dens_pred_df,
#             aes(x=adt_pred,
#                 y=dens_pred_med))
#   
#     
# ##### FOR TWO PREDICTORS:
# # Map object:
prediction <-
  ggplot()+
  #   # Heat map:
  geom_tile(data=pred_df,
            aes(x=SLA,
                y=Depth,
                fill=quant_dens),
            alpha=1)+
  scale_fill_brewer(palette="YlOrRd",
                    na.value=NA,
                    labels=labels_dens)+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(y="Depth (m)",x="SLA (cm)")+
  
  #     # Add some observations:
  geom_point(data=dframe_obs_detect,
             aes(x=sla_obs,
                 y=depth_obs,
                 size=enc_rate_empirical*10),
             color="black",
             alpha=0.2)+
  scale_size_continuous(guide=guide_legend(direction="horizontal",
                                           title.position="top",
                                           label.position="bottom",
                                           override.aes=list(size=c(2,4,6,8))))+
  
  #     # Makeup:
  theme_bw()+
  theme(aspect.ratio=1)
#     
# 
# 

plot(prediction)

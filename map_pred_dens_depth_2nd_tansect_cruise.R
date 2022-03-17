#### Clean everything: ####
rm(list=ls())  # clear all;
graphics.off() # close all;
gc()           # clear cache.. (?)


##### IMPORT AND FILTER DATA ######

# Call data for effort, sightings, and environmental variables:
transect_data_s_attenuata <- read.csv("~/My_R/transect_data_u_colima_5km.csv")
transect_data_s_attenuata$cruise<-transect_data_s_attenuata$interp_surv_n
# Call data for effort, sightings, and environmental variables:
eff_data <- read.csv("effort_u_colima_2010-2014_transect.csv")

unique_cruise<-unique(transect_data_s_attenuata$cruise)
min_d<-c(0, nrow(unique_cruise))
max_d<-c(0, nrow(unique_cruise))
cruise_df<-data.frame(cruise=unique_cruise, min_d, max_d)

for(i in seq(1,nrow(cruise_df))){
  index<-which(transect_data_s_attenuata$cruise == unique_cruise[i])
  cruise_df$min_d[i]<-min(transect_data_s_attenuata$interp_time[index]-8)
  cruise_df$max_d[i]<-max(transect_data_s_attenuata$interp_time[index])
}


####Open polygon for Colima####

# Read KML file (polygon): ### this can be improved with a procedure similar to the one of the islads below...
require(maptools) # To read and process .kml polygons
polyg_col_sa <- data.frame(getKMLcoordinates("polygon_colima.kml",ignoreAltitude=TRUE))
lat_min_pol<-min(polyg_col_sa$X2)
lat_max_pol<-max(polyg_col_sa$X2)
lon_min_pol<-min(polyg_col_sa$X1)
lon_max_pol<-max(polyg_col_sa$X1)
time_min<-min(cruise_df$min_d)
time_max<-max(cruise_df$max_d)


##### define lon and lat vec####
require(ncdf4)
#Archivo .nc
nc_base <- nc_open("variables/topography_etopo_001d.nc")
lon_depth <- ncvar_get(nc_base,'longitude')
lat_depth <- ncvar_get(nc_base,'latitude')

#sacar con which el rango entre minimo y maximo para  lon y lat

index_in_lon<-which(lon_depth>=lon_min_pol & lon_depth<=lon_max_pol)
index_in_lat<-which(lat_depth>=lat_min_pol & lat_depth<=lat_max_pol)

lon_depth<-lon_depth[index_in_lon]
lat_depth<-lat_depth[index_in_lat]


# ese index seran las variables index_in_lon_sst
depth <- ncvar_get(nc_base,
                   varid = "altitude",
                   start=c(index_in_lon[1],
                           index_in_lat[1]),
                   count=c(length(index_in_lon),
                           length(index_in_lat)))
depth[depth > 0] <- 0
depth <- depth*-1


library(plyr)

lon_depth_r<-round_any(lon_depth,0.0166667)
lat_depth_r<-round_any(lat_depth,0.0166667)

lon_uniq_depth<-unique(lon_depth_r)
lat_uniq_depth<-unique(lat_depth_r)

depth_mtx <- matrix(depth,
                  nrow = length(lon_uniq_depth),
                  ncol = length(lat_uniq_depth))# 
nc_close(nc_base)


lon_column_depth <- c(
  matrix(data=lon_uniq_depth,
         nrow=length(lon_uniq_depth),
         ncol=length(lat_uniq_depth),
         byrow=FALSE))

lat_column_depth <- c(
  matrix(data=lat_uniq_depth,
         nrow=length(lon_uniq_depth),
         ncol=length(lat_uniq_depth),
         byrow=TRUE))

list_depth<- list()
for (i in seq(1,length(unique_cruise))) {
  ssta_dframe<-data.frame(
    lon=lon_column_depth,
    lat= lat_column_depth,
    depth=c(depth_mtx),
    cruise=rep(unique_cruise[i],length(lon_column_depth)))
list_depth[[i]] <- ssta_dframe
  }


df_all_var_cruise<-do.call(rbind,list_depth) 


####open data cells ####
transect_data_s_attenuata$sigth_cruise<-rep(NA,nrow(transect_data_s_attenuata))
for (i in seq(1,length(unique_cruise))){
  index_sigth<-which(transect_data_s_attenuata$cruise == unique_cruise[i])
  df_sigth<-round(sum(transect_data_s_attenuata$Pan_animals[index_sigth]))
  transect_data_s_attenuata$sigth_cruise[index_sigth]<-rep(df_sigth,length(index_sigth))
  
}

transect_data_s_attenuata$eff_cruise<-rep(NA,nrow(transect_data_s_attenuata))

for (i in seq(1,length(unique_cruise))){
  index_eff<-which(transect_data_s_attenuata$cruise == unique_cruise[i])
  df_eff<-round(sum(transect_data_s_attenuata$dist_vec[index_eff]),digits=0)
  transect_data_s_attenuata$eff_cruise[index_eff]<-rep(df_eff,length(index_eff))
  
}

transect_data_s_attenuata$enc_rate_cruise<-rep(NA,nrow(transect_data_s_attenuata))
transect_data_s_attenuata$enc_rate_cruise<-round(((transect_data_s_attenuata$sigth_cruise)/
                                                (transect_data_s_attenuata$eff_cruise)),digits = 3)


####Take out data outside the colima, data.frame by cruise:####
require(SDMTools)
i3  <- pnt.in.poly(cbind(df_all_var_cruise$lon,
                         df_all_var_cruise$lat),
                   polyg_col_sa)
df_all_var_cruise <- df_all_var_cruise[i3$pip == 1,]

gc()
# 

####Open model result####
# Load model results:
load("model_s_atenuatta_colima_depth_2do_transect.RData")

require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics


### Take out data outside the colima, data.frame data by cell####
require(SDMTools)
i3  <- pnt.in.poly(cbind(transect_data_s_attenuata$lon,
                         transect_data_s_attenuata$lat),
                   polyg_col_sa)
transect_data_s_attenuata <- transect_data_s_attenuata[i3$pip == 1,]

gc()

#### COAST LINES AND BORDERS from GSHHS database: ####
# BINARY files obtained from: 
# http://www.soest.hawaii.edu/pwessel/gshhg/
# The ZIP file HAS to be extracted into one of R's
# packages. In this case, just for convenience, into 
# the ggplot2's folder (i.e. Documents/R/win-library/2.15/ggplot2)
require(ggplot2) # ploting

# Set path:
land_path <- system.file("gshhs_f.b",package="ggplot2") # Low resolution doesn't work (??)
bound_path <- system.file("wdb_borders_f.b",package="ggplot2")

# Extract from world database:
#### Coord limits for maps: ####
lim_lon <- c(round(min(polyg_col_sa$X1-0.1),digits = 1),
             ceiling(max(polyg_col_sa$X1)))
lim_lat <- c(round(min(polyg_col_sa$X2),digits = 1),
             round(max(polyg_col_sa$X2),digits = 1))




# Limits:
x_limits <- c(360+lim_lon[1],360+(lim_lon[2]))
y_limits <- c(lim_lat[1],lim_lat[2])#revisar si es necesario el 0.7 

# Land polygon (coast line):

library(rgeos)
library(rgdal)
library(ggplot2)
require(maptools)

land_polyg <- getRgshhsMap(land_path,
                           xlim=x_limits,
                           ylim=y_limits,
                           level = 1)

# Boundaries:
bound_polyg <- Rgshhs(bound_path,
                      xlim=NULL,
                      ylim=NULL,
                      level=2)
bound_polyg <- SpatialLines2PolySet(bound_polyg$SP) # Convert to polygon

bound_polyg <- bound_polyg[bound_polyg$X > x_limits[1] &
                             bound_polyg$X < x_limits[2],] # Filter data by limits
bound_polyg <- bound_polyg[bound_polyg$Y > y_limits[1] &
                             bound_polyg$Y < y_limits[2],] # Filter data by limits


# Fortify to convert into dataframe
land_polyg <- fortify(land_polyg)
bound_polyg <- fortify(bound_polyg)


##
####By cruise density ####
require(plyr)
library(plyr)
# Scale predictors:


depth_dens_resc_c<-(df_all_var_cruise$depth - mean(transect_data_s_attenuata$depth,na.rm=T))/1000
 


df_all_var_cruise$med_pred_dens_c  <- exp(model_summary$summary["s0","50%"]+
                                            (model_summary$summary["s1","50%"] * (depth_dens_resc_c))+
                                            (model_summary$summary["s2","50%"] * (depth_dens_resc_c^2)))

df_all_var_cruise$lwr_pred_dens_c <- exp(model_summary$summary["s0","2.5%"]+
                                           (model_summary$summary["s1","2.5%"] * (depth_dens_resc_c))+
                                           (model_summary$summary["s2","2.5%"] * (depth_dens_resc_c^2)))

df_all_var_cruise$upr_pred_dens_c <- exp(model_summary$summary["s0","97.5%"]+
                                           (model_summary$summary["s1","97.5%"] * (depth_dens_resc_c))+
                                           (model_summary$summary["s2","97.5%"] * (depth_dens_resc_c^2)))

df_all_var_cruise$upr_50_pred_dens_c<- exp(model_summary$summary["s0","75%"]+
                                             (model_summary$summary["s1","75%"] * (depth_dens_resc_c))+
                                             (model_summary$summary["s2","75%"] * (depth_dens_resc_c^2)))

df_all_var_cruise$lwr_50_pred_dens_c <- exp(model_summary$summary["s0","25%"]+
                                              (model_summary$summary["s1","25%"] * (depth_dens_resc_c))+
                                              (model_summary$summary["s2","25%"] * (depth_dens_resc_c^2)))





#
# #
df_all_var_cruise$med_pred_dens_c_100<- df_all_var_cruise$med_pred_dens_c *100

#
# # Histogram to explore the variable:
hist(df_all_var_cruise$med_pred_dens_c_100,100)
#

# # Ranges:
library(dplyr)
                             

quant_dens_c <- quantile(df_all_var_cruise$med_pred_dens_c_100,
                      probs = c(0.2, 0.4, 0.6, 0.8))
quant_dens_c <- c(min(df_all_var_cruise$med_pred_dens_c_100, na.rm = T),
                  quant_dens_c[1],
                  quant_dens_c[2],
                  quant_dens_c[3],
                  quant_dens_c[4],
                  max(df_all_var_cruise$med_pred_dens_c_100, na.rm = T))



abline(v=quant_dens_c,col="red")


# # Convert so ColorBrewer understands:
df_all_var_cruise$quant_dens_c <- cut(df_all_var_cruise$med_pred_dens_c_100,
                                      breaks = quant_dens_c,
                                      include.lowest = TRUE,
                                      right = FALSE)

quant_dens_c<- round(quant_dens_c,2)
# # Labels:
labels_dens_c <- paste(quant_dens_c[-length(quant_dens_c)],
                       quant_dens_c[-1],
                       sep="-")

#
#
# ### The observations:
dframe_obs_detect<- read.csv("sighting_data_u_colima_w_transect.csv")
dframe_obs_detect$cruise<- dframe_obs_detect$no_survey


#
#
##Effort
eff_col<-read.csv("effort_u_colima_2010-2014_transect.csv")
eff_d_f<-data.frame(lon_e=eff_col$lon,
                    lat_e=eff_col$lat,
                    cruise=eff_col$no_survey,
                    season_or=eff_col$season)
unique_season<- unique(eff_d_f$season)
####labels#### 
label_er<-data.frame(lon_l=c(-107.1),
                     lat_l=c(17.1),
                     lab=paste("ER:",transect_data_s_attenuata$enc_rate_cruise,"~ind.~km^-2",sep=' '),
                     cruise=transect_data_s_attenuata$cruise)


label_eff<-data.frame(lon_l=c(-107.12),
                      lat_l=c(17.4),
                      lab=paste("Effort:",transect_data_s_attenuata$eff_cruise,"km",sep=' '),
                      cruise=transect_data_s_attenuata$cruise)

label_cruise<-data.frame(lon_l=c(-103.5),
                         lat_l=c(20),
                         lab=paste(unique_cruise),
                         cruise=unique_cruise)
label_season<-data.frame(lon_l=c(-104),
                         lat_l=c(19.7),
                         lab=paste(unique_season),
                         cruise=unique_cruise)

require(RColorBrewer) # Color palletes
# For map units:
require(grid)
# For function alpha in label backgrounds:
require(scales)
# For north symbol and scale:
require(ggsn)
# Nuew color palettes viridis:
require(viridis)


####MAP####
prediction_cruise <-
  ggplot()+
  # Map-projection:
  coord_map(projection="mercator")+
  
  
  
  #   # Heat map:
  geom_tile(data=df_all_var_cruise,
            aes(x=lon,
                y=lat,
                fill=quant_dens_c),
            alpha=0.6)+
  
  scale_fill_brewer(palette="YlOrRd",
                    na.value=NA,
                    
                    labels=labels_dens_c,
                    guide=guide_legend(title=expression(paste("Predicted population density"~(ind.~100~km^-2))),
                                       title.position="top",
                                       label.position="bottom",
                                       label.hjust=0.5,
                                       label.vjust=0.5,
                                       keywidth=1,
                                       keyheight=0.4,
                                       nrow=1))+
  
  # # Polygon:
  geom_polygon(data = polyg_col_sa,
               aes(x = X1,
                   y = X2),
               fill="NA",
               color = "black",
               size = 0.3)+
  # Land:
  geom_polygon(data=land_polyg,
               aes(x=long,
                   y=lat,
                   group=group),
               fill="gray95",
               colour="darkgray",
               size=0.5)+
  
  
  # Countries boundaries:
  geom_path(data=bound_polyg,
            aes(x=X-360,
                y=Y,
                group=PID),
            color="darkgray",
            size=0.4,
            alpha=0.4)+
  
  # # 
  # 
  
  #effort lines
  geom_path (data=eff_d_f,
             aes(x=lon_e,
                 y=lat_e),
             colour="#605f63",
             linetype="dashed",
             alpha = 1)+
  # #labels 
  geom_text(data=label_er,
            aes(x=lon_l,
                y=lat_l,
                label=lab,"ind.km^-2"),
            parse=TRUE,
            hjust=0.1,
            vjust=0.1,
            color="black",
            size=2.8)+
  
  
  geom_text(data=label_eff,
            aes(x=lon_l,
                y=lat_l,
                label=lab),
            hjust=0.1,
            vjust=0.1,
            color="black",
            size=2.8)+
  
  geom_label(data=label_cruise,
             aes(x=lon_l,
                 y=lat_l,
                 label=lab),
             hjust=0.1,
             vjust=0.1,
             color="black",
             fill= "white",
             size=3,
             fontface= "bold")+
  
  geom_text(data=label_season,
             aes(x=lon_l,
                 y=lat_l,
                 label=lab),
             hjust=0.1,
             vjust=0.1,
             color="black",
             size=3
             )+
  # Add some observations:
  geom_point(data=dframe_obs_detect,
             aes(x=lon,
                 y=lat
             ),
             shape=21,
             fill="#0aee61",
             color="black",
             alpha=1)+
  #stroke = 1)+
  
  
  # Settings:
  labs(y="",x="")+
  theme_bw()+
  theme(
    legend.position=c(0.5,-0.1), # c(left,bottom)
    legend.direction="horizontal",
    legend.box="vertical",
    legend.box.just="left",
    legend.title=element_text(size=8.5),
    legend.title.align=0.5,
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
    
    plot.margin=unit(c(0.5,0.8, 2, -0.2),units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  
  scale_x_continuous(limits = c(lim_lon[1],
                                (lim_lon[2])),
                     breaks = seq(ceiling(min(lim_lon)+1),
                                  max(lim_lon),
                                  2),
                     labels = paste(as.character(abs(seq(ceiling(min(lim_lon)+1),
                                                         max(lim_lon),
                                                         2))),
                                    "°W",sep = ""),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(lim_lat[1],lim_lat[2]),
                     breaks = seq(ceiling(lim_lat[1]),floor(lim_lat[2]),2),
                     labels = paste(as.character(abs(seq(ceiling(lim_lat[1]),floor(lim_lat[2]),2))),
                                    "°N",sep = ""),
                     expand = c(0,0)) +
  
  coord_fixed()+
  
  
  #     # Makeup:
  facet_wrap(~cruise, nrow=5,ncol=3)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())




#### Layout and file ####
tiff(filename="map_depth_2nd_transect_cruise.tiff",
     width=16.9,height=22.5,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(prediction_cruise)
dev.off()

# #
#
#




#
#### Clean everything: ####
rm(list=ls())  # clear all;
graphics.off() # close all;
gc()           # clear cache.. (?)


##### IMPORT AND FILTER DATA ######

# Call data for effort, sightings, and environmental variables:
cell_data_s_atenuatta <- read.csv("~/My_R/cell_data_stenella_attenuata_0075deg.csv")
eff_data <- read.csv("effort_u_colima_2010-2014.csv")

unique_cruise<-unique(cell_data_s_atenuatta$cruise)
min_d<-c(0, nrow(unique_cruise))
max_d<-c(0, nrow(unique_cruise))
cruise_df<-data.frame(cruise=unique_cruise, min_d, max_d)

for(i in seq(1,nrow(cruise_df))){
  index<-which(cell_data_s_atenuatta$cruise==unique_cruise[i])
  cruise_df$min_d[i]<-min(cell_data_s_atenuatta$time.cell[index]-8)
  cruise_df$max_d[i]<-max(cell_data_s_atenuatta$time.cell[index])
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

####open sst anomaly spatial#### 
require(ncdf4)
nc_base  <- nc_open("~/My_R/variables/SST/sst_cruise/sst_col_01g_1d_01.nc", write=FALSE) # read the file

# Get coordinates:

# extraer lon, lat y tiempo
lon_sst <- (ncvar_get(nc_base,'longitude'))

lat_sst <- ncvar_get(nc_base,'latitude')

#sacar con which el rango entre minimo y maximo para lon y lat
index_in_lon_sst<-which(lon_sst>=lon_min_pol & lon_sst<=lon_max_pol)
index_in_lat_sst<-which(lat_sst>=lat_min_pol & lat_sst<=lat_max_pol)

lon_sst<-lon_sst[index_in_lon_sst]
lat_sst<-lat_sst[index_in_lat_sst]

#Meshgrid for final matrix
lon_column_sst <- c(
  matrix(data=lon_sst,
         nrow=length(lon_sst),
         ncol=length(lat_sst),
         byrow=FALSE))

lat_column_sst <- c(
  matrix(data=lat_sst,
         nrow=length(lon_sst),
         ncol=length(lat_sst),
         byrow=TRUE))

####Open sst .nc####



#Archivo .nc
dir_path_sst<- ("~/My_R/variables/SST/sst_cruise")
nc_files_sst <- list.files(dir_path_sst,recursive=T, pattern=paste("*.nc"))

sst <- rep(NA,nrow(cell_data_s_atenuatta))
nc_base <-list()
list_sst<-list()
list_ssta_day<- list()
for (i in seq(1,length(unique_cruise))) {
  sst_dframe<-data.frame(
    lon=lon_column_sst,
    lat= lat_column_sst,
    cruise=rep(unique_cruise[i],length(lon_column_sst)))
  
  # Search the .nc file with coincidence in date for each cell:
  if (unique_cruise[i] < 10) {
    ind_nc_file <- list.files(dir_path_sst,
                              recursive=T,
                              pattern=paste(0,unique_cruise[i],sep=""))
  } else {
    ind_nc_file <- list.files(dir_path_sst,
                              recursive=T,
                              pattern=paste(unique_cruise[i]))
  }
  
  
  if (length(ind_nc_file) < 1) {
    
  } else {
    # Uncompress .nc file:
    filename <- paste(dir_path_sst,
                      "/",
                      ind_nc_file,
                      sep="")
    
    # Open NetCDF file:
    nc_base  <- nc_open(filename, write=FALSE) # read the file
    # print.nc(nc_base)
    # Get coordinates:
    
    # extraer lon, lat y tiempo
    
    time_sst <- ncvar_get(nc_base,'time')/60/60/24
    
    #sacar con which el rango entre minimo y maximo para tiempo, lon y lat
    
    index_in_time_sst<-which(time_sst>time_min &  time_sst<time_max)
    # 
    time_sst <- time_sst[index_in_time_sst]
    
    duration<- cruise_df$max_d[i] - cruise_df$min_d[i]
    # ese index seran las variables index_in_lon_sst
    sst <- ncvar_get(nc_base,
                     varid = "analysed_sst",
                     
                     start=c(index_in_lon_sst[1],
                             index_in_lat_sst[1],
                             index_in_time_sst[1]
                     ) ,
                     count=c(length(index_in_lon_sst),
                             length(index_in_lat_sst),
                             length(index_in_time_sst)
                     ))
    
    for (k in seq(1,length(duration))) {
      index_array<-duration[k]
      ssta_day<- sst[,,index_array]-mean(sst[,,index_array],na.rm = T)
      list_ssta_day[[k]]<-list(ssta_day)
      
    }
    ssta_cruise<- array(as.numeric(unlist(list_ssta_day)), 
                        dim=c(length(lon_sst),length(lat_sst),length(duration)))
    
    ssta_cruise_day<- apply(ssta_cruise, c(1,2),mean,na.rm=T) 
    
    sst_dframe$ssta_f<-c(ssta_cruise_day)
    
    list_sst[[i]]<-sst_dframe
    # Close the .nc dataset:
    nc_close(nc_base)
  }
  
}

ssta_cruise<-do.call(rbind,list_sst) 


####open cell.data ####
cell_data_s_atenuatta$sigth_cruise<-rep(NA,length(cell_data_s_atenuatta$cruise))
for (i in seq(1,length(unique_cruise))){
  index_sigth<-which(cell_data_s_atenuatta$cruise==unique_cruise[i])
  df_sigth<-(sum(cell_data_s_atenuatta$n.groups.cell[index_sigth]))
  cell_data_s_atenuatta$sigth_cruise[index_sigth]<-rep(df_sigth,length(index_sigth))
  
}

cell_data_s_atenuatta$eff_cruise<-rep(NA,length(cell_data_s_atenuatta$cruise))

for (i in seq(1,length(unique_cruise))){
  index_eff<-which(cell_data_s_atenuatta$cruise==unique_cruise[i])
  df_eff<-round(sum(cell_data_s_atenuatta$eff.cell[index_eff]),digits=0)
  cell_data_s_atenuatta$eff_cruise[index_eff]<-rep(df_eff,length(index_eff))
  
}

cell_data_s_atenuatta$enc_rate_cruise<-rep(NA,length(cell_data_s_atenuatta$cruise))
cell_data_s_atenuatta$enc_rate_cruise<-round((cell_data_s_atenuatta$sigth_cruise/
                                                cell_data_s_atenuatta$eff_cruise),digits = 4)






### Take out data outside the colima, for anomaly spatial####
require(SDMTools)
i3  <- pnt.in.poly(cbind(ssta_cruise$lon,
                         ssta_cruise$lat),
                   polyg_col_sa)
df_all_var_cruise <- ssta_cruise[i3$pip == 1,]

gc()



### Take out data outside the colima, data.frame data by cell####
require(SDMTools)
i3  <- pnt.in.poly(cbind(cell_data_s_atenuatta$lon.cell,
                         cell_data_s_atenuatta$lat.cell),
                   polyg_col_sa)
cell_data <- cell_data_s_atenuatta[i3$pip == 1,]

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

####Open model result####
# Load model results:
load("model_s_atenuatta_colima_0075deg_ssta_spatial_day_2do.RData")


require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics
####By cruise density ####
require(plyr)

# Scale predictors:


ssta_dens_resc_c <- (df_all_var_cruise$ssta_f/10)



df_all_var_cruise$med_pred_dens_c <- exp(model_summary$summary["s0","50%"]+
                                   (model_summary$summary["s1","50%"]*ssta_dens_resc_c)+
                                   (model_summary$summary["s2","50%"]*ssta_dens_resc_c^2))


                                         
# Filter cells wher there were group counts of the species:
cells_with_groups <- cell_data_s_atenuatta[cell_data_s_atenuatta$n.groups.cell > 0,]


#
# #
df_all_var_cruise$med_pred_dens_c_100<- df_all_var_cruise$med_pred_dens_c*100

# # Histogram to explore the variable:
hist(df_all_var_cruise$med_pred_dens_c_100,100)
#

# # Ranges:
library(dplyr)
quant_dens_c <- quantile(df_all_var_cruise$med_pred_dens_c_100,
                         probs = c(0.2, 0.4, 0.6, 0.8),na.rm=T)
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
dframe_obs_detect <- cell_data_s_atenuatta[cell_data_s_atenuatta$n.groups.cell > 0,]
dframe_obs_detect<- dframe_obs_detect[which(!is.na(dframe_obs_detect$mean.gpz.cell)),]
dframe_obs_detect$enc_rate_empirical <- (dframe_obs_detect$n.groups.cell*dframe_obs_detect$mean.gpz.cell)/dframe_obs_detect$eff.cell
dframe_obs_detect$cruise<-dframe_obs_detect$cruise



#
##Effort
eff_col<-read.csv("effort_u_colima_2010-2014.csv")
eff_d_f<-data.frame(lon_e=eff_col$lon.eff,
                    lat_e=eff_col$lat.eff,
                    cruise=eff_col$cruise,
                    season_or=eff_col$season)
unique_season<- unique(cell_data$season)

####labels#### 
####labels#### 
label_er<-data.frame(lon_l=c(-107.1),
                     lat_l=c(17.1),
                     lab=paste("ER:",cell_data_s_atenuatta$enc_rate_cruise,"~ind.~km^-2",sep=' '),
                     cruise=cell_data_s_atenuatta$cruise)


label_eff<-data.frame(lon_l=c(-107.12),
                      lat_l=c(17.4),
                      lab=paste("Effort:",cell_data_s_atenuatta$eff_cruise,"km",sep=' '),
                      cruise=cell_data_s_atenuatta$cruise)
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
               fill="snow2",
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
  geom_path (aes(x=lon_e,
                 y=lat_e),
             data=eff_d_f,
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
             aes(x=lon.cell,
                 y=lat.cell
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
tiff(filename="map_ssta_spa_day_cruise.tiff",
     width=16.9,height=22.5,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(prediction_cruise)
dev.off()

# #
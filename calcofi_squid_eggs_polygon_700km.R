####Clear all ####
rm(list=ls())
graphics.off()
gc()

####Open data base ####
calcofi_zooplank<- read.csv("C:/Users/Macroeco/Documents/CalCOFI/fish_squid_eggs_calcofi_1996-2017.csv")

calcofi_zooplank <-  calcofi_zooplank[order(calcofi_zooplank$start_yr,calcofi_zooplank$start_mo),]

####Remove NA's ####
index_na<- which(!is.na(calcofi_zooplank$longitude) &
                   !is.na(calcofi_zooplank$latitude) &
                   !is.na(calcofi_zooplank$start_yr) &
                   !is.na(calcofi_zooplank$stop_longitude) &
                   !is.na(calcofi_zooplank$stop_latitude))

calcofi_zooplank <- calcofi_zooplank[index_na,]


calcofi_zooplank$j_day <- as.numeric(unclass(ISOdatetime(
  calcofi_zooplank$start_yr,
  calcofi_zooplank$start_mo,
  calcofi_zooplank$start_dy,
  calcofi_zooplank$start_hr, 
  calcofi_zooplank$start_min, 
  calcofi_zooplank$start_sec,
  tz="Greenwich")))/
  (60*60*24)

calcofi_zooplank$j_day_stop <- as.numeric(unclass(ISOdatetime(
  calcofi_zooplank$stop_yr,
  calcofi_zooplank$stop_mo,
  calcofi_zooplank$stop_dy,
  calcofi_zooplank$stop_hr, 
  calcofi_zooplank$stop_min, 
  calcofi_zooplank$stop_sec,
  tz="Greenwich")))/
  (60*60*24)

calcofi_zooplank$total_time_min<- (calcofi_zooplank$j_day_stop - calcofi_zooplank$j_day)*(24*60)
calcofi_zooplank$total_volume_m3<- ((calcofi_zooplank$start_pump_speed+
                                        calcofi_zooplank$stop_pump_speed)/2)*calcofi_zooplank$total_time_min

calcofi_zooplank$squid_egg_dens<- calcofi_zooplank$squid_eggs/calcofi_zooplank$total_volume_m3

# calcofi_zooplank$final_dist<- distance(calcofi_zooplank$latitude,
#                                        calcofi_zooplank$longitude,
#                                        calcofi_zooplank$stop_latitude,
#                                        calcofi_zooplank$stop_longitude)


####Open GFS pup count file to filter in time ####
pup_gfs <- read.csv('pup_count_GFS_sst.csv')

unique_julian <-  unique(pup_gfs$julian_init_70)

####Filter in time####
require(lubridate)
list_jday<- list()
for (i in seq(1,length(unique_julian))) {
  index_time<- which(calcofi_zooplank$j_day >= (julian(as.Date(unique_julian[i], origin= "1970-01-01") - period(5, "months")))
                     & calcofi_zooplank$j_day <= unique_julian[i])
  calcofi_zooplank_temp<- calcofi_zooplank[index_time,]
  calcofi_zooplank_temp$dum_vec<- calcofi_zooplank_temp$longitude * calcofi_zooplank_temp$latitude

  for (k in seq(1,nrow(calcofi_zooplank_temp))) {
    index_duplic<- which(calcofi_zooplank_temp$dum_vec == calcofi_zooplank_temp$dum_vec[k])
    if(length(index_duplic>1)){
      calcofi_zooplank_temp$squid_egg_dens_mean[index_duplic]<- mean(calcofi_zooplank_temp$squid_egg_dens[index_duplic])
    } else {
      calcofi_zooplank_temp$squid_egg_dens_mean[index_duplic]<- calcofi_zooplank_temp$squid_egg_dens
    }

  }
  
  list_jday[[i]] <- calcofi_zooplank_temp
}

calcofi_zooplank_filt <- do.call(rbind,list_jday)


####Open polygon for GFS####

 polygon_gfs <- data.frame(getKMLcoordinates                   
                           ("C:/Users/Macroeco/Documents/My_R/Apt_pop_grow/gpe_polygon_700km.kml",   #File example polygon               
                             ignoreAltitude=TRUE))
 lat_min_pol<-min(polygon_gfs$X2)
 lat_max_pol<-max(polygon_gfs$X2)
 lon_min_pol<-min(polygon_gfs$X1)
 lon_max_pol<-max(polygon_gfs$X1)

 #### Filt base to polygon 700km ####
 require(SDMTools)
 
 i3  <- pnt.in.poly(cbind(calcofi_zooplank_filt$longitude,
                          calcofi_zooplank_filt$latitude),
                    polygon_gfs)
 calcofi_zooplank_filt <- calcofi_zooplank_filt[i3$pip == 1,]
 
 gc()
 
 #### Filt base to California Gulf ####
 polygon_california_gulf <- data.frame(getKMLcoordinates                   
                                       ("C:/Users/Macroeco/Documents/My_R/Apt_pop_grow/california_gulf.kml",   #File example polygon               
                                         ignoreAltitude=TRUE))
 i3  <- pnt.in.poly(cbind(calcofi_zooplank_filt$longitude,
                          calcofi_zooplank_filt$latitude),
                    polygon_california_gulf)
 calcofi_zooplank_filt <- calcofi_zooplank_filt[i3$pip == 0,]
 
 gc()
 
 calcofi_zooplank_filt<- calcofi_zooplank_filt[!is.na(calcofi_zooplank_filt$squid_egg_dens),] 
 
 
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
lim_lon <- c(round(lon_min_pol,digits = 1),
             ceiling(lon_max_pol))
lim_lat <- c(round(lat_min_pol,digits = 1),
             round(lat_max_pol,digits = 1))




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




####Data frame obs.####
# for (k in seq(1,length(unique_years))) {
#   index_year<- which(calcofi_zooplank_filt == unique_year[k])
#   lon_mtx<- as.matrix(calcofi_zooplank_filt$longitude[index_year],
#                       nrow= calcofi_zooplank_filt$longitude[index_year],
#                       ncol = calcofi_zooplank_filt$latitude[index_year],
#                       byrow = F)
#   lat_mtx<- as.matrix(calcofi_zooplank_filt$latitude[index_year],
#                       nrow= calcofi_zooplank_filt$longitude[index_year],
#                       ncol = calcofi_zooplank_filt$latitude[index_year],
#                       byrow = T)
#   
#   
# }
squid_obs<- data.frame(year= as.factor(calcofi_zooplank_filt$start_yr),
                       time= calcofi_zooplank_filt$j_day,
                       lon= calcofi_zooplank_filt$longitude,
                       lat= calcofi_zooplank_filt$latitude,
                       eggs= calcofi_zooplank_filt$squid_egg_dens)


hist(squid_obs$eggs)
library(dplyr)
library(plyr)

# quant_eggs <- seq(0,
#                   max(squid_obs$eggs),
#                   round_any(max(squid_obs$eggs)/18,
#                             0.5))

######### solo hay un dato mayor a 2.5 poner quantiles o divisiones cada 0.5 hasta 2.5 y de ahi poner el maximo
quant_eggs <- c(min(squid_obs$eggs, na.rm = T),
                0.5,
                1,
                1.5,
                2,
                2.5,
                3,
                max(squid_obs$eggs, na.rm = T))



abline(v=quant_eggs,col="red")


# # Convert so ColorBrewer understands:
squid_obs$squid_dens <- cut(squid_obs$eggs,
                                      breaks=quant_eggs,
                                      include.lowest=F,
                                      right=FALSE)



# # Labels:
labels_eggs <- paste(quant_eggs[-length(quant_eggs)],
                     quant_eggs[-1],
                       sep="-")



#### Create d.frame for labels ####
unique_year<- unique(squid_obs$year)

label_year<-data.frame(lon_l=max(calcofi_zooplank_filt$longitude, na.rm=T)-2,
                       lat_l=max(calcofi_zooplank_filt$latitude,na.rm=T)-2,
                       lab=paste(unique_year),
                       year=unique_year)

                      

label_island<- data.frame(lon= -118.278839,
                          lat= 29.03309,
                          lab= "GI")

#### map ####
require(ggplot2)
require(RColorBrewer) # Color palletes
# For map units:
require(grid)
# For function alpha in label backgrounds:
require(scales)
# For north symbol and scale:
require(ggsn)
# Nuew color palettes viridis:
require(viridis)





squid_dist<- 
  ggplot(data = squid_obs, 
                mapping = aes(x=lon, y = lat )) + 
  # geom_raster(aes(fill = eggs))+
  # Map-projection:
  coord_map(projection="mercator")+
  
  # geom_path(data= calcofi_zooplank_filt,
  #            aes(x=longitude,
  #                y=latitude),
  #            colour="#addd8e",
  #           # linetype="dashed",
  #            # size= 0.1,
  #            alpha = 0.7)+
  # # 
  
  geom_tile(data= squid_obs,
              aes(x= lon,
                  y= lat,
                fill= squid_dens),
             # stroke= 21,
             # color= "black",
             size= 1,
             alpha= 0.7)+
  # # 
scale_color_brewer(palette="Spectral",
                  direction=-1,
                  na.value=NA,
                  
                  #labels=labels_eggs,
                  guide=guide_legend(title=expression(paste("Squid eggs")),
                                     title.position="top",
                                     label.position="bottom",
                                     label.hjust=0.5,
                                     label.vjust=0.5,
                                     nrow=1))+
 
  # # Polygon:
  geom_polygon(data = polygon_gfs,
               aes(x = X1,
                   y = X2),
               fill="NA",
               color = "purple2",
               size = 0.3)+
  # # Land:
  geom_polygon(data=land_polyg,
               aes(x=long,
                   y=lat,
                   group=group),
               fill="gray95",
               colour="black",
               size=0.5)+
  
  
  # Countries boundaries:
  geom_path(data=bound_polyg,
            aes(x=X-360,
                y=Y,
                group=PID),
            color="darkgray",
            size=0.4,
            alpha=0.4)+
  
  
  geom_label(data=label_year,
             aes(x=lon_l,
                 y=lat_l,
                 label=lab),
             hjust=0.1,
             vjust=0.1,
             color="black",
             fill= "white",
             size=4,
             fontface= "bold")+ 
  
  geom_point(data= label_island,
             aes(x= lon[1],
                 y= lat[1]),
             shape = 23,
             size= 1.5,
             color= "darkred",
             fill= "darkred")+
  
  
  geom_text(data = label_island,
            aes(x= lon+0.6,
                y=lat-0.6,
                label = lab),
            size= 2)+
  
  labs(y="",x="")+
  theme_bw()+
  theme(
    legend.position=c(0.5,-0.04), # c(left,bottom)
    legend.direction="horizontal",
    
    legend.title=element_text(size=8.5),
    legend.title.align=0.5,
    legend.text=element_text(size=8.5),
    axis.text=element_text(size=8,
                           colour="black"),
    
    # axis.ticks.length=unit(0.09,
    #                        units="cm"),
    panel.border=element_rect(colour="black",
                              fill=NA,
                              size=0.4),
    panel.grid=element_line(colour="black"),
    panel.grid.minor=element_line(size=0.2),
    panel.grid.major=element_line(size=0.25),
    plot.margin=unit(c(0.5,0.1, 2, 0),units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  
  scale_x_continuous(limits = c(lim_lon[1],
                                (lim_lon[2])),
                     breaks = seq(ceiling(min(lim_lon)+2),
                                  max(lim_lon),
                                  10),
                     labels = paste(as.character(abs(seq(ceiling(min(lim_lon)+2),
                                                         max(lim_lon),
                                                         10))),
                                    "°W",sep = ""),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(lim_lat[1]-1,lim_lat[2]),
                     breaks = seq(ceiling(lim_lat[1]-1),floor(lim_lat[2]),5),
                     labels = paste(as.character(abs(seq(ceiling(lim_lat[1]-1),floor(lim_lat[2]),5))),
                                    "°N",sep = ""),
                     expand = c(0,0)) +
  
  coord_fixed()+
  
  
  
  #     # Makeup:
  facet_wrap(~year, ncol = 4, nrow = 3)+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
  
x11()
plot(squid_dist)

#### Layout and file ####
tiff(filename="map_calcofi_squids_eggs.tiff",
     width=25,height=27,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(squid_dist)
dev.off() 

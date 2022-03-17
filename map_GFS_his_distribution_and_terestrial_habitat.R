#### Clean everything: ####
rm(list=ls())  # clear all;
graphics.off() # close all;
gc()           # clear cache.. (?)




####Open polygon 700-km####

# Read KML file (polygon): ### this can be improved with a procedure similar to the one of the islads below...
require(maptools) # To read and process .kml polygons
polyg_700km <- data.frame(getKMLcoordinates("GFS_core_McCue_Gallo.kml",ignoreAltitude=TRUE))

# Read KML file (polygon): ### this can be improved with a procedure similar to the one of the islads below...
require(maptools) # To read and process .kml polygons
polyg_his_dits <- data.frame(getKMLcoordinates("historical_dist_gfs_etnier.kml",ignoreAltitude=TRUE))

##boat based count ####
# require(maptools) # To read and process .kml polygons
# polyg_boat <- data.frame(getKMLcoordinates("boat_area_gfs.kml",ignoreAltitude=TRUE))
# 
# ##foot based count ####
# require(maptools) # To read and process .kml polygons
# polyg_foot <- data.frame(getKMLcoordinates("foot_area_gfs.kml",ignoreAltitude=TRUE))

#### PATH FOR MAP 2####
require(ggplot2) # ploting

# Set path:
land_path_2<- system.file("gshhs_f.b",package="ggplot2") # Low resolution doesn't work (??)
bound_path_2 <- system.file("wdb_borders_f.b",package="ggplot2")

# Extract from world database:
#### Coord limits for maps: ####
lim_lon_2 <- c(-118.4,-118.16)
lim_lat_2 <- c(28.8,29.25)




# Limits:
x_limits_2 <- c(360+lim_lon_2[1],360+(lim_lon_2[2]))
y_limits_2 <- c(lim_lat_2[1],lim_lat_2[2])#revisar si es necesario el 0.7 

# Land polygon (coast line):

library(rgeos)
library(rgdal)
library(ggplot2)
require(maptools)
 
land_polyg_2 <- getRgshhsMap(land_path_2,
                           xlim=x_limits_2,
                           ylim=y_limits_2,
                           level = 1)

# Boundaries:
bound_polyg_2 <- Rgshhs(bound_path_2,
                      xlim=NULL,
                      ylim=NULL,
                      level=1)
bound_polyg_2 <- SpatialLines2PolySet(bound_polyg_2$SP) # Convert to polygon

bound_polyg_2 <- bound_polyg_2[bound_polyg_2$X > x_limits_2[1] &
                             bound_polyg_2$X < x_limits_2[2],] # Filter data by limits
bound_polyg_2 <- bound_polyg_2[bound_polyg_2$Y > y_limits_2[1] &
                             bound_polyg_2$Y < y_limits_2[2],] # Filter data by limits


# Fortify to convert into dataframe
land_polyg_2 <- fortify(land_polyg_2)
bound_polyg_2 <- fortify(bound_polyg_2)

####foot area####
#which(land_polyg_2$long %in% polyg_foot$X1) 



lim_lon<-c(-135,-106)
lim_lat<-c(16,50)
##### IMPORT AND FILTER DATA ######

#### COAST LINES AND BORDERS from GSHHS database: ####
# BINARY files obtained from: 
# http://www.soest.hawaii.edu/pwessel/gshhg/
# The ZIP file HAS to be extracted into one of R's
# packages. In this case, just for convenience, into 
# the ggplot2's folder (i.e. Documents/R/win-library/2.15/ggplot2)

# Set path:
land_path <- system.file("gshhs_f.b",package="ggplot2") # Low resolution doesn't work (??)
bound_path <- system.file("wdb_borders_f.b",package="ggplot2")

# Extract from world database:
#### Coord limits for maps: ####

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

# Fortify to convert into dataframe
land_polyg <- fortify(land_polyg)
# Boundaries:
bound_polyg<- Rgshhs(bound_path_2,
                        xlim=NULL,
                        ylim=NULL,
                        level=1)
bound_polyg<- SpatialLines2PolySet(bound_polyg$SP) # Convert to polygon

bound_polyg <- bound_polyg[bound_polyg$X > x_limits[1] &
                                 bound_polyg$X < x_limits[2],] # Filter data by limits
bound_polyg <- bound_polyg[bound_polyg$Y > y_limits[1] &
                                 bound_polyg$Y < y_limits[2],] # Filter data by limits


# Fortify to convert into dataframe

bound_polyg <- fortify(bound_polyg)

#foot_area<- data.frame(lon= land_polyg$long[which(land_polyg$long<=)])


####map 2####
label_df_2<-data.frame(lon_l=c(-130,-130.2),
                       lat_l=c(20,19.3),
                       lab=c("NORTHEAST","PACIFIC OCEAN"))
label_gulf<-  data.frame(lon_l= -112,
                         lat_l= 28,
                         lab= "GULF OF CALIFORNIA")
label_country <- data.frame(lon_l= c(-111,-116),
                            lat_l= c(29.5,41),
                            lab= c("MEX", "USA"))
label_guadalupe<-  data.frame(lon_l= -118.27,
                         lat_l= 29.05,
                         lab= "Guadalupe Island")

label_island <-  data.frame(lon=c(-124.7145,-122.8046,-119.7,-118.2591,-115.588315, -110.9796),
                            lat=c(48.389,37.777765,33.7,29.0003,28.304666,18.7960),
                            lab=c("Ozette archeol. site","Farallon Islands", "Channel Islands","Guadalupe Island","San Benito Archipelago", "Revillagigedo Archipelago"),
                            use= c("archaeological","rest","rest","breeding","rest","rest"))

campo_lima <- data.frame(lon= -118.229,
                         lat= 28.995,
                         lab= "Campo Lima")
cueva_lefty <- data.frame(lon= -118.22,
                         lat= 29.011,
                         lab= "Cueva de Lefty")
punta_sur <- data.frame(lon= -118.297,
                         lat= 28.885,
                         lab= "Punta Sur")


washington<- data.frame(lon= -121.16360,
                        lat= 47.12922,
                        lab= "Washington")
require(RColorBrewer) # Color palletes
# For map units:
require(grid)
# For function alpha in label backgrounds:
require(scales)
# For north symbol and scale:
require(ggsn)
# Nuew color palettes viridis:
require(viridis)
require(ggrepel)
feeding_area <-
  ggplot()+
  # Map-projection:
  coord_map(projection="mercator")+
  #   # Heat map:
  
  
  # # Polygon:
  #historic distribution
  geom_polygon(data=polyg_his_dits,
               aes(x=X1,
                   y=X2),
               fill="#1985A1",
               colour="NA",
               alpha=0.4)+
  #feeding area
  geom_polygon(data=polyg_700km,
               aes(x=X1,
                   y=X2),
               fill="#fcedee",
               colour="#BD8F75",
               linetype= "dashed",
               alpha=0.3)+
  # Land:
  geom_polygon(data=land_polyg,
               aes(x=long,
                   y=lat,
                   group=group),
               fill="#dcdcdd",
               colour="gray40",
               alpha= 1,
               size=0.2)+
  
  
  # Countries boundaries:
  geom_path(data=bound_polyg,
            aes(x=X-360,
                y=Y,
                group=PID),
            color="gray70",
            size=0.5,
            alpha=0.7)+
  
 
# # #labels 
geom_text(data=label_df_2,
          aes(x=lon_l,
              y=lat_l,
              label=lab),
          hjust=0.1,
          vjust=0.1,
          color="gray40",
          size=2)+
  geom_text(data=label_country,
            aes(x=lon_l,
                y=lat_l,
                label=lab),
            hjust=0.1,
            vjust=0.1,
            color="gray30",
            size=2.5)+
  geom_text(data=washington,
            aes(x=lon,
                y=lat,
                label=lab),
            hjust=0.1,
            vjust=0.1,
            color="gray30",
            size=2.5)+
  geom_text(data=label_gulf,
            aes(x=lon_l,
                y=lat_l,
                label=lab),
            angle= 300,
            hjust=0.1,
            vjust=0.1,
            color="gray40",
            size=2)+
  
  # geom_text(data=label_island,
  #           aes(x=lon,
  #               y=lat,
  #               label=lab,
  #               color= ),
  #           hjust=0.1,
  #           vjust=0.1,
  #           color="black",
  #           size=2)+
  
  geom_text_repel(data = label_island,
                  aes(x= lon,
                      y= lat,
                      label =lab),
                  segment.color = "gray10",
                  color= "gray10",
                  force        = 1,
                  nudge_x      = -0.35,
                  nudge_y = -0.35,
                  direction    = "both",
                  hjust        = 1,
                  segment.size = 0.3,
                  min.segment.length = 0,
                  size= 2.5,
                  alpha=1
                    
  ) +
  
   scale_x_continuous(#limits = c(lim_lon[1],
  #                               (lim_lon[2])),
                     breaks = seq(min(lim_lon),
                                  max(lim_lon),
                                  5),
                     labels = paste(as.character(abs(seq(ceiling(min(lim_lon)),
                                                         max(lim_lon),
                                                         5))),
                                    "°W",sep = ""),
                     expand = c(0,0)) +
  scale_y_continuous(#limits = c(lim_lat[1],lim_lat[2]),
                     breaks = seq(ceiling(lim_lat[1]-1),floor(lim_lat[2]),5),
                     labels = paste(as.character(abs(seq(ceiling(lim_lat[1]-1),floor(lim_lat[2]),5))),
                                    "°N",sep = ""),
                     expand = c(0,0)) +
  geom_text(aes(x=-131,
                y=48.85),
            label= "a)",
            fontface = "bold",
            size=3)+
  # 
  
  coord_cartesian(xlim = c(-132,-108),
                  ylim= c(17,49.5))+
  # Settings:
  labs(y="",x="")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size=10, margin = margin(t = 0, r = 5, b = 0, l = 0)),
    axis.title.x = element_text(size=10),
    # aspect.ratio = 1,
    legend.position = 'none',  # c(left, bottom)
    legend.text = element_text(size=10),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    
    strip.text = (element_text(size = 10)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length = unit(0.2, "cm"), 
    panel.grid = (element_line(size=0.35)),
    panel.grid.minor = element_blank(),
    
    plot.margin=unit(c(0.1,0.1,0.1, 0.01),units="cm") #(top, right, bottom, and left distance from the device borders))
  )
 

####MAP####
guadalupe_area <-
  ggplot()+
  # Map-projection:
  coord_map(projection="mercator")+
 
  #boat habitat
  
  
  # Land:
  geom_polygon(data=land_polyg_2,
               aes(x=long,
                   y=lat,
                   group=group),
               fill="#dcdcdd",
               colour="gray40",
               size=0.3)+
  
  
  # Countries boundaries:
  # geom_path(data=bound_polyg_2,
  #           aes(x=X-360,
  #               y=Y,
  #               group=PID),
  #           color="darkgray",
  #           size=0.4,
  #           alpha=0.4)+
  
  geom_point(data = campo_lima,
             aes(x=lon,
                 y=lat),
             shape= 21,
             fill= "#9cf536",
             color= "black",
             size=2,
             stroke=0.5)+
  geom_point(data = cueva_lefty,
             aes(x=lon-0.002,
                 y=lat),
             shape= 21,
             fill= "#9cf536",
             color= "black",
             size=2,
             stroke=0.5)+
  geom_point(data = punta_sur,
             aes(x=lon,
                 y=lat),
             shape= 21,
             fill= "#9cf536",
             color= "black",
             size=2,
             stroke=0.5)+

geom_text(data=campo_lima,
          aes(x=lon+0.042,
              y=lat,
              label=lab),
          size=2.5)+
  geom_text(data=cueva_lefty,
            aes(x=lon+0.046,
                y=lat,
                label=lab),
            size=2.5)+
  geom_text(data=punta_sur,
            aes(x=lon+0.03,
                y=lat-0.01,
                label=lab),
            size=2.5)+
  geom_text(data=label_guadalupe,
            aes(x=lon_l-0.012,
                y=lat_l+0.02,
                label=lab),
            size=3)+
  geom_text(aes(x=-118.415,
                y=29.24),
            label= "b)",
            fontface = "bold",
            size= 3)+
  
  # # Settings:
  labs(y="",x="")+
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size=10, margin = margin(t = 0, r = 8, b = 0, l = 0)),
    axis.title.x = element_text(size=10),
    # aspect.ratio = 1,
    legend.position = 'none',  # c(left, bottom)
    legend.text = element_text(size=10),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    
    strip.text = (element_text(size = 10)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length = unit(0.2, "cm"), 
    panel.grid = (element_line(size=0.35)),
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.1,0.15, 0.1,0.15),units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  
  scale_x_continuous(#limits = c(lim_lon_2[1],
                       #         lim_lon_2[2]),
                     breaks = seq(min(lim_lon_2),
                                  max(lim_lon_2),
                                  0.1),
                     labels = paste(as.character(abs(round(seq(min(lim_lon_2),
                                                         max(lim_lon_2),
                                                      0.1),digits = 2))),
                                    "°W",sep = ""),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(lim_lat_2[1],lim_lat_2[2]),
                     breaks = seq(lim_lat_2[1],lim_lat_2[2],0.1),
                     labels = paste(as.character(abs(round(seq(lim_lat_2[1],lim_lat_2[2],0.1),digits = 2))),
                                    "°N",sep = ""),
                     expand = c(0,0)) +
  
  coord_cartesian(xlim = c(lim_lon_2[1]-0.03,lim_lon_2[2]+0.03),
                  ylim= c(lim_lat_2[1],lim_lat_2[2]))+
  north(symbol=4,
        location = "topleft",
        scale = 0.2,
        x.min = lim_lon_2[1]+0.05,
        x.max = lim_lon_2[2],
        y.min = lim_lat_2[1]+0.05,
        y.max = lim_lat_2[2],
        anchor = c(x = -118.19, y = 29.26)
  )


  
  
  

   add_scale<- guadalupe_area+ scalebar(dist = 5,
                                           dist_unit = "km",
                                                      location = "bottomleft",
                                                     st.size = 2.5,
                                                      transform = TRUE,
                                                      model =  "International",
                                        border.size = 0.2,
                                        st.dist = 0.02,
                                        st.bottom = FALSE,
                                           x.min = -118.41,
                                           x.max = -118.28,
                                           y.min = 28.81,
                                           y.max=29.1
                                                      
                                             )
# Layout:
require(cowplot)
study_area_plot<-plot_grid(feeding_area, add_scale, ncol= 2)

### Layout and file: 
tiff(filename="map_study_area_guadalupe_1200_ices.tiff",
     width=17,height=24.5*0.5,
     units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(study_area_plot)
dev.off()

plot()

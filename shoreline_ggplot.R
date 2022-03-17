


# Map Guadalupe w/SST:


# Clean:
rm(list = ls(all = TRUE)) #clear all;
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)

load("means_sst_glupe_circ_2015.RData")
str(sst_mean_glupe_2015)


#### Coord limits for maps: ####
lim_lon <- c(floor(min(sst_mean_glupe_2015$lon_mean)),
             ceiling(max(sst_mean_glupe_2015$lon_mean)))
lim_lat <- c(floor(min(sst_mean_glupe_2015$lat_mean)),
             ceiling(max(sst_mean_glupe_2015$lat_mean)))


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
# Limits:
x_limits <- c(360+lim_lon[1],360+lim_lon[2])
y_limits <- c(lim_lat[1],lim_lat[2]-0.7)#revisar si es necesario el 0.7 

# Land polygon (coast line):
require(maptools)
land_polyg <- getRgshhsMap(land_path,
                           xlim=x_limits,
                           ylim=y_limits,
                           level = 1)

# Boundaries:
bound_polyg <- Rgshhs(bound_path,
                      xlim=NULL,
                      ylim=NULL,
                      level=1)
bound_polyg <- SpatialLines2PolySet(bound_polyg$SP) # Convert to polygon

bound_polyg <- bound_polyg[bound_polyg$X > x_limits[1] &
                             bound_polyg$X < x_limits[2],] # Filter data by limits
bound_polyg <- bound_polyg[bound_polyg$Y > y_limits[1] &
                             bound_polyg$Y < y_limits[2],] # Filter data by limits


# Fortify to convert into dataframe
land_polyg <- fortify(land_polyg)
bound_polyg <- fortify(bound_polyg)



##### Process SST:

require(plyr) # for function round_any

hist(sst_mean_glupe_2015$sst_mean)

# Ranges:
quant_sst <- round(seq(floor(min(sst_mean_glupe_2015$sst_mean,na.rm=T)),
                       ceiling(max(sst_mean_glupe_2015$sst_mean,na.rm=T)),
                       round_any(ceiling(max(sst_mean_glupe_2015$sst_mean,na.rm=T))/30,1)),
                   digits = 1)

abline(v=quant_sst,col="red")



# round values higher than the desired resolution:
ind_high <- which(sst_mean_glupe_2015$sst_mean > max(quant_sst))
sst_mean_glupe_2015$sst_mean[ind_high] <- max(quant_sst)

# Convert so ColorBrewer understands:
sst_mean_glupe_2015$quant_sst <- cut(sst_mean_glupe_2015$sst_mean,
                               breaks=quant_sst,
                               include.lowest=TRUE,
                               right=FALSE)

# Labels:
labels_sst <- paste(quant_sst[-length(quant_sst)],
                     quant_sst[-1],sep="-")



# Coordinate of the colony:
lon_colony <- -118.291369
lat_colony <- 28.881736             

# Define circular polygon:
pi <- 3.14159265359
n_points <- 200
radius <- 3.14 # in degrees: approximately 350 km)
pts <- seq(0,2*pi,length.out=n_points)
xy <- cbind(lon_colony + radius * sin(pts),
            lat_colony + radius * cos(pts))
polyg_circ <- as.data.frame(xy)
colnames(polyg_circ) <- c("lon","lat")



#### MAPPING ####
# Color palettes:
require(RColorBrewer) # Color palletes
# For map units:
require(grid)
# For function alpha in label backgrounds:
require(scales)
# For north symbol and scale:
require(ggsn)
# Nuew color palettes viridis:
require(viridis)




# Map object:
map_sst_glupe <-
  ggplot()+
  
  # Map-projection:
  coord_map(projection="mercator")+
  
  # Polygon:
  geom_polygon(data = polyg_circ,
               aes(x = lon,
                   y = lat),
               color = "darkgreen",
               size = 0.75)+
  
  # Heat map :
  geom_tile(data=sst_mean_glupe_2015,
            aes(x=lon_mean,
                y=lat_mean,
                fill=quant_sst),
            alpha=1)+

  scale_fill_brewer(na.value=NA,
                    direction = -1,
                    palette="YlGnBu",

                    labels=labels_sst,
                    guide=guide_legend(title="TSM (°C)",
                                       title.position="top",
                                       title.hjust=0.5,
                                       label.position="bottom",
                                       label.hjust=0.5,
                                       label.vjust=0.5,
                                       keywidth=1.5,
                                       keyheight=0.4,
                                       nrow=1))+
  # Land:
  geom_polygon(data=land_polyg,
               aes(x=long,
                   y=lat,
                   group=group),
               fill="gray95",
               colour="black",
               size=0.3)+
  
  # Countries' boundaries:
  geom_path(data=bound_polyg,
            aes(x=X-360,
                y=Y,
                group=PID),
            color="darkgray",
            size=0.2,
            alpha=0.4)+
  
  # Text labels:
  geom_text(aes(x=-119,
                y=29,
                label="Isla\nGuadalupe"),
            hjust=0.5,
            vjust=0.5,
            color="black",
            size=3.5)+
  geom_text(aes(x=-115.7,
                y=31.25,
                label="Baja\nCalifornia"),
            hjust=0.5,
            vjust=0.5,
            color="black",
            size=3.5)+
  geom_text(aes(x=-121.25,
                y=31.65,
                label="2015"),
            hjust=0.5,
            vjust=0.5,
            color="black",
            size=6)+
  
  # Settings:
  labs(y="",x="")+
  theme_bw()+
  theme(
    legend.position=c(0.73,0.05), # c(left,bottom)
    legend.direction="horizontal",
    legend.box="vertical",
    legend.box.just="left",
    legend.title=element_text(size=8.5,
                              vjust=0),
    legend.text=element_text(size=7),
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
    
    plot.margin=unit(c(0.5,0.5, -2, -0.5),units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  scale_x_continuous(limits = c(lim_lon[1],
                                lim_lon[2]),
                     breaks = seq(ceiling(min(lim_lon))+1,
                                  max(lim_lon),
                                  2),
                     labels = paste(as.character(abs(seq(ceiling(min(lim_lon)),
                                                         max(lim_lon),
                                                         2))),
                                    "°W",sep = ""),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(lim_lat[1],lim_lat[2]-0.7),
                     breaks = seq(ceiling(lim_lat[1]),floor(lim_lat[2]-0.7),2),
                     labels = paste(as.character(abs(seq(ceiling(lim_lat[1]),floor(lim_lat[2]-0.7),2))),
                                    "°N",sep = ""),
                     expand = c(0,0)) +
  coord_fixed()+
  
  # Scalebar:
  scalebar(dist = 100,
           location = "bottomleft",
           height = 0.015,
           # st.dist = 0.005,
           st.size = 2.4,
           dd2km = TRUE,
           model = 'WGS84',
           x.min = lim_lon[1]+0.35,
           x.max = lim_lon[2],
           y.min = lim_lat[1]+0.34,
           y.max = lim_lat[2]-0.7)




#### Layout and file ####
tiff(filename="map_sst_guadalupe.tiff",
     width=13,height=22,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(map_sst_glupe)
dev.off()



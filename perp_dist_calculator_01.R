# PERPENDICULAR DISTANCE CALCULATOR
# FOR DISTANCE SAMPLING
# FOLLOWING Lerczac & Hobbs (1998):

# Clean everything:
rm(list = ls(all = TRUE)) #clear all;
graphics.off() #close all;
gc() #clear cache...

# File sightings:
sight.data <- read.csv("F:/My_R_hp/My_R/sightings_mammals_u_colima_120219.csv",sep = ';')
# sight.data <-sight.data[which(sight.data$sp.name=="unid_delphinid"|sight.data$sp.name=="s_attenuata"|sight.data$sp.name=="s_attenuata_graffmani"|sight.data$sp.name=="s_attenuata_attenuata"),]


############## CALCULATE REAL SIGHTING ANGLE ##################:
mini <- apply(cbind(sight.data$header.trk.sigh,sight.data$angle.sigh),1,min) # Min value by row
maxi <- apply(cbind(sight.data$header.trk.sigh,sight.data$angle.sigh),1,max) # Max value by row
tdiff <- abs(sight.data$header.trk.sigh - sight.data$angle.sigh) # Absolute difference between angle and track



# Conditional substraction between angle and track:
angle.sigh <- rep(NA,length(tdiff))
for (i in seq(1,length(tdiff))) {
  if (is.na(tdiff[i])) {
    angle.sigh[i] <- NA
  } else {
    if (tdiff[i] > 180) {
      angle.sigh[i] <- (360-maxi[i])+mini[i]
    } else {
      angle.sigh[i] <- maxi[i]-mini[i]
    }
  }
}

# Angles larger than 90° are NA:
angle.sigh[angle.sigh > 90] <- NA

########## CALCULATE ANGULAR DROP FROM RETICLES #############
# FUJINON/MT 7x50 FMTRC-SX User's Manual:
# "One mil is equal to the angle needed to view a 1-m object 
# at a distance of 1000m. The binoculars feature a scale of 
# up to 70mils"

# Angle by each "mil":
angle.retic <- atan(1/1000)*10 # *10 because of the survey protocol

# Add observer height to the platform height:
visual.height <- (sight.data$altitude.m.sigh + 1.66)/1000

######### EARTH RADIUS AT SIGHTING LATITUDE: ################
equ.rad <- 6378.137 # equatorial radius (km)
pol.rad <- 6356.7523142 # polar radius (km)
rad.earth <- sqrt(((equ.rad^2*cos(sight.data$lat.sigh))^2
                   + (pol.rad^2*sin(sight.data$lat.sigh))^2)
                  /((equ.rad*cos(sight.data$lat.sigh))^2
                    +(pol.rad*sin(sight.data$lat.sigh))^2))

### ESTIMATE DISTANCE WHEN HORIZON IS THE REFERENCE: ####
# Angle between the line perpendicular to Earth's radius and horizon:
h2h.angle <- atan((sqrt((((2*visual.height)*rad.earth)+
                           (visual.height^2))))/rad.earth)

# Angle between the animals and the platform:
anim2platf.angle = (pi/2)-h2h.angle-(angle.retic*sight.data$reticle.sigh)

# Distance from vessel to horizon (above the Earth's surface):
H <- h2h.angle*rad.earth
max_dist_plats <- unique(round(H,digits=1))

# Distance from eye to animals:
Do <- ((rad.earth+visual.height)*(cos(anim2platf.angle)))-
  sqrt(
    (((rad.earth+visual.height)^2)*(cos(anim2platf.angle))^2)-
      (((2*visual.height)*rad.earth) + visual.height^2)
    )

# Central arc angle from animals to platform (i.e. inverse angle):
arc.angle.plat2anim <- asin(sin(anim2platf.angle)*(Do/rad.earth))

# Radial distance from platform to animals:
rad.D <- sqrt((Do^2)-(visual.height^2))

# Max. radial distances for each platform:
# Mary Chui:
rads.m.chui <- rad.D[sight.data$platf.name.sigh == "marichuy_III"]
max.rad.mchui <- max(rads.m.chui[rads.m.chui < 4],na.rm=T)
# BIP:
rads.bip <- rad.D[sight.data$platf.name.sigh == "BIP_XII"]
max.rad.bip <- max(rads.bip[rads.bip < 6],na.rm=T)


##### PERPENDICULAR DISTANCE FROM RADIAL... #############
# From angle to radians:
library(ggplot2)
library(circular)
sight.angle.rad <- rad(angle.sigh)
# Arc:
arc <- asin(rad.D/rad.earth)*rad.earth
perp.dist <- sin(sight.angle.rad)*arc


#####################################################
### ESTIMATE DISTANCE WHEN LAND IS THE REFERENCE ####
#####################################################

# Central arc angle from shoreline to platform:
y <- sight.data$dist.to.land/rad.earth

# Distance from observer's eyes to shoreline:
Lo <- sqrt(rad.earth^2
           +(rad.earth+visual.height)^2
           -2*rad.earth*(rad.earth+visual.height)*cos(y))

# Angle from animal to platform:
anim2platf.angle <- acos((2*visual.height*rad.earth+visual.height^2+Lo^2)
                         /(2*(rad.earth+visual.height)*Lo))-anim2platf.angle

# Distance from eyes to animals:
Do <- ((rad.earth+visual.height)*(cos(anim2platf.angle)))-
  sqrt((((rad.earth+visual.height)^2)*(cos(anim2platf.angle))^2)-
  (((2*visual.height)*rad.earth) + visual.height^2))

# Central arc angle from animals to platform (i.e. inverse angle):
arc.angle.plat2anim <- asin(sin(anim2platf.angle)*(Do/rad.earth))

# Radial distance from platform to animals:
rad.D <- sqrt((Do^2)-(visual.height^2))

# Perpendicular distance when land is horizon:
perp.dist.2 <- visual.height/tan(h2h.angle+(angle.retic*sight.data$reticle.sigh))

### INSERT DISTANCES CALCULATED WHEN LAND IS THE  REFERENCE:
# Find which distances from platform to land are larger than the 
# maximum possible distance to horizon.
i1 <- which(sight.data$dist.to.land < perp.dist[1])
i2 <- which(sight.data$dist.to.land < perp.dist[2])

for (kk in seq(1,length(i1))) {
  if (sight.data$platf.name.sigh[i1[kk]] == "BIP_XII") {
        perp.dist[i1[kk]] <- perp.dist.2[i1[kk]]
  } else {
    
  } 
}

for (kk in seq(1,length(i2))) {
  if (sight.data$platf.name.sigh[i2[kk]] == "marichuy_III") {
    perp.dist[i2[kk]] <- perp.dist.2[i2[kk]]
  } 
}

##### REPLACE SOME NA WHERE THERE'S #####
##### IN-SITU ESTIMATED DISTANCES  ######
i2 <- which(is.na(perp.dist))
perp.dist[i2] <- sight.data$dist.eye.est[i2]

# Save to working directory as CSV:
write.csv(perp.dist.2,file="perp_dist_col_land.csv")



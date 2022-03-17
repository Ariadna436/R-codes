# Clean:
rm(list=ls())  # clear all
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)


## Historical abundances:
abun_area_df<- read.csv("gfs_count_by_stratum_rel_abund_boat_corrected_CF_pond.csv")
abun_area_df$sqr_km <- abun_area_df$sqr_m/1000000
abun_area_df$dens_km<- abun_area_df$total_corrected/abun_area_df$sqr_km
# Re-order factor levels:
#abun_area_df$area <- as.factor(abun_area_df$area)


sink("gfs_by_stratum_year_effect_dnorm_2nd_dens_100m.txt")
### JAGS MODEL ###
cat(
"model {
  
       # Model of counts as a function of the year
       # with fixed effects of each area:
       for (i in 1:n_counts) {
         count[i] ~ dnorm(mu_count[i],1/pow(sd_count,2))                   # Normal likelihhod
         #p[i] <- r/(r+mu_count[i])
        log(mu_count[i])<- log_mu_count[i]
        log_mu_count[i] <- alpha0[area[i]] +  
                        alpha1[area[i]]*year[i] +
                        alpha2[area[i]]*pow(year[i],2)#+e0[i]              # 2nd-order polynomial
      #e0[i] ~ dnorm(0, 1/pow(sd_e0,2)) 
      error_vec[i]<- count[i] - mu_count[i]                                #Error vector for R2 estimations
                                          
      squared_error_vec[i] <- pow((count[i] - mu_count[i]),2)            # Squared residuals for observed data
      new_mu_count[i] ~  dnorm(mu_count[i],1/pow(sd_count,2))                                 # Replicate (one new data set at each MCMC iteration)
      new_squared_error_vec[i] <- pow((new_mu_count[i] - mu_count[i]), 2)   # Squared residuals for new data
}


 #### POSTERIOR PREDICTIVE CHECKS ####
 # # # Bayesian p-value fordensity by area model:
 fit_dens_area <- sum(squared_error_vec[])               # Sum of squared residuals for actual dataset
 fit_new_dens_area <- sum(new_squared_error_vec[])       # Sum of squared residuals for replicated dataset
 test_fit_dens <- step(fit_new_dens_area-fit_dens_area)  # Test whether new data set is more extreme
 b_p_value_dens_area <- mean(test_fit_dens)              # Bayesian p-value (0.5 expected)
  

     r_squ_yr<- pow(sd(mu_count),2)/
                    (pow(sd(mu_count),2) +
                    (pow(sd(error_vec),2)))
  
      
       # Estimations of area parameters:
       for (j in 1:n_areas) {
         
         # Priors of 2nd-order polynomial
         # coefficients per area (uninformative): 
         # alpha0[j] ~ dnorm(0,0.0001)
         # alpha1[j] ~ dnorm(0,0.0001)
         # alpha2[j] ~ dnorm(0,0.0001)
         alpha0[j] ~ dunif(min_alpha0[j], max_alpha0[j])
         alpha1[j] ~ dunif(min_alpha1[j], max_alpha1[j])
         alpha2[j] ~ dunif(min_alpha2[j], max_alpha2[j])
       }
      
       sd_count ~ dunif(min_sd,max_sd)
       #alpha0 ~ dunif(min_alpha0, max_alpha0)
       # Yearly predictions of total population
       # size and abundance per colony:
       for (k in 1:n_yrs_pred) {                                    # Loop on years of prediction
         for (l in 1:n_areas) {                                     # Loop on colonies
           pred_count_area[l,k] <- exp(alpha0[l] + 
                                       alpha1[l]*yr_pred[k]
                                      + alpha2[l]*pow(yr_pred[k],2)) 
         }
         
         # Predict annual population size
         # as summ of the colonies'
         pred_sum_gpe[k] <- sum(pred_count_area[,k]) 
                                     
       }
  

    }",
fill=TRUE) # end of JAGS model
sink() 


# 
# Ranges of alphas:
unique_area <- unique(abun_area_df$area)
df_uniform_ranges <- data.frame()
for (i in seq(1, length(unique_area))) {
  df_uniform_ranges[i, "min_alpha0"] <- 0
                                        # ifelse(min(abun_area_df$dens_km[abun_area_df$area == unique_area[i]],
                                        #            na.rm = TRUE) == 0, log(1), log(min(abun_area_df$dens_km[abun_area_df$area == unique_area[i]],
                                        #                                                na.rm = TRUE)))
  df_uniform_ranges[i, "max_alpha0"] <- log(max(abun_area_df$dens_ind[abun_area_df$area == unique_area[i]],
                                          na.rm = TRUE*100) +
                                          min(abun_area_df$dens_ind[abun_area_df$area == unique_area[i]],
                                          na.rm = TRUE*100))
   df_uniform_ranges[i, "min_alpha1"] <- -5
  df_uniform_ranges[i, "max_alpha1"] <- 10

    df_uniform_ranges[i, "min_alpha2"] <--3
    df_uniform_ranges[i, "max_alpha2"] <- 3
}



data_list <- list(
  # Temporal trend:
  n_counts = nrow(abun_area_df),
  count = abun_area_df$dens_ind*100,
  area = abun_area_df$area,
  year = (abun_area_df$year)/1000, #- mean(abun_area_df$year))/sd(abun_area_df$year),
  n_areas = length(unique(abun_area_df$area)),
  
   #min_alpha0 = 7, max_alpha0 = 10,
  min_alpha0 = df_uniform_ranges$min_alpha0,
  max_alpha0 = df_uniform_ranges$max_alpha0,
  #
  min_alpha1 = df_uniform_ranges$min_alpha1,
  max_alpha1 = df_uniform_ranges$max_alpha1,


    min_alpha2 = df_uniform_ranges$min_alpha2,
    max_alpha2 = df_uniform_ranges$max_alpha2,
  # # 
   min_sd= 0 , max_sd = 2,
  
  # Yearly predictions:
  n_yrs_pred = length(seq(min(abun_area_df$year)-2, 
                          max(abun_area_df$year)+2, 1)),
  yr_pred = (seq(min(abun_area_df$year)-2,
                 max(abun_area_df$year)+2, 1))/1000 #- mean(abun_area_df$year))/sd(abun_area_df$year)
  
  
  )


###########################
## set MCMC controls
############################
n.iter <- 100000
n.chains <- 3
n.thin <- 20
n.burn <- round(n.iter*0.2)


#############################
### set parameters to monitor
#############################
monitor <- c("alpha0",
             "alpha1",
              "alpha2",
             #"r",
             #"e0",
            "sd_count",
            "r_squ_yr",
            "b_p_value_dens_area",
             "pred_count_area",
           
             "pred_sum_gpe"
                         )


###########################
## run JAGS model
###########################
require(jagsUI)
out <- jags(data=data_list,
            parameters.to.save=monitor,
            model.file="gfs_by_stratum_year_effect_dnorm_2nd_dens_100m.txt",
            n.chains=n.chains,
            n.thin=n.thin,
            n.iter=n.iter,
            n.burnin=n.burn)

      


#### MODEL RESULTS ########
# Summarize results:
require(MCMCvis)
model_summary <- MCMCsummary(out, params = "all",
                              probs = c(0.025, 0.5, 0.975), round = 4, func = function(x) ecdf(x)(0))
# # Add the n.eff in %:
#model_summary$n.eff.perc <- round((model_summary$n.eff/out$mcmc.info$n.samples)*100,digits = 3)


# Save summary as CSV table:
# write.csv(model_summary$summary,
#           file = "table_results_calif_sea_lion_rookery_trends.csv")


MCMCtrace(object=out,
          params = c("alpha0",
                     "alpha1",
                     "alpha2",
                    #"e0",
                    "sd_count",
                     "r_squ_yr",
                    "b_p_value_dens_area",
                     "deviance"),
          # main_den = c(bquote(alpha[0]),
          #              bquote(alpha[1]),
          #              bquote("DIC")
          #              
          # ),
          xlab_den = "",
          xlab_tr = NULL,
          sz_main_txt = 1.5,
          n.eff = F,
          type = 'density',
          ind = TRUE,
          pdf = T,
          filename = "posteriors_of_interest_model_gfs_by_stratum_year_effect_dnorm_2nd_dens_100m.pdf")

# # Save summary as CSV table:
# posterior_statistics<- as.data.frame(model_summary)
# write.csv(posterior_statistics,
#           file = "table_results_gfs_by_stratum_year_effect_norm_2nd.csv")
#save(data_list,out, file = "model_results_gfs_by_stratum_16_pois_2nd.RData")
#   
  
## Construct the prediction dataframes:
## FOr each rookery:
tot_count_predict <- data.frame(
  pred_years = seq(min(abun_area_df$year, na.rm = T)-2,
                   max(abun_area_df$year, na.rm = T)+2)
  )
tot_count_predict$pred_years_sc = (tot_count_predict$pred_years - 
                                     mean(abun_area_df$year))/sd(abun_area_df$year)


pred_all_area<-  data.frame(
  area= rep(1:16, times=nrow(tot_count_predict)))
  #year = rep(tot_count_predict$pred_years,16)
  # med_count = model_summary$`50%`[34:833],
  # lwr_count = model_summary$`2.5%`[34:833],
  # upr_count = model_summary$`97.5%`[34:833]


unique_years<-  unique(tot_count_predict$pred_years)
list_year<- list()
temp_pred_all_area<- data.frame()
for (i in 1:length(unique_area)) {
  for(k in 1:length(unique_years)){
    temp_pred_all_area[k, "pred_year"] <-  unique_years[k]
    temp_pred_all_area[k, "med_pred_count"] <-  round(model_summary[paste("pred_count_area[",i,",",k,"]",sep=""), "50%"],digits = 3)
    temp_pred_all_area[k, "lwr_pred_count"] <-  round(model_summary[paste("pred_count_area[",i,",",k,"]",sep=""), "2.5%"],digits = 3)
    temp_pred_all_area[k, "upr_pred_count"] <-  round(model_summary[paste("pred_count_area[",i,",",k,"]",sep=""), "97.5%"],digits = 3)
    
    }
  list_year[[i]]<-temp_pred_all_area
}
pred_count_df<- do.call(rbind,list_year)

pred_count_df$area<- rep(1:16, each=length(unique_years))


 pred_count_df$med_pred_count[which(pred_count_df$med_pred_count < 0)] <- 0
 pred_count_df$lwr_pred_count[which(pred_count_df$lwr_pred_count < 0)] <- 0
 pred_count_df$upr_pred_count[which(pred_count_df$upr_pred_count < 0)] <- 0


#abun_area_df$dens_area<- abun_area_df$total_corrected/abun_area_df$length

# Re-order authors:
#abun_area_df$reference <- factor(abun_area_df$reference)
                               


require(ggplot2)
figure_area <- 
  ggplot()+
  # Bayesian results:
  geom_ribbon(data = pred_count_df,
              aes(x = pred_year,
                  ymin = lwr_pred_count,
                  ymax = upr_pred_count),
              fill = "lightgray",
              color = NA,
              alpha = 0.6)+
  geom_path(data = pred_count_df,
            aes(x = pred_year,
                y = med_pred_count,
                group = area),
            color = "darkgray",
            size = 0.4)+
  # Observations:
  # Fake white:
 
 
  # Dots:
  geom_point(data = abun_area_df,
             aes(x = year,
                 y = dens_km),
             shape = 21,
             fill = "black",
             color = "white",
             size = 1,
             stroke = 0.2)+
  
  # # Original counts:
  # geom_point(data = abun_area_df,
  #            aes(x = year,
  #                y = total_corrected/1000,
  #                fill = author),
  #            shape = 21,
  #            color = "white",
  #            size = 1.2,
  #            stroke = 0.4)+
  # scale_fill_brewer(palette = "Dark2",
  #                   direction = 1,
  #                   guide = guide_legend(
  #                     ncol = 2,
  #                     override.aes = list(size = 3)))+

  # Settings:
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 5.5),
    axis.title.y = element_text(size=12),
    
    legend.position = c(0.625 , 0.077),  # c(left, bottom)
    legend.text = element_text(size=8.5),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    
    strip.text = (element_text(size = 8.5)),
    axis.ticks = (element_line(size=0.1)),
    panel.grid = (element_line(size=0.12)),
    
    plot.margin=unit(c(0.15, 0.3, 7.5, 0.02),units="cm"), #top, right, bottom, and left 
    strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
    )+
  # scale_x_continuous(limits = c(min(tot_count_predict$pred_years)-2,
  #                               max(tot_count_predict$pred_years)+2),
  #                    breaks = seq(1965, 2015, 10),
  #                    expand = c(0, 0))+
  # 
  labs(x="",
       y=bquote("Density (ind./km ^2)"))+
  facet_wrap(~area, ncol=4, scales="free_y")

### SAVE AS JOURNAL FIGURE: ###
tiff(filename="gfs_by_stratum_16_ind_dens_norm_2nd.tiff",
     width=17.3,height=23.5*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(figure_area)
dev.off()






#### Inferred sum of counts: ####
# Create the dataframe:
## For the GUlf:
sum_count_predict <- data.frame(
  pred_years = seq(min(abun_area_df$year), max(abun_area_df$year))
)
for (i in seq(1, nrow(sum_count_predict))) {
  sum_count_predict[i, "med_count"] <-  model_summary$summary[paste("pred_sum_gulf[",i,"]",sep=""), "50%"]
  sum_count_predict[i, "lwr_count"] <-  model_summary$summary[paste("pred_sum_gulf[",i,"]",sep=""), "2.5%"]
  sum_count_predict[i, "upr_count"] <-  model_summary$summary[paste("pred_sum_gulf[",i,"]",sep=""), "97.5%"]
  sum_count_predict[i, "sd_count"] <-  model_summary$summary[paste("pred_sum_gulf[",i,"]",sep=""), "sd"]
  sum_count_predict[i, "mean_count"] <-  model_summary$summary[paste("pred_sum_gulf[",i,"]",sep=""), "mean"]
}

# Create dataframe only with years of > 10 rookeries counted:
uniq_years_tot_counts <- unique(abun_area_df$year)
count_rook_yr <- rep(NA, length(uniq_years_tot_counts))
for (i in seq(1, length(uniq_years_tot_counts))) {
  index_yrs <- which(abun_area_df$year == uniq_years_tot_counts[i])
  count_rook_yr[i] <- length(unique(abun_area_df$rook_labels[index_yrs]))
}

years_choosen <- uniq_years_tot_counts[which(count_rook_yr > 10)]
rooks_count_chosen <- count_rook_yr[which(count_rook_yr > 10)]

sum_abund_df <- data.frame()
for (j in seq(1, length(years_choosen))) {
  sum_abund_df[j, "sum"] <- sum(abun_area_df$total_corrected[which(abun_area_df$year == years_choosen[j])])
  sum_abund_df[j, "year"] <- years_choosen[j]
  sum_abund_df[j, "author"] <- paste(abun_area_df$author[which(abun_area_df$year == years_choosen[j])][1],
                                     ": ", rooks_count_chosen[j], " colonies", sep = "")
}


# Add the drone correction:
sum_abund_df$sum_drone_corr_med <- sum_abund_df$sum * (1 + model_summary$summary["prop_non_det", "50%"])
sum_abund_df$sum_drone_corr_lwr <- sum_abund_df$sum * (1 + model_summary$summary["prop_non_det", "2.5%"])
sum_abund_df$sum_drone_corr_upr <- sum_abund_df$sum * (1 + model_summary$summary["prop_non_det", "97.5%"])


# Add the at-sea correction:
sum_abund_df$sum_at_sea_corr_med <- 
  sum_abund_df$sum_drone_corr_med +
  ((sum_abund_df$sum_drone_corr_med *
      model_summary$summary["mu_prop_non_pups","50%"]) * 
     model_summary$summary["mu_at_sea_prop","50%"] ) *
  (1 + model_summary$summary["prop_other_rook","50%"])

sum_abund_df$sum_at_sea_corr_lwr <- 
  sum_abund_df$sum_drone_corr_lwr +
  ((sum_abund_df$sum_drone_corr_lwr *
      model_summary$summary["mu_prop_non_pups","2.5%"]) * 
     model_summary$summary["mu_at_sea_prop","2.5%"] ) *
  (1 + model_summary$summary["prop_other_rook","2.5%"])

sum_abund_df$sum_at_sea_corr_upr <- 
  sum_abund_df$sum_drone_corr_upr +
  ((sum_abund_df$sum_drone_corr_upr *
      model_summary$summary["mu_prop_non_pups","97.5%"]) * 
     model_summary$summary["mu_at_sea_prop","97.5%"] ) *
  (1 + model_summary$summary["prop_other_rook","97.5%"])




# Create a dataframe with the decadal trends in percentaje:
decad_change_df <- data.frame()
for (i in seq(1, data_list$n_index_deca)) {
  decad_change_df[i, "pred_change"] <- paste(round(
    model_summary$summary[paste("diff_decade_perc[",i,"]",sep=""), "50%"]),
    "%", sep = "")
}

decad_change_df$x_location <- seq(1985, 2020, 10) 
decad_change_df$pred_change[1] <- paste("+", decad_change_df$pred_change[1] , sep = "")
decades <- c("80's: ","90's: ","00's: ", "10's: ")
decad_change_df$pred_change <- paste(decades, decad_change_df$pred_change, sep="")


# 3-DECADE decline
three_dedc_decline <- paste("28-year\ndecline: ",
                            abs(round(model_summary$summary["diff_max_min", "50%"],
                                  digits = 1)),
                            "%",
                            sep = "")







## SST STUFF: ####

# Open .nc:
require(ncdf4)
filename <- "D:/mario/Documents/My_R/data_r/SST/SST_gulf_ca_1970-2018/erdHadISST_8e16_9910_56a6.nc"
nc_base <- nc_open(filename)
print(nc_base)




# Define the running-mean period:
running <- 25

# Coordinates:
time_sst <- ncvar_get(nc_base,
                      'time')/60/60/24 # days since 01-01-1970
year_sst <- as.numeric(format(as.Date(time_sst, origin= "1970-01-01"), format = "%Y"))
index_neg <- which(year_sst < 1920-running)

time_sst <- time_sst[-index_neg]
year_sst <- year_sst[-index_neg]

lon_sst <- ncvar_get(nc_base,
                     'longitude')
lat_sst <- ncvar_get(nc_base,
                     'latitude')
sst <- ncvar_get(nc_base,
                 'sst')
sst <- sst[ , ,-index_neg]


# Close the .nc dataset:
nc_close(nc_base)


## Construct the data frame:
sst_df_list <- list()
for (i in seq(1, length(time_sst))) {
  sst_df <- data.frame(
    time = c(matrix(time_sst[i],
                    ncol = ncol(sst[,,i]),
                    nrow = nrow(sst[,,i]))),
    year = c(matrix(year_sst[i],
                    ncol = ncol(sst[,,i]),
                    nrow = nrow(sst[,,i]))),
    sst = c(sst[, , i]),
    lon = c(matrix(data=lon_sst,
                   nrow=length(lon_sst),
                   ncol=length(lat_sst),
                   byrow=FALSE)),
    lat = c(matrix(data=lat_sst,
                   nrow=length(lon_sst),
                   ncol=length(lat_sst),
                   byrow=TRUE))
  )
  sst_df_list[[i]] <- sst_df
}
sst_df <- do.call("rbind",
                  sst_df_list)
sst_df <- sst_df[which(!is.na(sst_df$sst)), ]







# Take out data outside the gulf:
# Read KML file (polygon):
require(maptools)
require(SDMTools)
polyg_gulf_ca <- data.frame(getKMLcoordinates("D:/mario/Documents/My_R/data_r/polygons/polyg_b_physalus_gulf_ca/00_all_gulf_ca.kml",
                                              ignoreAltitude=TRUE))
i3  <- pnt.in.poly(cbind(sst_df$lon,
                         sst_df$lat),
                   polyg_gulf_ca)

sst_df <- sst_df[i3$pip == 1,]
gc()



# Estimate the means:
# Empirically:
mean_sst_df <- data.frame()
for(i in seq(1, length(unique(sst_df$time)))) {
  index_yr <- which(sst_df$time <= unique(sst_df$time)[i] &
                      sst_df$time >= (unique(sst_df$time)[i]-(running*365.25)))     
  mean_sst_df[i, "time"] <- unique(sst_df$time)[i]
  
  mean_sst_df[i, "year"] <- as.numeric(format(as.Date(unique(sst_df$time)[i], origin= "1970-01-01"), format = "%Y"))
  
  mean_sst_df[i, "mean_sst"] <- mean(sst_df$sst[index_yr], na.rm = T)
  mean_sst_df[i, "sd_sst"] <- sd(sst_df$sst[index_yr], na.rm = T)
}

# Minimium Julian day:
min_jul_des_sst <- as.numeric(unclass(ISOdatetime(1920, 1, 1, 
                                                  hour=0, min=0, sec=0,
                                                  tz="Greenwich")))/(60*60*24)
max_jul_des_sst <- as.numeric(unclass(ISOdatetime(2020, 1, 1, 
                                                  hour=0, min=0, sec=0,
                                                  tz="Greenwich")))/(60*60*24)



# Cut the period:
mean_sst_df <- mean_sst_df[which(mean_sst_df$year >= 1920 & mean_sst_df$year <= 2020),]






# SST model:
## JAGS MODEL ###
sink("sst_century_means_vs_pred_abund.txt")
cat(
  "model {
       
       # Estimation of the century SST means:
       for (i in 1:n_sst_mean) {
         sst_mean[i] ~ dnorm(centu_mean,                  # Normal likelihood of SST:
                             1/pow(sd_sst_mean[i], 2))    # with known (espatial-temporal) SD's
         # Estimation of anomalies (one-month resolution):
         sst_month_anom[i] <- sst_mean[i] - centu_mean    
       }
       # Uninformative prior on the mean:
       centu_mean ~ dunif(min_mu_sst, 
                          max_mu_sst)

   }",

  fill=TRUE) # end of JAGS model
sink() 

data_list <- list(
  # Temporal trend:
  n_sst_mean = nrow(mean_sst_df),
  sst_mean = mean_sst_df$mean_sst,
  # year = as.factor(mean_sst_df$year),
  sd_sst_mean = mean_sst_df$sd_sst,
  # n_years = length(levels(as.factor(mean_sst_df$year))),
  
  min_mu_sst = 5, max_mu_sst = 35
  )


###########################
## set MCMC controls
############################
n.iter <- 1000000
n.chains <- 3
n.thin <- 20
n.burn <- round(n.iter*0.2)

#############################
### set parameters to monitor
#############################
monitor <- c("centu_mean",
             "sst_month_anom")



###########################
## run JAGS model
###########################
require(R2jags)
out <- jags(data = data_list,
            parameters.to.save = monitor,
            model.file = "sst_century_means_vs_pred_abund.txt",
            n.chains = n.chains,
            n.thin = n.thin,
            n.iter = n.iter,
            n.burnin = n.burn,
            DIC = T)

# Summarize results:
model_summary <- print(out, dig=3) # number of decimals for the output.


# Extract MCMC-type list:
mcmc_list <- as.mcmc(out)





# Create table of results:
results_table_02 <- as.data.frame(rbind(model_summary$summary["centu_mean",]))
results_table_02[, "Parameter"] <- c("SST century mean")



# Save summary as CSV table:
write.csv(model_summary$summary,
          file = "table_results_sst_century_anomalies_gulf_ca.csv")





# Libraries for diagnostics:
require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics

# Plot  posteriors:
length(densityplot(mcmc_list)$panel.args)
# x11()
# densityplot(mcmc_list)



# Add the anomalies:
for (i in seq(1, nrow(mean_sst_df))) {
  mean_sst_df[i, "anom_sst_med"] <- model_summary$summary[paste("sst_month_anom[",i,"]",
                                                                sep=""), "50%"]
  mean_sst_df[i, "anom_sst_mean"] <- model_summary$summary[paste("sst_month_anom[",i,"]",
                                                                sep=""), "mean"]
  mean_sst_df[i, "anom_sst_sd"] <- model_summary$summary[paste("sst_month_anom[",i,"]",
                                                                 sep=""), "sd"]
  mean_sst_df[i, "anom_sst_lwr"] <- model_summary$summary[paste("sst_month_anom[",i,"]",
                                                                sep=""), "2.5%"]
  mean_sst_df[i, "anom_sst_upr"] <- model_summary$summary[paste("sst_month_anom[",i,"]",
                                                                sep=""), "97.5%"]
}

# Condition:
mean_sst_df[mean_sst_df$anom_sst_med >= 0,"condition"] <- "  Warm"
mean_sst_df[mean_sst_df$anom_sst_med < 0,"condition"] <- "  Cold"

# Add month:
mean_sst_df$month <- as.numeric(format(as.Date(mean_sst_df$time, origin= "1970-01-01"), format = "%m"))

# Save century mean:
century_mean <- model_summary$summary["centu_mean",]


require(ggplot2)
require(RColorBrewer)

pers_palette <- c("#ef8a62", # red
                  "#67a9cf") # blue


# Polygon of square:
ylimits_pol <- c(mean(c(-0.75, -1)),
                 mean(c(1.25, 1.5)))
xlimits_pol <- c(as.numeric(unclass(ISOdatetime(
  1978, 7, 1, hour=0, min=0, sec=0, tz="Greenwich")))/(60*60*24),
  as.numeric(unclass(ISOdatetime(
    2019, 7, 1, hour=0, min=0, sec=0, tz="Greenwich")))/(60*60*24)
)
  

poly_square <- data.frame(
  x = c(xlimits_pol[1], xlimits_pol[1],
        xlimits_pol[2], xlimits_pol[2], xlimits_pol[1]),
  y = c(ylimits_pol[1], ylimits_pol[2],
        ylimits_pol[2], ylimits_pol[1], ylimits_pol[1])
)








sst_plot <-
  ggplot()+
  # # Global mean:
  geom_hline(yintercept = 0,
             color = "darkgray",
             size = 0.75)+
  geom_text(aes(x = (-365.25*48),
                y = 0.1),
            label = paste("Century mean: ",
                          round(century_mean["mean"],
                                digits = 2),
                          "°C (",
                          round(century_mean["2.5%"],
                                digits = 2), " - ",
                          round(century_mean["97.5%"],
                                digits = 2), ")",
                          
                          sep = ""),
            color = "darkgray",
            # fontface = "bold",
            hjust = 0,
            size = 3)+
  # Peak of sea lion abundance:
  geom_vline(xintercept = 365.25*21,
             color = "darkgray",
             size = 0.65)+
  geom_label(aes(x = 365.25*21,
                 y = -0.575),
             label = paste("California sea lion\nmaximum", 
                           # round(max(sum_count_predict$med_count,
                           #           na.rm = T),
                           #       digits = 0),
                           # " animals",
                           sep = ""),
             color = "darkgray",
             fill = "white",
             # fontface = "bold",
             hjust = 0.5,
             size = 3)+
  # Anomalies:
  geom_errorbar(data = mean_sst_df,
                aes(x = time,
                    ymin = anom_sst_lwr,
                    ymax = anom_sst_upr,
                    color = condition),
                size = 0.05,
                alpha = 0.4)+
  geom_point(data = mean_sst_df,
            aes(x = time,
                y = anom_sst_med,
                color = condition),
            size = 0.25)+
  scale_color_brewer(palette = "Set1",
                     direction = -1,
                     guide = guide_legend(override.aes =
                                            list(size = 1)))+
  # # Squared polygon:
  # geom_polygon(data = poly_square,
  #              aes(x = x,
  #                  y = y),
  #              color = "black",
  #              fill = NA,
  #              size = 0.5)+
  # Settings:
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 9),
          axis.ticks.length = unit(.2, "cm"),
          axis.title = element_text(size=10),

          legend.position = c(0.1 , 0.85),  # c(left, bottom)
          legend.text = element_text(size = 8.5),
          # legend.key.height = unit(0.5,"cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill = alpha('white', 1),
                                           color = "black",
                                           size = 0.2),

          panel.grid = (element_line(size=0.25)),
          plot.margin = unit(c(0.2, 0.45, 10.25, 0.075), units="cm") #(top, right, bottom, and left distance from the device borders))
    )+
  scale_x_continuous(limits = c(min_jul_des_sst, max_jul_des_sst),
                     breaks = seq(min_jul_des_sst, max_jul_des_sst, 
                                  365.25*10),
                     labels = seq(1920, 2020, 10),
                     expand = c(0, 0))+
  scale_y_continuous(limits = ylimits_pol,
                     breaks = seq(-1, 1.5, 0.25),
                     expand = c(0, 0))+
  labs(x="",
       y = expression(paste(SST[ICE]," anomaly [25-yr running means] (°C)",
                            sep = "")))
         

### SAVE AS JOURNAL FIGURE: ###
tiff(filename="Figure_04_Adame_etal_2019_sst_anomaly_vs_abundance_calif_s_lion.tiff",
     width=17.3,height=23.5*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(sst_plot)
dev.off()



# # Cut the period:
# mean_sst_df <- mean_sst_df[which(mean_sst_df$time >= min_jul_des_sst),]

## Plot the SST anomalies vs. the abundances:
# Add the mean anomaly to the abundance dataframe:


# # First add a column with a 3-yr lag:
# mean_sst_df$lagged_yr <- mean_sst_df$year + 3

## For this model, only anomalies from July each year:
mean_sst_df <- mean_sst_df[mean_sst_df$month == 7, ]

uniq_years_sst <- unique(mean_sst_df$year)


for (i in seq(1, nrow(sum_count_predict))) {
  index_yrs <- which(mean_sst_df$year == sum_count_predict$pred_years[i])
  sum_count_predict[i, "med_sst_anom"] <- mean_sst_df$anom_sst_med[index_yrs]
  sum_count_predict[i, "lwr_sst_anom"] <- mean_sst_df$anom_sst_lwr[index_yrs]
  sum_count_predict[i, "upr_sst_anom"] <- mean_sst_df$anom_sst_upr[index_yrs]
  sum_count_predict[i, "mean_sst_anom"] <- mean_sst_df$anom_sst_mean[index_yrs]
  sum_count_predict[i, "sd_sst_anom"] <- mean_sst_df$anom_sst_sd[index_yrs]
}
   


## Model of abundance as function of SST anomalies
## JAGS MODEL ###
sink("model_pred_abund_vs_sst_anom.txt")
cat(
  "model {

     for(i in 1:n_pred_ab) {
        pred_ab[i] ~ dnorm(mu_pred_ab[i], 
                           1/pow(sd_pred_ab[i], 2))                       # Normal likelihood with know SD (previous model)

        log(mu_pred_ab[i]) <- omega*(a0 + a2*pow(sst_anom[i], 2)) +       # Parabola funtion
                              (1 - omega)*(a0 + a1*sst_anom[i] 
                                          + a2*pow(sst_anom[i], 2))       # 2nd-order polynomial

        sst_anom[i] ~ dnorm(mu_sst_anom, 
                            1/pow(sd_sst_anom[i], 2))                     # Normal likelihood with know SD (previous model)         

        err_vec_ab[i] <- pred_ab[i] - mu_pred_ab[i]                       # Vector of errors (for B-R2 estimation)
     }
     
     # Priors:
     a0 ~ dunif(min_a0, max_a0)
     a1 ~ dunif(min_a1, max_a1)
     a2 ~ dunif(min_a2, max_a2)
     omega ~ dunif(0, 1)
     mu_sst_anom ~ dunif(min_mu_sst_anom, max_mu_sst_anom)


     # Bayesian R-squared:
     r_squ_ecol <- pow(sd(mu_pred_ab), 2) / (pow(sd(mu_pred_ab), 2) + (pow(sd(err_vec_ab), 2)))
  

   }",
  fill=TRUE)
sink() # end of JAGS model



data_list <- list(
  n_pred_ab = nrow(sum_count_predict[which(!is.na(sum_count_predict$mean_sst_anom)),]),
  pred_ab = sum_count_predict$mean_count[which(!is.na(sum_count_predict$mean_sst_anom))]/1000,
  sd_pred_ab = sum_count_predict$sd_count[which(!is.na(sum_count_predict$mean_sst_anom))]/1000,
  
  sst_anom = sum_count_predict$mean_sst_anom[which(!is.na(sum_count_predict$mean_sst_anom))],
  sd_sst_anom = sum_count_predict$sd_sst_anom[which(!is.na(sum_count_predict$mean_sst_anom))],
  
  min_a0 = -60, max_a0 = 60,
  min_a1 = -50, max_a1 = 200,
  min_a2 = -150, max_a2 = 100,
  # min_a3 = -100, max_a3 = 100,
  
  min_mu_sst_anom = -1, max_mu_sst_anom = 1
  
)


###########################
## set MCMC controls
############################
n.iter <- 1000000
n.chains <- 3
n.thin <- 20
n.burn <- round(n.iter*0.2)

#############################
### set parameters to monitor
#############################
monitor <- c("a0", 
             "a1",
             "a2",
             "omega",
             # "a3",
             "mu_sst_anom",
             "r_squ_ecol"
             ) #,
             # "mu_sst_anom")

###########################
## run JAGS model
###########################
require(R2jags)
out <- jags(data = data_list,
            parameters.to.save = monitor,
            model.file = "model_pred_abund_vs_sst_anom.txt",
            n.chains = n.chains,
            n.thin = n.thin,
            n.iter = n.iter,
            n.burnin = n.burn,
            DIC = T)



# Summarize results:
model_summary <- print(out, dig=3) # number of decimals for the output.



# Create table of results:
results_table_03 <- as.data.frame(rbind(model_summary$summary["omega",],
                                        model_summary$summary["r_squ_ecol",],
                                        model_summary$summary["mu_sst_anom",],
                                        model_summary$summary["a0",],
                                        model_summary$summary["a1",],
                                        model_summary$summary["a2",]))
results_table_03[, "Parameter"] <- c("Ecologycal model\nmixing parameter",
                                     "Ecologycal model\nexplained variance",
                                     "Mean SST anomaly\nduring the sea lion\nstudy period",
                                     "Ecologycal model intercept",
                                     "Ecologycal model first coefficient",
                                     "Ecologycal model second coefficient")


# Save summary as CSV table:
write.csv(model_summary$summary,
          file = "table_results_calif_sea_lion_pop_size_vs_sst_cent_anom.csv")


# Extract MCMC-type list:
mcmc_list <- as.mcmc(out)



# Libraries for diagnostics:
require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics

# Plot  posteriors:
length(densityplot(mcmc_list)$panel.args)
x11()
densityplot(mcmc_list)








# Create the prediction data.frame
pred_ab_sst_anom <- data.frame(
  sst_anom_vec = seq(-0.75, mean(c(1.25, 1.5)),
                     length.out = 500)
)
pred_ab_sst_anom$med_abun <- exp(
  model_summary$summary["omega","50%"]*(model_summary$summary["a0","50%"] +
                                          model_summary$summary["a2","50%"]*(pred_ab_sst_anom$sst_anom_vec^2)) +
    (1 -  model_summary$summary["omega","50%"])*(model_summary$summary["a0","50%"] +
                                                   model_summary$summary["a1","50%"]*pred_ab_sst_anom$sst_anom_vec +
                                                   model_summary$summary["a2","50%"]*(pred_ab_sst_anom$sst_anom_vec^2))
  )*1000


pred_ab_sst_anom$lwr_abun <- exp(
  model_summary$summary["omega","2.5%"]*(model_summary$summary["a0","2.5%"] +
                                          model_summary$summary["a2","2.5%"]*(pred_ab_sst_anom$sst_anom_vec^2)) +
    (1 -  model_summary$summary["omega","2.5%"])*(model_summary$summary["a0","2.5%"] +
                                                   model_summary$summary["a1","2.5%"]*pred_ab_sst_anom$sst_anom_vec +
                                                   model_summary$summary["a2","2.5%"]*(pred_ab_sst_anom$sst_anom_vec^2))
  )*1000


pred_ab_sst_anom$upr_abun <- exp(
  model_summary$summary["omega","97.5%"]*(model_summary$summary["a0","97.5%"] +
                                           model_summary$summary["a2","97.5%"]*(pred_ab_sst_anom$sst_anom_vec^2)) +
    (1 -  model_summary$summary["omega","97.5%"])*(model_summary$summary["a0","97.5%"] +
                                                    model_summary$summary["a1","97.5%"]*pred_ab_sst_anom$sst_anom_vec +
                                                    model_summary$summary["a2","97.5%"]*(pred_ab_sst_anom$sst_anom_vec^2))
  )*1000

             
# Save Bayesian R squared:
r_squ_ecol <- model_summary$summary["r_squ_ecol", ]


# Identify the optimum habitat range:
index_opt_df <- seq(which.max(pred_ab_sst_anom$lwr_abun),
                    which.max(pred_ab_sst_anom$upr_abun))

opt_hab_df <- pred_ab_sst_anom[index_opt_df,]

# Optimum anomaly:
optim_anom_sst <- round(c(min(opt_hab_df$sst_anom_vec),
                          max(opt_hab_df$sst_anom_vec)),
                        digits = 2)



require(extrafont)
# font_import()
loadfonts(device = "win")


gc()
plot_sst_anom_vs_abund <-
  ggplot()+
  # Model prediction:
  geom_ribbon(data = pred_ab_sst_anom,
              aes(x = sst_anom_vec,
                  ymin = lwr_abun/1000,
                  ymax = upr_abun/1000),
              fill = "#4daf4a",
              color = NA,
              alpha = 0.3)+
  geom_path(data = pred_ab_sst_anom,
            aes(x = sst_anom_vec,
                y = med_abun/1000),
            color = "#4daf4a",
            size = 0.2)+
  
  # Optimum habitat:
  geom_ribbon(data = opt_hab_df,
              aes(x = sst_anom_vec,
                  ymin = lwr_abun/1000,
                  ymax = upr_abun/1000),
              fill = "#4daf4a",
              color = NA,
              alpha = 0.75)+
  geom_path(data = opt_hab_df,
            aes(x = sst_anom_vec,
                y = med_abun/1000),
            color = "#4daf4a",
            size = 0.55)+
  
  # "Observations":
  geom_errorbar(data = sum_count_predict,
                aes(x = med_sst_anom,
                    ymin = lwr_count/1000,
                    ymax = upr_count/1000),
                alpha = 0.25,
                size = 0.25)+
  geom_errorbarh(data = sum_count_predict,
                 aes(y = med_count/1000,
                     xmin = lwr_sst_anom,
                     xmax = upr_sst_anom),
                 alpha = 0.2,
                 size = 0.3)+
  geom_point(data = sum_count_predict,
             aes(x = med_sst_anom,
                 y = med_count/1000),
             shape = 21,
             fill = "white",
             color = "black",
             stroke = 0.5,
             size = 0.4)+
  # The Bayesian R squared:
  geom_text(aes(x = 0.16,
                y = 7),
            fontface = "italic",
            family = "Times New Roman",
            color = "#4daf4a",
            label = paste("italic(BR)^{2}==",
                          round(r_squ_ecol["50%"],
                                digits = 3),
                          "~(",
                          round(r_squ_ecol["2.5%"],
                                     digits = 3),
                          " - ", 
                          round(r_squ_ecol["97.5%"],
                                digits = 3),
                          ")",
                          sep = ""),
            parse = TRUE,
            size = 2.25)+
  # Formula:
  geom_text(aes(x = 0.16,
                y = 13),
            fontface = "italic",
            family = "Times New Roman",
            color = "#4daf4a",
            label = "italic(log(y)==(1-omega)*(alpha[0]+alpha[1]*x+alpha[2]*x^{2}) + 
                      omega*(alpha[0]+alpha[2]*x^{2}))",
            parse = TRUE,
            size = 2.25
            )+
  

  # Settings:
  theme_bw()+
  theme(axis.text = element_text(size = 5),
        axis.ticks.length = unit(.1, "cm"),
        axis.title = element_text(size=7),
        panel.grid = (element_line(size=0.1)),
        plot.margin = unit(c(0.2, 0.05, 15.5, 0.05), units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  labs(x = expression(paste(SST[ICE]," anomaly [25-yr running means] (°C)",
                            sep = "")),
       y = "Population size (ind. x 1000)")+
  scale_x_continuous(breaks = seq(-1, 1.5, 0.25),
                     expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10),
                     expand = c(0, 0))

### SAVE AS JOURNAL FIGURE: ###
tiff(filename="Figure_05_Adame_etal_2019_sst_anomaly_vs_abundance_calif_s_lion.tiff",
     width=8.2,height=26.3*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(plot_sst_anom_vs_abund)
dev.off()










# Add the predictions:
mean_sst_df$predic_sst_abund_med <- exp(
  model_summary$summary["omega","50%"]*(model_summary$summary["a0","50%"] +
                                          model_summary$summary["a2","50%"]*(mean_sst_df$anom_sst_med^2)) +
    (1 -  model_summary$summary["omega","50%"])*(model_summary$summary["a0","50%"] +
                                                   model_summary$summary["a1","50%"]*mean_sst_df$anom_sst_med +
                                                   model_summary$summary["a2","50%"]*(mean_sst_df$anom_sst_med^2))
  )*1000


mean_sst_df$predic_sst_abund_lwr <- exp(
  model_summary$summary["omega","2.5%"]*(model_summary$summary["a0","2.5%"] +
                                           model_summary$summary["a2","2.5%"]*(mean_sst_df$anom_sst_med^2)) +
    (1 -  model_summary$summary["omega","2.5%"])*(model_summary$summary["a0","2.5%"] +
                                                    model_summary$summary["a1","2.5%"]*mean_sst_df$anom_sst_med +
                                                    model_summary$summary["a2","2.5%"]*(mean_sst_df$anom_sst_med^2))
)*1000


mean_sst_df$predic_sst_abund_upr <- exp(
  model_summary$summary["omega","97.5%"]*(model_summary$summary["a0","97.5%"] +
                                            model_summary$summary["a2","97.5%"]*(mean_sst_df$anom_sst_med^2)) +
    (1 -  model_summary$summary["omega","97.5%"])*(model_summary$summary["a0","97.5%"] +
                                                     model_summary$summary["a1","97.5%"]*mean_sst_df$anom_sst_med +
                                                     model_summary$summary["a2","97.5%"]*(mean_sst_df$anom_sst_med^2))
)*1000




# Cut limits of time:
mean_sst_df <- mean_sst_df[which(mean_sst_df$year >= min(abun_area_df$year) &
                                   mean_sst_df$year <= max(abun_area_df$year)),]
sum_count_predict <- sum_count_predict[which(sum_count_predict$pred_years >= min(abun_area_df$year) &
                                         sum_count_predict$pred_years <= max(abun_area_df$year)),]




require(ggplot2)
figure_sum_gulf <- 
  ggplot()+
  # Predicted by SST anomalies:
  geom_ribbon(data = mean_sst_df,
              aes(x = year,
                  ymin = predic_sst_abund_lwr,
                  ymax = predic_sst_abund_upr),
              fill = "#4daf4a",
              alpha = 0.25,
              color = NA)+
  geom_path(data = mean_sst_df,
            aes(x = year,
                y = predic_sst_abund_med),
            color = "#4daf4a",
            # linetype = "dashed",
            size = 0.5)+
  # Divide decades:
  geom_vline(xintercept = seq(1970, 2020, 10),
             color = "darkgray",
             size = 0.175)+
  # Rug lines with counts per rookery:
  geom_rug(data = abun_area_df,
           aes(x = year),
           size = 1)+
  # Bayesian results:
  geom_errorbar(data = sum_count_predict,
                aes(x = pred_years,
                    ymin = lwr_count,
                    ymax = upr_count),
                color = "black",
                size = 0.35)+
  geom_point(data = sum_count_predict,
             aes(x = pred_years,
                 y = med_count),
             shape = 21,
             color = "white",
             fill = "black",
             size = 1.65,
             stroke = 0.35)+
  # Decadal changes:
  geom_label(data = decad_change_df,
             aes(x = x_location,
                 y = 61500,
                 label = pred_change),
             # fontface = "bold",
             color = "darkgray",
             size = 2.5,
             hjust = 0.5,
             vjust = 0.5)+
  # 30-year decline:
  geom_label(aes(x = 2012,
                 y = 47500,
                 label = three_dedc_decline),
             fontface = "bold",
             color = "#984ea3",
             size = 3.2,
             hjust = 0.5,
             vjust = 0.5)+
  # Text inferred abundance:
  geom_label(aes(x = 1985,
                 y = 25000),
             label = "Numerical\nreconstruction",
             # fontface = "bold",
             color = "black",
             size = 3.2,
             hjust = 0.5,
             vjust = 0.5)+
  geom_label(aes(x = 1998,
                 y = 20000),
             label = expression(atop(Predicted~from~SST[ICE],
                                     paste("century anomalies",
                                           sep = ""))),
             # fontface = "bold",
             color = "#4daf4a",
             size = 3.2,
             hjust = 0.5,
             vjust = 0.5)+
  
  guides(fill=guide_legend(ncol=1))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 9),
        axis.ticks.length = unit(.2, "cm"),
        axis.title = element_text(size=11),
        
        legend.position = c(0.2 , 0.25),  # c(left, bottom)
        legend.text = element_text(size = 8.5),
        # legend.key.height = unit(0.5,"cm"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('white', 1),
                                         color = "black",
                                         size = 0.2),
        
        panel.grid = (element_line(size=0.2)),
        plot.margin = unit(c(0.2, 0.5, 10, 0), units="cm") #(top, right, bottom, and left distance from the device borders))
  )+
  scale_x_continuous(limits = c(min(abun_area_df$year)-1,
                                max(abun_area_df$year)+1),
                     breaks = seq(1970, 2020, 5),
                     expand = c(0, 0))+
  scale_y_continuous(limits = c(5000, 65000),
                     breaks = seq(5000, 65000, 5000),
                     labels = seq(5000, 65000, 5000)/1000,
                     expand = c(0, 0))+
  labs(x="",y="Population size (ind. x 1000)")+
  coord_fixed(ratio = 0.00035)


### SAVE AS JOURNAL FIGURE: ###
tiff(filename="Figure_03_Adame_etal_2019_trend_sum_pop_size.tiff",
     width=17.3,height=23.5*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(figure_sum_gulf)
dev.off()













#### Map of rookeries

# Sort by latitude
point_rookeries <- point_rookeries[order(-point_rookeries$lat),]

# Assign numbers:
point_rookeries$rook_number <- as.character(seq(1,nrow(point_rookeries)))


# New names:
point_rookeries$new_names <- paste(point_rookeries$rook_number,
                                   ". ",
                                   point_rookeries$rook_name,
                                   sep = "")


# Coord limits for maps:
lim_lon <- c(-115.5 , -107)
lim_lat <- c(22.5, 32.5)




# Packages to avoid problems with polygon permits:
require(rgeos) 
require(rgdal)


# Extract land from world database:
### COAST LINES AND BORDERS from GSHHS database: ###
# BINARY files obtained from: 
# http://www.soest.hawaii.edu/pwessel/gshhg/
# The ZIP file HAS to be extracted into one of R's
# packages. In this case, just for convenience, into 
# the ggplot2's folder (i.e. Documents/R/win-library/2.15/ggplot2)

land_path <- system.file("gshhs_f.b",package = "ggplot2")


# 360 Limits: 
x_limits <- c(360+lim_lon[1], 360+lim_lon[2])
y_limits <- c(lim_lat[1],lim_lat[2])

# Extract coast line:
require(maptools)
land_polyg <- getRgshhsMap(land_path,
                           xlim = c(x_limits[1] + 0.01,x_limits[2] - 0.01),
                           ylim = c(y_limits[1] + 0.01,y_limits[2] - 0.01),
                           level = 1)


# Fortify to convert into dataframe:
require(ggplot2)
land_polyg <- fortify(land_polyg)


# New coordinates for rookery labels:
point_rookeries$lon_lab <- rep(NA, nrow(point_rookeries))
point_rookeries$lat_lab <- rep(NA, nrow(point_rookeries))



# In lon:
point_rookeries$lon_lab[1] <- point_rookeries$lon[1] + 1.2
point_rookeries$lon_lab[2] <- point_rookeries$lon[2] + 1.3
point_rookeries$lon_lab[3] <- point_rookeries$lon[3] - 0.175
point_rookeries$lon_lab[4] <- point_rookeries$lon[4] + 2
point_rookeries$lon_lab[5] <- point_rookeries$lon[5] + 2
point_rookeries$lon_lab[6] <- point_rookeries$lon[6] - 0.75
point_rookeries$lon_lab[7] <- point_rookeries$lon[7] + 1.8
point_rookeries$lon_lab[8] <- point_rookeries$lon[8] - 1
point_rookeries$lon_lab[9] <- point_rookeries$lon[9] + 2
point_rookeries$lon_lab[10] <- point_rookeries$lon[10] - 1.3
point_rookeries$lon_lab[11] <- point_rookeries$lon[11] + 1.75
point_rookeries$lon_lab[12] <- point_rookeries$lon[12] + 1.25
point_rookeries$lon_lab[13] <- point_rookeries$lon[13] - 1.5

# In lat:
point_rookeries$lat_lab[1] <- point_rookeries$lat[1] + 0.75
point_rookeries$lat_lab[2] <- point_rookeries$lat[2]
point_rookeries$lat_lab[3] <- point_rookeries$lat[3] - 0.5
point_rookeries$lat_lab[4] <- point_rookeries$lat[4] + .7
point_rookeries$lat_lab[5] <- point_rookeries$lat[5] + 0.3
point_rookeries$lat_lab[6] <- point_rookeries$lat[6] - 0.4
point_rookeries$lat_lab[7] <- point_rookeries$lat[7] + 0.28
point_rookeries$lat_lab[8] <- point_rookeries$lat[8] - 0.375
point_rookeries$lat_lab[9] <- point_rookeries$lat[9] + 0
point_rookeries$lat_lab[10] <- point_rookeries$lat[10] - 1
point_rookeries$lat_lab[11] <- point_rookeries$lat[11] 
point_rookeries$lat_lab[12] <- point_rookeries$lat[12] + 1
point_rookeries$lat_lab[13] <- point_rookeries$lat[13] + 0.3


# New dataframe to correct visual overlap among points:
df_visual_olp <- point_rookeries[which(point_rookeries$rook_number == 4 |
                                         point_rookeries$rook_number == 5 |
                                         point_rookeries$rook_number == 7),]




require(scales)
require(ggsn)


# Simplify DF.
sst_df <- sst_df[which(sst_df$time == min(sst_df$time)),]



gulf_rookeries_map <- 
  ggplot()+
  
  # Map-projection:
  coord_map(projection="mercator")+
  
  # SST measurements:
  geom_point(data = sst_df,
             aes(x = lon,
                 y = lat),
             shape = 8,
             size = 1.5,
             color = "#4daf4a",
             # fill = NA,
             stroke = 0.6)+
  
  # Land:
  geom_polygon(data = land_polyg,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "lightgray",
               color = NA)+
  
  # Lines between labels and points:
  geom_segment(data = point_rookeries,
               aes(x = lon,
                   y = lat,
                   xend = lon_lab, 
                   yend = lat_lab), 
               color = "black",
               size = 0.2)+
  
  # Points at rookeries:
  geom_point(data = point_rookeries,
             aes(x = lon,
                 y = lat),
             color = "white",
             fill = "black",
             size = 1.5,
             pch = 21,
             stroke = 0.3)+
  # Labels:
  geom_label(data = point_rookeries,
             aes(x = lon_lab,
                 y = lat_lab,
                 label = new_names),
             # fontface = "bold",
             hjust = 0.5,
             vjust = 0.5,
             size = 1.8,
             color = "black",
             label.size = 0.2,
             label.padding = unit(0.14, "lines")) +
  
  # Places:
  geom_text(aes(x = -111,
                y = 27),
            label = "Gulf of\nCalifornia",
            fontface = "italic",
            size = 2,
            color = "darkgray")+
  geom_text(aes(x = -114,
                y = 25),
            label = "NORTHEAST\nPACIFIC\nOCEAN",
            fontface = "italic",
            size = 2.5,
            color = "darkgray")+
  geom_text(aes(x = -108.5,
                y = 29),
            label = "MEXICO",
            fontface = "italic",
            size = 2.5,
            color = "darkgray")+
  
  # Plot settings:
  labs(x="", y="") +
  
  # Settings:
  theme_bw()+
  theme(
    panel.border = element_rect(colour = "black",
                                fill = NA,
                                size = 0.5),
    panel.grid = element_line(size = 0.2),
    axis.text=element_text(size = 6),
    plot.margin=unit(c(0.1, 0.35, -0.4, -0.3),
                     units="cm"))+ #(top, right, bottom, and left distance from the device borders))
  scale_x_continuous(limits=lim_lon,
                     breaks=seq(-115, -107, 2),
                     labels=paste(as.character(abs(seq(-115, -107, 2))),
                                  "°W",sep=""),
                     expand=c(0,0))+
  scale_y_continuous(limits=lim_lat,
                     breaks=seq(22, 33, 2),
                     labels=paste(as.character(seq(22, 33, 2)),
                                  "°N",sep=""),
                     expand=c(0,0))+
  
  # Scalebar:
  scalebar(dist = 100,
           dist_unit = "km",
           location = "bottomleft",
           border.size = 0.2,
           height = 0.013,
           st.size = 1.5,
           transform = TRUE,
           model = 'WGS84',
           x.min = lim_lon[1]+0.3,
           x.max = lim_lon[2],
           y.min = lim_lat[1]+0.4,
           y.max = lim_lat[2])


# ### SAVE AS JOURNAL FIGURE: ###
# tiff(filename="gc_rookeries.tiff",
#      width=8.2,height=26.3*0.8,units="cm",
#      bg="white",
#      res=600,
#      compression = c("lzw"))
# 
# print(gulf_rookeries_map)
# dev.off()





## Inset map:


poly_are_shown <- data.frame(
  lon = c(lim_lon[1], lim_lon[1], lim_lon[2], lim_lon[2], lim_lon[1]),
  lat = c(lim_lat[1], lim_lat[2], lim_lat[2], lim_lat[1], lim_lat[1])
)

# Coord limits:
lim_lon_inset <- c(-115.5-18 , -107+60)
lim_lat_inset <- c(22.5-40, 32.5+25)

land_path_inset <- system.file("gshhs_l.b",package = "ggplot2")


# 360 Limits: 
x_limits_inset <- c(360+lim_lon_inset[1], 360+lim_lon_inset[2])
y_limits_inset <- c(lim_lat_inset[1], lim_lat_inset[2])

land_polyg_inset <- getRgshhsMap(land_path_inset,
                                 xlim = c(x_limits_inset[1] + 0.01,x_limits_inset[2] - 0.01),
                                 ylim = c(y_limits_inset[1] + 0.01, y_limits_inset[2] - 0.01),
                                 level = 1)

# Fortify to convert into dataframe:
land_polyg_inset <- fortify(land_polyg_inset)


gulf_rookeries_inset <- 
  ggplot()+
  
  # Map-projection:
  coord_map(projection="mercator")+
  
  # Land:
  geom_polygon(data = land_polyg_inset,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "lightgray",
               color = NA)+
  
  # Area shown:
  geom_polygon(data = poly_are_shown,
               aes(x = lon,
                   y = lat),
               color = "black",
               size = 0.5,
               fill = NA)+
  
  # Oceans:
  geom_text(aes(x = -66,
                y = 26.9),
            color = "darkgray",
            label = "ATLANTIC\nOCEAN",
            size = 1.3,
            fontface = "italic")+
  geom_text(aes(x = -105.1,
                y = -0.36),
            color = "darkgray",
            label = "PACIFIC\nOCEAN",
            size = 1.3,
            fontface = "italic")+
  
  # Continent:
  geom_text(aes(x = -98,
                y = 39),
            color = "darkgray",
            label = "NORTH\nAMERICA",
            size = 1.3,
            fontface = "italic")+
  geom_text(aes(x = -66.2,
                y = -2.33),
            color = "darkgray",
            label = "SOUTH\nAMERICA",
            size = 1.3,
            fontface = "italic")+
  

  # # Places:
  # geom_text(aes(x = -111,
  #               y = 27),
  #           label = "Gulf of\nCalifornia",
  #           fontface = "italic",
  #           size = 2,
  #           color = "darkgray")+
  
  # Plot settings:
  labs(x="", y="") +
  
  # Settings:
  theme_bw()+
  theme(
    panel.border = element_rect(colour = "black",
                                fill = NA,
                                size = 0.4),
    panel.grid = element_line(size = 0.05),
    axis.text = element_blank(),
    plot.background = element_rect(fill = NA,
                                   color = NA))+
    # plot.margin=unit(c(0.1, 0.35, -0.4, -0.3),
    #                  units="cm"))+ #(top, right, bottom, and left distance from the device borders))
  scale_x_continuous(limits = lim_lon_inset,
                     breaks = NULL,
                     expand=c(0,0))+
  scale_y_continuous(limits = lim_lat_inset,
                     breaks = NULL,
                     expand=c(0,0))



# ### SAVE AS JOURNAL FIGURE: ###
# tiff(filename="inset_rookeries.tiff",
#      width=8.2,height=26.3*0.8,units="cm",
#      bg="white",
#      res=600,
#      compression = c("lzw"))
# 
# print(gulf_rookeries_inset)
# dev.off()











# Layout:
grid.newpage()
library(grDevices)

### Layout and file: 
### JOurnal: Global Change Biology
tiff(filename="Figure_01_Adame_etal_2019_map_gulf_ca_rook_w_inset.tiff",
     width=8.2,height=26.3*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))


vpb <- viewport(width = 1, height = 1,
                x = 0.5, y = 0.5)  # the larger map
vpa <- viewport(width = 0.4, height = 0.4,
                x = 0.76, y = 0.655)  # the inset

print(gulf_rookeries_map,
      vp = vpb)
print(gulf_rookeries_inset,
      vp = vpa)

dev.off()


### Table of results (final):
final_table_results <- rbind(results_table_01,
                             results_table_02,
                             results_table_03)
# Round to 3 digits:
final_table_results[,1:9] <- round(final_table_results[,1:9], digits = 3)

# Add the percentage of n.eff
final_table_results$percent_n_eff <- round((final_table_results$n.eff/(((n.iter-n.burn)/n.thin)*n.chains))*100,
                                           digits = 1)


# Save summary as CSV table:
write.csv(final_table_results,
          file = "table_results_calif_sea_lion_all_paper.csv")




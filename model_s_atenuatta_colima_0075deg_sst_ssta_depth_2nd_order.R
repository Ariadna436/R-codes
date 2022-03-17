## Habitat models Stenella attenuata ##


#### Clean everything: ####.
rm(list=ls())  # clear all;
graphics.off() # close all;
gc()           # clear cache.. (?)


##### IMPORT AND FILTER DATA ######

# Call sightings database:
orig_sight_data <- read.csv("sighting_data_PCM_2010-2014_w_transect.csv")

# Chose species:
sp_ind <- which(orig_sight_data$sp_name == "Pantropical spotted dolphin" |
                  orig_sight_data$sp_name == "Pantropical spotted dolphin (offshore)" |
                  orig_sight_data$sp_name == "Pantropical spotted dolphin (coastal)")
orig_sight_data <- orig_sight_data[sp_ind,]
nona_gp_size <- which(!is.na(orig_sight_data$av_gp_size))
orig_sight_data_wgpsize <- orig_sight_data[nona_gp_size,]

# Index of sightings WITH perpendicular distance and beaufort (AND platform height)
nona_pdist_beauf_ph <- which(!is.na(orig_sight_data$perp.dist) &
                               !is.na(orig_sight_data$beaufort) &
                               !is.na(orig_sight_data$platf_height_m) &
                               !is.na(orig_sight_data$av_gp_size))
orig_sight_data_wcov <- orig_sight_data[nona_pdist_beauf_ph,]


# Call data for effort, sightings, and environmental variables


transect_data <- read.csv("transect_data_PCM_2010-2014_5km.csv")

# Index of cells with enviromental variable data
nona_ssta_cell <- which(!is.na(transect_data$ssta_spa))
transect_data <- transect_data[nona_ssta_cell,]
nona_depth_cell <- which(!is.na(transect_data$depth))
transect_data <- transect_data[nona_depth_cell,]
nona_sst_cell <- which(!is.na(transect_data$sst))
transect_data <- transect_data[nona_sst_cell,]


# Previous information on g(0) from Barlow & Forney (2007):
##### See Gelman et al. (2004) p. 582:
mean_barlow_g0 <- 0.970
cv_barlow_g0 <- 0.017
var_barlow_g0 <- (cv_barlow_g0*mean_barlow_g0)^2
# For the Beta distribution (McCarthy 2007 or Gelman 2004):
a_g0_prior <- mean_barlow_g0*(mean_barlow_g0*(1-mean_barlow_g0)/var_barlow_g0-1)
b_g0_prior <- (1-mean_barlow_g0)*(mean_barlow_g0*(1-mean_barlow_g0)/var_barlow_g0-1)


#### JAGS model ###
###Text file to save the model: ###
sink("bayes_density_s_atenuatta_colima_2nd_transect_sst_ssta_depth.txt")

#### MODEL IN BUGS LANGUAJE: ####
cat("model {
    
    ##### OBSERVATION PROCESS MODELS #####
    
    #### Models from sightings (j): ####
    
    ### Predicted group size model: ###
    
    # Priors:
    logmu_gs ~ dunif(min_logmu_gs,max_logmu_gs)       # Mean group size model for cells
    logsd_gs ~ dunif(min_logsd_gs,max_logsd_gs)       # Mean group size model for cells
    logvar_gs <- log(1+pow(logsd_gs/logmu_gs,2))      # Mean group size model for cells (the variance in the log space)
    logtau_gs <- 1/logvar_gs                          # Mean group size model for cells (the precision)
    
    # Likelihoods:
    for (j in 1:n_sigh) {
    # Likelihood of the group sizes at sightings:
    gp_sz_sigh[j] ~ dlnorm(logmu_gs,logtau_gs)                               # Observed group sizes are assumed log-normally distributed
    }   
    
    # Mean group size (derived parameter):
    mean_gs <- exp(logmu_gs+0.5 * logvar_gs)          # Mean group size fitted from sightings data
    
    
    ### ESW model from sightings data (for the species): ###
    
    # Priors: 
    a0 ~ dunif(min_a0,max_a0)                                  # Half-effective strip width model
    a1 ~ dunif(min_a1,max_a1)                                  # Half-effective strip width model
    a2 ~ dunif(min_a2,max_a2)  
    a3 ~ dunif(min_a3,max_a3)                                  # Half-effective strip width model
    sd_eps_a ~ dunif(min_sd_eps_a, max_sd_eps_a)               # Half-effective strip width model 
    eps_a_tau <- 1/(sd_eps_a*sd_eps_a)                         # Half-effective strip width model (residuals sightings)
    
    # Likelihoods:
    for (h in 1:n_sigh_w_cov) {
    perp_dist_sigh[h] ~ dnorm(0,perp_dist_tau[h])                                                      # Perpendicular distances are assumed half-normally distributed
    perp_dist_tau[h] <- 1/(pow(w_sigh[h],2)*(2/pi))                                                    # The precision of perpendicular distances' distribution defines the ESW          
    w_sigh[h] <- exp(a0+(a1 * beaufort[h])+
                        (a2 * gp_sz_sigh_wcov[h])+
                        (a3 * platf_height_m)+
                       eps_a[h]) # ESW model from sightings: the ESW varies with Beaufort and log-group size
    
    eps_a[h] ~ dnorm(0,eps_a_tau)                                                                      # Residuals of the ESW model are assumed normally distributed
    }
    
    
    
    #### Models from cells with predictors (i): ####
    
    # Likelihoods:
    for (i in 1:n_cells){
    
    # ESW model for cells:
    w_cell[i] <- exp(a0+(a1*interp_beauf[i])+
                    (a3*platf_height_cell[i])+
                    (a2*mean_gs)+
                    eps_a2[i])     # ESW model, taking parameters from sightings data

    eps_a2[i] ~ dnorm(0,eps_a2_tau)                                                              # ESW residuals are assumed normally distributed
    
    # Model of latent group counts in each cell:
    n_groups_cell[i] ~ dpois(pred_gp[i])                                            # Group counts at cells are assumed Poisson distributed
    pred_gp[i] <- (2*w_cell[i]*eff_cell[i]*dens_cell[i]*g0_barlow)/mean_gs          # Latent group counts are densodependent
    
    # Group counts model check (sums-of-squares type discrepancy):
    squared_res_obs_gp_counts[i] <- pow(n_groups_cell[i]-pred_gp[i],2)                    # Squared residuals for observed data
    new_n_groups_cell[i] ~ dpois(pred_gp[i])                                              # Replicate (one new data set at each MCMC iteration)
    new_squared_res_gp_counts[i] <- pow(new_n_groups_cell[i] - pred_gp[i], 2)             # Squared residuals for new data
    
    
   

     ### ECOLOGICAL PROCESS MODELS ###
    
   
      # # quadratic polynom 3 covariate (sst_mur + ssta  + depth):

          dens_cell[i] <- exp(s0+(s1 * pow(sst_cells[i],2))+
                                         (s2 * pow(ssta_cells[i],2))+
                                          (s3 * pow(depth_cells[i],2))+
                                          (s4 * sst_cells[i])+
                                           (s5 * ssta_cells[i])+
                                            (s6 * depth_cells[i])+
                                              eps_s[i])
        
  
      
    # Likelihood of residuals
       eps_s[i] ~ dnorm(0, 1/pow(sd_eps_s,2))
       
   
   }
    
    # Priors:
    sd_eps_a2 ~ dunif(min_sd_eps_a2,max_sd_eps_a2)             # Half-effective strip width model
    eps_a2_tau <- 1/(sd_eps_a2*sd_eps_a2)                      # Half-effective strip width model (residuals cells)
    
    g0_barlow ~ dbeta(a_g0,b_g0)
    
    
    
    ####Prior ecological model ####
    
    ## SST + Depth density model ##
    s0 ~ dunif(min_s0,max_s0)                              
    s1 ~ dunif(min_s1,max_s1)                               
    s2 ~ dunif(min_s2,max_s2)                              
    s3 ~ dunif(min_s3,max_s3)                               
    s4 ~ dunif(min_s4,max_s4) 
    s5 ~ dunif(min_s5,max_s5)                               
    s6 ~ dunif(min_s6,max_s6) 
    sd_eps_s ~ dunif(min_sd_eps_s,max_sd_eps_s)              
    
    
    #### POSTERIOR PREDICTIVE CHECKS ####
    # Bayesian p-value for group counts model:
    fit_obs_counts <- sum(squared_res_obs_gp_counts[])      # Sum of squared residuals for actual dataset
    fit_new_counts <- sum(new_squared_res_gp_counts[])      # Sum of squared residuals for replicated dataset
    test_fit_counts <- step(fit_new_counts-fit_obs_counts)  # Test whether new data set is more extreme
    b_p_value_counts <- mean(test_fit_counts)               # Bayesian p-value (0.5 expected)
    
    
    }", 
    fill=TRUE)
sink()

#### Data list ####

data_list <- list(
  # For priors:
  a_g0 = a_g0_prior,
  b_g0 = b_g0_prior,
  
  min_a0 = -2, max_a0 = 0,
  min_a1 = -0.8, max_a1= 0,
  min_a2 = 0, max_a2= 0.07,
  min_a3 = 0, max_a3= 10,
  min_sd_eps_a = 0, max_sd_eps_a = 2,
  min_sd_eps_a2 = 0, max_sd_eps_a2 = 2,
  
  min_logmu_gs = 1.5, max_logmu_gs = 2.5,
  min_logsd_gs = 2, max_logsd_gs = 4.5,
  
  min_s0 = -6, max_s0 = 0,
  min_s1 = 0, max_s1 = 60,
  min_s2 = -10, max_s2 = 100,
  min_s3 = -10, max_s3 =10,
  min_s4 = -10, max_s4 = 20,
  min_s5 = -10,max_s5 = 10 ,                               
  min_s6 = -10,max_s6 = 50,
 
  min_sd_eps_s = 0.01, max_sd_eps_s=2.5,
  
  
  # For g. size:
  n_sigh = nrow(orig_sight_data_wgpsize),
  gp_sz_sigh = orig_sight_data_wgpsize$av_gp_size,
  
  # For sightings ESW:
  n_sigh_w_cov = nrow(orig_sight_data_wcov),
  perp_dist_sigh = orig_sight_data_wcov$perp.dist,
  beaufort = orig_sight_data_wcov$beaufort,
  platf_height_m = orig_sight_data_wcov$platf_height_m,
  gp_sz_sigh_wcov = orig_sight_data_wcov$av_gp_size,
  pi = 3.141593,
  
  # For cells ESW:
  n_cells = nrow(transect_data),
  interp_beauf = transect_data$interp_beauf,
  platf_height_cell = transect_data$interp_platf_h,
  
  # For group counts model:
  n_groups_cell = transect_data$Pant_groups,
  eff_cell = transect_data$dist_vec,
  
  # For ecological process:
  
  depth_cells = (transect_data$depth - mean(transect_data$depth,na.rm=T))/1000,
  ssta_cells = (transect_data$ssta_spa)/10,
  sst_cells = (transect_data$sst - mean(transect_data$sst,na.rm=T))/10
)

####Parametros a monitorear####
params <- c(
  "a0","a1",
   "a2","a3",
  "sd_eps_a",
  "sd_eps_a2",
  "mean_gs",
  "logmu_gs",
  "logsd_gs",
  "g0_barlow",
  "s0","s1",
  "s2","s3","s4",
  "s5","s6",
  "sd_eps_s",
  "b_p_value_counts",
  "test_fit_counts"
)

#### Parameters for the MCMCs:####
n_iter <- 100000     # iterations
n_burn <- n_iter*0.1      # burnin
n_thin <- 10        # thin
n_chain <- 3         # chains


#### RUN MODEL ####
require(R2jags)
jags.dir <- "c:/Program Files/JAGS/JAGS-4.2.0/i386/bin"
out <- jags(data=data_list,
            #inits=inits_list,
            parameters.to.save=params,
            model.file="bayes_density_s_atenuatta_colima_2nd_transect_sst_ssta_depth.txt",
            n.chains=n_chain,
            n.thin=n_thin,
            n.iter=n_iter,
            n.burnin=n_burn,
            DIC=T)

#### MODEL RESULTS ########
# Summarize posteriors and statistics:
model_summary <- print(out, dig=3) # number of decimals for the output.

# Extract MCMC list: 
mcmc_list <- as.mcmc(out)

## Save object:
save(out,
     model_summary,
     mcmc_list,
     file="model_s_atenuatta_colima_sst_ssta_depth_2nd_order.RData")

require(mcmcplots)
# Libraries for diagnostics:
require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics


# Densities:
x11()
densityplot(mcmc_list)

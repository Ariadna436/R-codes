# Clean:
rm(list=ls())  # clear all
graphics.off() # close all;
gc() # Clear memmory (residuals of operations?, cache? Not sure)


requirements <- c(
  'igraph',
  'coda',
  'testthat',
  'R6',
  'mvtnorm',  ## needed for test-distributions.R
  'abind',    ## needed for test-compareMCMCs.R
  'ggplot2',  ## needed for test-compareMCMCs.R
  'covr'      ## needed for code coverage reports
)     

for (package in requirements) {
  if (!suppressPackageStartupMessages(require(package,
                                              character.only = TRUE))) {
    install.packages(package, repos = 'http://cran.us.r-project.org')
  }
}

# #
    #mean_month_sst_df<- read.csv("mean_month_sst_df_1990-2020_McCue.csv")
     mean_year_sst_df<- read.csv("mean_year_sst_df_1990-2020_McCue.csv")
    hist_abund_df<- read.csv("historical_count_1991-2019_correction_factor_pup_production_hypermedia_muA_norm.csv")
#   hist_abund_df<- hist_abund_df[order(hist_abund_df$year),]

# ##Historical abundances: ####
# hist_abund_df<- read.csv("historical_count_1977-2019_foot_boat_based.csv")
# hist_abund_df<- hist_abund_df[which(hist_abund_df$year >= "1991"),]
# hist_abund_df<- hist_abund_df[which(hist_abund_df$method == "boat" | hist_abund_df$method == "foot"
#                                     | hist_abund_df$method == "boat_foot_correction"),]
# hist_abund_df<- hist_abund_df[order(hist_abund_df$method),]
# hist_abund_df<- hist_abund_df[-which(hist_abund_df$year_lab == "2018"),]
# 
# index_boat<- which(hist_abund_df$method == "boat")
# index_foot <- which(hist_abund_df$method == "foot")
# index_w_cf<- which(hist_abund_df$method == "boat_foot_correction")
# 
# # # # #Open data for correction by day
# pup_increment <- read.csv("pup_increment_by_day.csv")
# #pup_increment<- pup_increment[which(pup_increment$pup > 0),]
# pup_increment<- pup_increment[order(pup_increment$day_year),]
# pup_increment<- pup_increment[-c(29),]                           # conteo posterior al paso del huracán
# pup_increment<- pup_increment[order(pup_increment$year),]
# pup_increment$year_lab<- factor(pup_increment$year)
# 
# 
# # #
# # # #
# library(R2jags)
# # ###############Model correction factor by day ###############
# sink("pup_gfs_trend_count_corrected_by_day_correction_foot_plus_boat_pois_muA.txt")
# #
# #
# # ### JAGS MODEL pop trend ####
# cat(
#   "model {
# 
# 
#   for (j in 1:n_count_day) {
# 
#   # Breeding season model:
#   count_day[j] ~  dpois(pup_day[j]) #dnorm(pup_day[j],1/pow(sd_pup_day,2)) 
#   A[j] ~ dnorm(mu_A, 1/pow(sd_A, 2))
# 
#   pup_day[j] <- A[j] * exp(-a0 * exp(-a1[year[j]]*dy_yr[j]))
#   #pup_day[j] <- A[year[j]] * exp(-a0 * exp(-a1[year[j]]*dy_yr[j]))
# 
#  
#   error_vec[j]<-  count_day[j] - pup_day[j]
# 
#     ##pup trend model check (sums-of-squares type discrepancy):###
#     squared_error_timing[j] <- pow((count_day[j] - pup_day[j]),2)                    # Squared residuals for observed data
#     new_pup_day[j] ~ dpois(pup_day[j]) # dnorm(pup_day[j],1/pow(sd_pup_day,2))                                                # Replicate (one new data set at each MCMC iteration)
#     new_squared_error_pup_day[j] <- pow((new_pup_day[j] - pup_day[j]), 2)             # Squared residuals for new data
# 
#     }
# 
# #### POSTERIOR PREDICTIVE CHECKS ####
# # # # Bayesian p-value for birth timing model:
# fit_obs_pups <- sum(squared_error_timing[])      # Sum of squared residuals for actual dataset
# fit_new_pups <- sum(new_squared_error_pup_day[])      # Sum of squared residuals for replicated dataset
# test_fit_pups <- step(fit_new_pups-fit_obs_pups)  # Test whether new data set is more extreme
# b_p_value_pups <- mean(test_fit_pups)               # Bayesian p-value (0.5 expected)
# 
#   
# 
# 
#   for (y in 1:n_years_seas) {
#   #A[y] ~ dnorm(hypmu_A, 1/pow(hypsd_A, 2))
#   #a0[y] ~ dnorm(hypmu_a0, 1/pow(hypsd_a0, 2))
#   a1[y] ~ dnorm(hypmu_a1, 1/pow(hypsd_a1, 2))
# 
#   }
# 
#   #Uninformative priors #
#   #sd_pup_day ~ dunif(min_sd_pup_day,max_sd_pup_day)
#   a0 ~ dunif(min_a0,max_a0)
#   hypmu_a1 ~ dunif(min_a1,max_a1)
#   #hypsd_a0 ~ dunif(min_hypsd_a0, max_hypsd_a0)
#   hypsd_a1 ~ dunif(min_hypsd_a1, max_hypsd_a1)
#    # hypsd_A ~ dunif(0, max_sd_A)
# 
#   #hypmu_A ~ dunif(min_A, max_A)
# 
# 
#   mu_A ~ dunif(min_A,max_A)
#   sd_A ~ dunif(min_sd_A,max_sd_A)
#   
#   r_squ_day<- pow(sd(pup_day),2)/
#               (pow(sd(pup_day),2)+
#                  pow(sd(error_vec),2))
# 
#   for(k in 1:n_dy_yr_pred){
#   pred_c_day[k] <- mu_A * exp(-a0 * exp(-hypmu_a1*dy_yr_pred[k]))
#   pred_c_day_y1[k] <- mu_A * exp(-a0 * exp(-a1[1]*dy_yr_pred[k]))
#   pred_c_day_y2[k] <- mu_A * exp(-a0 * exp(-a1[2]*dy_yr_pred[k]))
#   }
#   
#   
# 
#  # Correction boat count
#  boat_detect ~ dbin (p,foot_observed)
#  p ~ dunif(0,1)
# 
#   count_boat_c<- count_boat * (1+p)
#   count_total <- c(count_cf,(count_foot+ count_boat_c))
# 
#   # Now, predict the count for every day
#    # of pup counts (your data base),
#    # and estimate the proportion:
#     for (l in 1:n_dy_yr_hist) {
#      pred_pups_day[l] <-  mu_A * exp(-a0 * exp(-hypmu_a1*dy_yr_hist[l]))
#      prop_pups_day_mult[l] <- abs((pred_pups_day[l]/mu_A)-1)+1
#      pup_production[l]<- (count_total[l]*prop_pups_day_mult[l])
# 
#     }
# 
# }",
#  fill=TRUE)
#  sink() #ends JAGS model
# # # #
# 
# # #
# 
#  # # # #### Data list ####
# data_list <- list(
#   n_count_day = nrow(pup_increment),
#   count_day = pup_increment$pup,
#   n_years_seas = length(unique(pup_increment$year_lab)),
#   year = (pup_increment$year_lab),
#   min_a0 = 0.1, max_a0= 0.5,  #0.3-0.55
#   min_a1 = 1, max_a1= 3.5,
#   #min_hypsd_a0= 0, max_hypsd_a0 =0.4,
#   min_hypsd_a1= 0,max_hypsd_a1 =0.6,
# 
# 
#   min_A =95, max_A= 120,
#   min_sd_A= 0,max_sd_A =20,
#   #min_sd_pup_day= 0,max_sd_pup_day=5,
#   #A = (pup_increment$max_count),
# 
# 
#    dy_yr = (pup_increment$day_year - (mean(pup_increment$day_year)))/sd(pup_increment$day_year),
#    n_dy_yr_pred = length((seq(min(pup_increment$day_year),max(hist_abund_df$mean_day_yr)+1,1)-(mean(pup_increment$day_year)))/sd(pup_increment$day_year)),
#                             
#   dy_yr_pred = (seq(min(pup_increment$day_year),max(hist_abund_df$mean_day_yr)+1,1)-(mean(pup_increment$day_year)))/sd(pup_increment$day_year),
#  
# 
#   boat_detect = 2460,
#   foot_observed = 4507,
# 
#   count_foot = hist_abund_df$pup_count[index_foot],
#   count_boat =hist_abund_df$pup_count[index_boat],
#   count_cf = hist_abund_df$pup_count[index_w_cf],
# 
#   n_dy_yr_hist = length(hist_abund_df$mean_day_yr[c(index_w_cf,index_foot)]),
#   dy_yr_hist = (hist_abund_df$mean_day_yr[c(index_w_cf,index_foot)]-(mean(pup_increment$day_year)))/sd(pup_increment$day_year)
#   )
# #
# # # #########set MCMC controls##################
# # # ## ## ## ### ##### ##### ###### ###
# n.iter <- 1000000
# n.chains <- 5
# n.thin <- 20
# n.burn <- round(n.iter*0.2)
# 
# # ## ######################### ##
# # ### set parameters to monitor
# # ## ######################### ##
# monitor <- c( "a0",
#               #"sd_pup_day",
#               "hypmu_a1",
#              # "hypsd_a0",
#               "hypsd_a1",
#               "mu_A",
#               "sd_A",
#              "r_squ_day",
#              "pred_c_day",
#              "pred_c_day_y1",
#              "pred_c_day_y2",
#              #"prop_pups_day_mult",
#              "p",
#              "pup_production",
#             # "pup_day",
#              "b_p_value_pups"
#             )
# #
# #
# # # ## ####################### ##
# # # #### run JAGS model ####
# # # ## ####################### ##
# # #
# #library(jagsUI)
# #
# out_pois <- jags(data = data_list,
#             # inits = inits,
#             parameters.to.save = monitor,
#             model.file = "pup_gfs_trend_count_corrected_by_day_correction_foot_plus_boat_pois_muA.txt",
#             n.thin = n.thin, n.chains = n.chains,
#             n.burnin = n.burn, n.iter = n.iter)
# 
#            # parallel = TRUE, n.cores=3
# # m1_pois <- jags.samples(out_pois$model, 
# #                            c("WAIC","deviance"), 
# #                            type = "mean", 
# #                            n.iter = 5000,
# #                            n.burnin = 1000,
# #                            n.thin = 1)
# # m1_pois$p_waic <- m1_pois$WAIC
# # m1_pois$waic <- m1_pois$deviance + m1_pois$p_waic
# # tmp <- sapply(m1_pois, sum)
# # waic.m1_pois <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
# # 
# # m1_poiss <- lapply(m1_pois, unclass)
# # m1_poiss<-sapply(m1_poiss, sum)
# # 
#  load("model_pupping-timing_gompertz_correction_fact_pup_production_hypermedia_norm_muA_a0.RData")
# # m1_norm <- jags.samples(out$model, 
# #                         c("WAIC","deviance"), 
# #                         type = "mean", 
# #                         n.iter = 5000,
# #                         n.burnin = 1000,
# #                         n.thin = 1)
# # m1_norm$p_waic <- m1_norm$WAIC
# # m1_norm$waic <- m1_norm$deviance + m1_norm$p_waic
# # tmp <- sapply(m1_norm, sum)
# # waic.m1 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
# # 
# # 
# # m1_norm_waic<- m1_norm$deviance + m1_norm$WAIC
# # tmp <- sapply(m1_norm_waic, sum)
# # waic.m1 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
# # m1_norm <- lapply(m1_norm, unclass)
# # m1_norm<-sapply(m1_norm, sum)
# #  
# # model1_df <- data.frame(m1_pois=waic.m1_pois,m1_norm= waic.m1)
# require(MCMCvis)
# # # #### MODEL RESULTS ########
# # # # Summarize results:
# #model_summary1<- read.csv("model_seasonality_statistics_gompertz_1977-2019.csv",header = T)
# model_summary1_pois <- MCMCsummary(out_pois, params = "all",
#                               probs = c(0.025, 0.5,  0.975), round = 3, func = function(x) ecdf(x)(0))
# 
# 
# # # # Add the n.eff in %:
# #model_summary1$n.eff.perc <- floor((model_summary1$n.eff/out$BUGSoutput$n.sims)*100)#floor((model_summary1$n.eff/out$mcmc.info$n.samples)*100)
# # Convert funct in percent:
# # model_summary1$prob_less_t_zero <- ceiling(model_summary1$func*100)
# # model_summary1$prob_more_t_zero <- floor(abs(model_summary1$func-1)*100)
# # #
# # # # Save summary as CSV table:
# posterior_statistics<- as.data.frame(model_summary1)
#  write.csv(posterior_statistics,
#          file = "table_results_gfs_pup_pupping-timming_gompertz_foot_correction_1991-2019_total_pup_production_normal_muA.csv")
# 
# # 
# # 
# #save(out, model_summary1, data_list, file = "model_pupping-timing_gompertz_correction_fact_pup_production_hypermedia_norm_muA_a0.RData")
# #load("model_pupping-timing_gompertz_correction_fact_pup_production_hypermedia_norm_hypA_a0.RData")
# #   # #
# # # # Plot  posteriors:
#   MCMCtrace(out_pois,
#             params = c("p",
#                       #"sd_pup_day",
#                       "a0",
#                       "hypmu_a1",
#                      # "hypsd_a0",
#                       "hypsd_a1",
#                       "mu_A",
#                       "sd_A",
#                       "r_squ_day",
#                       "b_p_value_pups",
#                       "deviance"
#             # params = c("p",
#             #            "a0",
#             #            "a1",
#             #            "r_squ_day",
#             #            "deviance"
#                        #"pup_production"
#                        ),
#             main_den = c(bquote(italic("p")),
#                          #bquote(sigma[p_day]),
#                          bquote(alpha[0]),
#                          bquote(mu[alpha[1]]),
#                         # bquote(sigma[alpha[0]]),
#                          bquote(sigma[alpha[1]]),
#                          bquote(mu[A]),
#                          bquote(sigma[A]),
#                          bquote("Bayesian R"[day]^2),
#                          bquote("Bayesian p-value [Birth timing]"),
#                          
#                          bquote("DIC")
#                          #rep(bquote("Pup production"),12)
#                          ),
#             xlab_den = "",
#             xlab_tr = NULL,
#             sz_main_txt = 1.5,
#             n.eff = F,
#             type = 'density',
#             ind = TRUE,
#             pdf = T,
#             filename = "posteriors_of_interest_model_1_total_pup_production_year_hyper_S1_pois_muA_a0.tiff")
# 
# # library(loo)
# #   out_list<- list(out,out_pois)
# # looic<- compare_models(out_list, loo = TRUE)  
# # # #
# hist_abund_df<- hist_abund_df[which(hist_abund_df$method == "foot"|hist_abund_df$method =="boat_foot_correction"),]  
# 
# corrected_count<- data.frame(year= hist_abund_df$year,
#                              year_lab = hist_abund_df$year_lab,
#                              day_year= hist_abund_df$mean_day_yr)
# 
#  pred_day<- data.frame(pred_day= seq(min(pup_increment$day_year),max(hist_abund_df$mean_day_yr)+1))
# 
# for (i in seq(1, nrow(pred_day))) {
#   pred_day[i, "med_pred_day_count"] <-  round(model_summary1[paste("pred_c_day[",i,"]",sep=""), "50%"],digits = 0)
#   pred_day[i, "lwr_pred_day_count"] <-  round(model_summary1[paste("pred_c_day[",i,"]",sep=""), "2.5%"],digits = 0)
#   pred_day[i, "upr_pred_day_count"] <-  round(model_summary1[paste("pred_c_day[",i,"]",sep=""), "97.5%"],digits = 0)
#   
#   pred_day[i, "med_pred_day_count_y1"] <-  round(model_summary1[paste("pred_c_day_y1[",i,"]",sep=""), "50%"],digits = 0)
#   pred_day[i, "med_pred_day_count_y2"] <-  round(model_summary1[paste("pred_c_day_y2[",i,"]",sep=""), "50%"],digits = 0)
#  # pred_day[i, "lwr_pred_day_count_50"] <-  round(model_summary1[paste("pred_c_day[",i,"]",sep=""), "25%"],digits = 0)
#   #pred_day[i, "upr_pred_day_count_50"] <-  round(model_summary1[paste("pred_c_day[",i,"]",sep=""), "75%"],digits = 0)
# }
# # 
# 
# for (i in seq(1, nrow(hist_abund_df))) {
#   hist_abund_df[i, "mean_pup_production"]<-  round(model_summary1[paste("pup_production[",i,"]",sep=""), "mean"],digits = 0)
#    hist_abund_df[i, "med_pup_production"]<-  round(model_summary1[paste("pup_production[",i,"]",sep=""), "50%"],digits = 0)
#   hist_abund_df[i, "lwr_pup_production"] <-  round(model_summary1[paste("pup_production[",i,"]",sep=""), "2.5%"],digits = 0)
#   hist_abund_df[i, "upr_pup_production"] <-  round(model_summary1[paste("pup_production[",i,"]",sep=""), "97.5%"],digits = 0)
#   hist_abund_df[i, "sd_pup_production"] <-  round(model_summary1[paste("pup_production[",i,"]",sep=""), "sd"],digits = 0)
# }
# 
# 
#  r_squ_day <- model_summary1["r_squ_day",]
# 
# require(scales)
# x_label<- c(min(pup_increment$day_year),182,214)
# x_label<- format(as.Date(x_label,origin = "1970-01-01"), "%B")
# 
# # 
# write.csv(hist_abund_df,file = "historical_count_1991-2019_correction_factor_pup_production_hypermedia_muA_norm.csv")
# # 
# # 
# # require("ggrepel")
# # #### Plot corrected count by day ####
# require(ggplot2)
# pup_corrected_count<-
#   ggplot()+
#   # bayesian results:
#   geom_ribbon(data= pred_day,
#               aes(x = pred_day,
#                   ymin = lwr_pred_day_count,
#                   ymax = upr_pred_day_count
#               ),
#               fill = "gray81",
#               color = NA,
#               alpha = 0.5
#   )+
# 
#   geom_path(data = pred_day,
#             aes(x = pred_day,
#                 y = med_pred_day_count_y1,
#             ),
#             linetype= "dashed",
#             color = "#9A94BC",#01665e
#             alpha=1,
#             size = 0.5)+
#   geom_path(data = pred_day,
#             aes(x = pred_day,
#                 y = med_pred_day_count_y2,
#             ),
#             color = "#ADBF97",#01665e
#             linetype= "dashed",
#             alpha=1,
#             size = 0.5)+
#   geom_path(data = pred_day,
#             aes(x = pred_day,
#                 y = med_pred_day_count
# 
#             ),
#             color = "gray65",#01665e
#             alpha=1,
#             size = 1.5)+
#   
#  
# 
#   #modeled count
# 
#   geom_point(data= pup_increment[which(pup_increment$pup>0),],
#              aes(x=day_year,
#                  y=pup,
#                  fill= factor(year)),
#               color = "gray40",
#               # fill= "gray65",
#              shape= 21,
#              size= 3,
#              stroke = 0.5,
#              alpha=1)+
#   scale_fill_manual(values = c("#9A94BC", "#ADBF97"),
#                     guide = guide_legend(show= F))+
#   
#   #            size= 0.5,
#   #            linetype = "dashed",
#   #            color = "gray40")+
# 
# 
#   # geom_rug(data = hist_abund_df,#[which(pred_df$pred_year %in% hist_abund_df$year),],
#   #            aes(x =mean_day_yr),
#   #          position = position_dodge2(width = 0.5),
#   #          alpha= 1,
#   #          size= 0.7,
#   #          color="gray60"
#   #
#   #            )+
# 
#   geom_text(aes(x = min(pup_increment$day_year) + 60,
#                 y = 41),
#             fontface = "bold",
#             family= "serif",
#             color = "gray50",
#             label ="italic(n == mu[N[max]] * e ^ (-alpha[0]*e ^ (alpha[1] * d)))",
#             parse = TRUE,
#             size = 6
#   )+
#   geom_text(aes(x = min(pup_increment$day_year) +59.5,
#                 y = 25),
#             fontface = "bold",
#             color = "gray50",
#             family = "serif",
#             label = paste("italic(BR)^{2}==",
#                           round(r_squ_day["50%"],
#                                 digits = 3),
#                           "~(",
#                           round(r_squ_day["2.5%"],
#                                 digits = 3),
#                           " - ",
#                           round(r_squ_day["97.5%"],
#                                 digits = 3),
#                           ")",
#                           sep = ""),
#             parse = TRUE,
#             size = 4)+
# 
#   # geom_label(aes(x=min(pup_increment$day_year) + 3,
#   #                y=118),
#   #            label= "A",
#   #            fontface = "bold",
#   #            size= 5)+
# 
#     # Settings:
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(size = 12),
#     axis.text.y = element_text(size = 12),
#     axis.title.x = element_text(size=12,margin = margin(t = 2, r = 0, b = 0, l = 0)),
#     axis.title.y = element_text(size=12,margin = margin(t = 0, r = 10.5, b = 0, l = 0)),
# 
#     legend.position = 'none',  # c(left, bottom)
#     legend.text = element_text(size=12),
#     legend.key.height = unit(0.5,"cm"),
#     legend.title = element_blank(),
#     legend.background=element_rect(fill=alpha('white',0)),
# 
#     strip.text = (element_text(size = 12)),
#     axis.ticks = (element_line(size=0.35)),
#     axis.ticks.length=unit(0.2, "cm"),
#     panel.grid = (element_line(size=0.35)),
#     panel.grid.minor = element_blank(),
# 
#     plot.margin=unit(c(0.3, 0.6, 0.2, 0.01),units="cm"), #top, right, bottom, and left
#     strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
#   )+
# 
#   scale_x_continuous(limits = c(min(pup_increment$day_year),max(hist_abund_df$mean_day_yr)+1),
#     breaks = c(151,166,182,197,213),
#     labels = c("June","","July","","August"),
#     expand = c(0,0))+
# 
#   scale_y_continuous(limits = c(0,max(pup_increment$pup)+5),
#                      breaks = seq(0,max(pup_increment$pup),20),
#                      expand = c(0,0))+
#  coord_cartesian(ylim = c(0,127))+
#   labs(x="",
#        y="Cumulative pup count")
# 
# # # # ### SAVE AS JOURNAL FIGURE: ###
# tiff(filename="Pup_gfs_trend_increment_by_dy_gompertz_1991-2020_by_year_p_production_hypermedia_norm_a0_muA.tiff",
#      width=17 ,height=24.5*0.4 ,units="cm",
#      bg="white",
#      res=600,
#      compression = c("lzw"))
# print(pup_corrected_count)
# dev.off()
# # # # # #
# # # #  write.csv(as.data.frame(model_summary1),file="model_seasonality_statistics_gompertz_1990-2020.csv")

# # # #################################################################
# ################## SST values and index years ################## 

    
unique_year <- unique(hist_abund_df$year)


pred_df<- data.frame(
  pred_year=seq(1990,2020))

list_index <- list()
for (i in 1:nrow(hist_abund_df)) {
  index_yr <- which(pred_df$pred_year == hist_abund_df$year[i])
  list_index[[i]]<- list(index_yr)
}

index_year <- unlist(list_index)
### SST file, HadISST_sst.nc have global SST since 1870-01-01 to 2020-01-16 ####

# # # Open .nc:
# require(ncdf4)
# filename <- "~/My_R/variables/SST/HadISST_sst.nc"
# nc_base <- nc_open(filename)
# print(nc_base)
# #
# # Define the running-mean period:
# running <- 1
# 
# # Coordinates:
# time_sst <- ncvar_get(nc_base,
#                       'time') # days since 01-01-1870
# 
# year_sst <- as.numeric(format(as.Date(time_sst, origin= "1870-01-01"), format = "%Y"))
# 
# lon_sst <- ncvar_get(nc_base,
#                      'longitude')
# lat_sst <- ncvar_get(nc_base,
#                      'latitude')
# sst <- ncvar_get(nc_base,
#                  'sst')
# sst <- sst[ , ,]
# 
# 
# # Close the .nc dataset:
# nc_close(nc_base)
# 
# 
# ## Construct the data frame:
# sst_df_list <- list()
# for (i in seq(1400, length(time_sst))) {
#   sst_df <- data.frame(
#     time = c(matrix(time_sst[i],
#                     ncol = ncol(sst[,,i]),
#                     nrow = nrow(sst[,,i]))),
#     year = c(matrix(year_sst[i],
#                     ncol = ncol(sst[,,i]),
#                     nrow = nrow(sst[,,i]))),
#     sst = c(sst[, , i]),
#     lon = c(matrix(data=lon_sst,
#                    nrow=length(lon_sst),
#                    ncol=length(lat_sst),
#                    byrow=FALSE)),
#     lat = c(matrix(data=lat_sst,
#                    nrow=length(lon_sst),
#                    ncol=length(lat_sst),
#                    byrow=TRUE))
#   )
#   sst_df_list[[i]] <- sst_df
# }
# sst_df <- do.call("rbind",
#                   sst_df_list)
# sst_df <- sst_df[which(!is.na(sst_df$sst)), ]
# 
# # Take out data outside guadalupe 700km:
# # Read KML file (polygon):
# require(maptools)
# require(SDMTools)
# require(spatialEco)
# require(sp)
# polyg_gpe <- data.frame(getKMLcoordinates("~/My_R/GFS_core_McCue_Gallo.kml",
#                                           ignoreAltitude=TRUE))
# 
# i3  <- point.in.polygon(sst_df$lon,sst_df$lat,
#                    polyg_gpe$X1,polyg_gpe$X2)
# 
# sst_df <- sst_df[i3 == 1,]
# gc()
# 
# # #take out data inside cal. gulf #
# # polyg_ca_gulf <- data.frame(getKMLcoordinates("~/My_R/Apt_pop_grow/california_gulf.kml",
# #                                               ignoreAltitude=TRUE))
# # i3  <- pnt.in.poly(cbind(sst_df$lon,
# #                          sst_df$lat),
# #                    polyg_ca_gulf)
# #
# # sst_df <- sst_df[i3$pip == 0,]
# # gc()
# 
# #Mean sst each month:
# mean_month_sst_df <- data.frame()
# for(i in seq(1, length(unique(sst_df$time)))) {
#   index_month <- which(sst_df$time == unique(sst_df$time)[i])
#   mean_month_sst_df[i, "time"] <- unique(sst_df$time)[i]
# 
#   mean_month_sst_df[i, "year"] <- as.numeric(format(as.Date(unique(sst_df$time)[i], origin= "1870-01-01"), format = "%Y"))
# 
#   mean_month_sst_df[i, "mean_sst"] <- mean(sst_df$sst[index_month], na.rm = T)
#   mean_month_sst_df[i, "sd_sst"] <- sd(sst_df$sst[index_month], na.rm = T)
#   mean_month_sst_df[i, "month"] <- as.numeric(format(as.Date(unique(sst_df$time)[i], origin= "1870-01-01"), format = "%m"))
# }
# # Estimate the running means:
# # Empirically:
# mean_year_sst_df <- data.frame()
# for(j in seq(1, length(unique(sst_df$time)))) {
#   index_yr <- which(sst_df$time <= unique(sst_df$time)[j] &
#                       sst_df$time >= (unique(sst_df$time)[j]-(running*365.25)))
#   mean_year_sst_df[j, "time"] <- unique(sst_df$time)[j]
# 
#   mean_year_sst_df[j, "year"] <- as.numeric(format(as.Date(unique(sst_df$time)[j], origin= "1870-01-01"), format = "%Y"))
# 
#   mean_year_sst_df[j, "mean_sst"] <- mean(sst_df$sst[index_yr], na.rm = T)
#   mean_year_sst_df[j, "sd_sst"] <- sd(sst_df$sst[index_yr], na.rm = T)
#   mean_year_sst_df[j, "month"] <- as.numeric(format(as.Date(unique(sst_df$time)[j], origin= "1870-01-01"), format = "%m"))
# }
# 
# 
# # Cut the period:
# mean_month_sst_df <- mean_month_sst_df[which(mean_month_sst_df$year >= 1990 & mean_month_sst_df$year <= 2020),]
# mean_year_sst_df <- mean_year_sst_df[which(mean_year_sst_df$year >= 1990 & mean_year_sst_df$year <= 2020),]
# #
# write.csv(mean_month_sst_df, file = "mean_month_sst_df_1990-2020_McCue.csv")
# write.csv(mean_year_sst_df, file= "mean_year_sst_df_1990-2020_McCue.csv")



# ###############################################################
index_july <-  which(mean_year_sst_df$month == "7" )
# ##################SST model & ecological model################

# sink("pup_gfs_trend_anomaly_ssta_modeled_count_1990-2019_normal_trunc_rep_rate_hyp_normal.txt")
# 
# #modelo_trunc_2nd.bugs<- 
# ### JAGS MODEL pop trend ####
# cat(
#  "model {
library(nimble)
Section6p4_code <- nimbleCode( {
############## Predict anomaly sst ################
###################################################

# Estimation of 1950-2019 SST means:
for (i in 1:n_sst_yearly) { #todos los meses
  sst_mean_yearly[i] ~ dnorm(mu_sst_yearly[i],         #mean july                # Normal likelihood of SST:
                      1/pow(sd_sst_yearly[i], 2))       # with known (espatial-temporal) SD'sone-year_running-mean

  mu_sst_yearly[i]<- g0 + g1 * pred_d_julian[i] #mismas unidades que sig. modl #solo medias anuales

  sst_yearly_anom[i]<- sst_mean_yearly[i] - mu_sst_yearly[i]
  error_sst[i] <- sst_mean_yearly[i]- mu_sst_yearly[i]
  
  # ## SST anomaly model check (sums-of-squares type discrepancy):
   squared_error_sst[i] <- pow((sst_mean_yearly[i] - mu_sst_yearly[i]),2)                    # Squared residuals for observed data
   new_mu_sst_yearly[i] ~  dnorm(mu_sst_yearly[i],1/pow(sd_sst_yearly[i],2))  #dpois(lambda_count[j])                                              # Replicate (one new data set at each MCMC iteration)
   new_squared_error_sst[i] <- pow((new_mu_sst_yearly[i] - mu_sst_yearly[i]), 2)             # Squared residuals for new data

 
}
 sst_july<- mu_sst_yearly[index_july]
 sst_july_anom <- sst_yearly_anom[index_july]

#  #### POSTERIOR PREDICTIVE CHECKS ####

# # # Bayesian p-value for group counts model:
 fit_sst_mean <- sum(squared_error_sst[])      # Sum of squared residuals for actual dataset
 fit_new_sst_mean <- sum(new_squared_error_sst[])      # Sum of squared residuals for replicated dataset
 test_fit_sst <- step(fit_new_sst_mean - fit_sst_mean)  # Test whether new data set is more extreme
 b_p_value_sst <- mean(test_fit_sst)               # Bayesian p-value (0.5 expected)


#Bayesian R2 from sst model
r_squ_sst_model<- pow(sd(mu_sst_yearly),2)/
                 (pow(sd(mu_sst_yearly),2) +
                 (pow(sd(error_sst),2)))

# Uninformative prior on the mean:
g0 ~ dunif(min_g0, max_g0)
g1 ~ dunif(min_g1, max_g1)

############## Predict count ###############
###########################################

##Model of counts as a function of the year
for (j in 1:n_counts) {
  pup_production[j] ~ dnorm(mu_pup_production[j], 1/pow(sd_pup_production[j],2))
  log(mu_pup_production[j])<-log_lambda_count[j]
  log_lambda_count[j] <- b0 + b1*year[j] + (b2*pow(year[j],2)) + e0[j]

  e0[j]~ dnorm(0, 1/pow(sd_e0,2))
  error_exp[j]<- log(pup_production[j]) - log_lambda_count[j]

## pup trend model check (sums-of-squares type discrepancy):
   squared_error_exp[j] <- pow((log(pup_production[j]) - log_lambda_count[j]),2)                    # Squared residuals for observed data
   new_lambda_count[j] ~  dnorm(mu_pup_production[j],1/pow(sd_pup_production[j],2))  #dpois(lambda_count[j])                                              # Replicate (one new data set at each MCMC iteration)
   new_squared_error_exp[j] <- pow((log(new_lambda_count[j]) - log_lambda_count[j]), 2)             # Squared residuals for new data

}

r_squ_exp_mod<- pow(sd(log_lambda_count),2)/
               (pow(sd(log_lambda_count),2) +
               (pow(sd(error_exp),2)))


 #### POSTERIOR PREDICTIVE CHECKS ####
# # # Bayesian p-value for group counts model:
 fit_obs_counts <- sum(squared_error_exp[])      # Sum of squared residuals for actual dataset
 fit_new_counts <- sum(new_squared_error_exp[])      # Sum of squared residuals for replicated dataset
 test_fit_counts <- step(fit_new_counts-fit_obs_counts)  # Test whether new data set is more extreme
 b_p_value_counts <- mean(test_fit_counts)               # Bayesian p-value (0.5 expected)
 # 


####Predict count ####
for (k in 1:n_pred_yr) {
  pred_count[k] <- exp(b0 + b1*pred_year[k] + (b2*pow(pred_year[k],2)))
  }

#### Priors: ####

b0 ~ dunif(min_b0, max_b0)
b1 ~ dunif(min_b1, max_b1)
b2 ~ dunif(min_b2, max_b2)
sd_e0 ~ dunif(min_sd_e0, max_sd_e0)

grow_rate <- ((max(pred_count)/min(pred_count))^(1/n_pred_yr))-1
#######################################################################
###### Ecological model proportion of expected count vs. ssta #########
for (l in 1:n_index_yr){


  prop_anom[l] <- (pup_production[l] / pred_count[index_yr[l]]) - 1

  zero[l] ~  dnorm(mu_pred_anom[l],1/pow(sd_vec_anom_prop,2)) #T(,0.75)

  mu_pred_anom[l]<- (z0 +
                       (z1 * sst_july_anom[index_yr[l]]) +
                       (z2 * pow(sst_july_anom[index_yr[l]],2))+
                       (z3 * pow(sst_july_anom[index_yr[l]],3)))
                     - prop_anom[l]

  pred_prop_anom[l] <- (z0 +
                       (z1 * sst_july_anom[index_yr[l]]) +
                       (z2 * pow(sst_july_anom[index_yr[l]],2))+
                       (z3 * pow(sst_july_anom[index_yr[l]],3)))

  err_vec_anom[l] <- prop_anom[l] - (z0 +
                       (z1 * sst_july_anom[index_yr[l]]) +
                       (z2 * pow(sst_july_anom[index_yr[l]],2))+
                       (z3 * pow(sst_july_anom[index_yr[l]],3)))
                       
  # ## Ecological model check (sums-of-squares type discrepancy):
    squared_error_ecol[l] <- pow((zero[l] - mu_pred_anom[l] ),2)                    # Squared residuals for observed data
  #squared_error_ecol[l] <- pow((prop_anom[l] - pred_prop_anom[l]),2)                    # Squared residuals for observed data
   
 new_mu_pred_anom[l] ~  dnorm(mu_pred_anom[l] ,1/pow(sd_vec_anom_prop,2))  #dpois(lambda_count[j])                                              # Replicate (one new data set at each MCMC iteration)
 new_squared_error_ecol[l] <- pow((new_mu_pred_anom[l] - mu_pred_anom[l]), 2)             # Squared residuals for new data
              
  }
#  #### POSTERIOR PREDICTIVE CHECKS ####
# # # # Bayesian p-value for group counts model:
 fit_obs_prop_anom <- sum(squared_error_ecol[])      # Sum of squared residuals for actual dataset
 fit_new_prop_anom <- sum(new_squared_error_ecol[])      # Sum of squared residuals for replicated dataset
 test_fit_prop_anom <- step(fit_new_prop_anom - fit_obs_prop_anom)  # Test whether new data set is more extreme
 b_p_value_prop_anom <- mean(test_fit_prop_anom)               # Bayesian p-value (0.5 expected)

# for(n in 1:n_index_yr){
#  new_mu_pred_anom[n] ~  dnorm(pred_prop_anom[n],1/pow(sd_vec_anom_prop,2))  #dpois(lambda_count[j])                                              # Replicate (one new data set at each MCMC iteration)
#  new_squared_error_ecol[n] <- pow((new_mu_pred_anom[n] - pred_prop_anom[n]), 2)             # Squared residuals for new data
# }
##### Predicted values for ecological model #####
for(m in 1:n_ssta_vec){

  predict_ssta_anom_prop[m]<- (z0 +
                                          (z1 * ssta_july_vec[m]) +
                                         (z2 * pow(ssta_july_vec[m],2))+
                                         (z3 * pow(ssta_july_vec[m],3)))
  
              }

#   # Bayesian R-squared:
r_squ_ecol <- pow(sd((z0 +
                       (z1 * sst_july_anom[index_yr]) +
                       (z2 * pow(sst_july_anom[index_yr],2))+
                       (z3 * pow(sst_july_anom[index_yr],3)))), 2)
           / (pow(sd(z0 +
                       (z1 * sst_july_anom[index_yr]) +
                       (z2 * pow(sst_july_anom[index_yr],2))+
                       (z3 * pow(sst_july_anom[index_yr],3))),2)
              + (pow(sd(err_vec_anom), 2)))


# ####Priors ####

z0 ~ dunif(min_z0, max_z0)
z1 ~ dunif(min_z1, max_z1)
z2 ~ dunif(min_z2, max_z2)
z3 ~ dunif(min_z3, max_z3)
sd_vec_anom_prop ~ dunif(min_sd_prop, max_sd_prop)
#
 for(p in 1:n_pred_yr_sst){

  count_modeled[p]<- ifelse((pred_count[p] * ((z0 +
                                             (z1 * sst_july_anom[p]) +
                                             (z2 * pow(sst_july_anom[p],2))+
                                             (z3 * pow(sst_july_anom[p],3))) + 1 )) 
                                             > (pred_count[p]/0.6), ((pred_count[p]/0.6)*0.8),
                          (pred_count[p] * ((z0 +
                                            (z1 * sst_july_anom[p]) +
                                            (z2 * pow(sst_july_anom[p],2))+
                                            (z3 * pow(sst_july_anom[p],3))) + 1 )))                    
                                            


                                    
 }

 }",
fill=TRUE)
sink() # end of JAGS model
# 
#cat(modelo_trunc_2nd.bugs, file = "modelo_trunc_2nd.bugs")

data_list2 <- list(
  
  n_sst_yearly= nrow(mean_year_sst_df),
  sst_mean_yearly = mean_year_sst_df$mean_sst,
  sd_sst_yearly = mean_year_sst_df$sd_sst,
  pred_d_julian = (mean_year_sst_df$time - mean(mean_year_sst_df$time))/sd(mean_year_sst_df$time),
  
  
  index_july = index_july,
  min_g0 = 17.8, max_g0 = 18.4,   #min_g0 = 18.4, max_g0 = 18.9,
  min_g1 = -0.1, max_g1 =0.3,       # min_g1 = 0, max_g1 =0.4,

  n_counts  =  nrow(hist_abund_df),
  pup_production = hist_abund_df$mean_pup_production,
  sd_pup_production = hist_abund_df$sd_pup_production,
  
  min_b0 = 8.1,max_b0 =8.5,   # min_b0 = 7.9,max_b0 = 8.6,
  min_b1 =1, max_b1 =1.25,
  min_b2 =0.2, max_b2 =0.45, 
  #min_r = 0,  max_r= 100,# min_b1 =0.9, max_b1 = 1.3,
 # min_sd_e0 = 0, max_sd_e0 = 0.3,
  
  year = ((hist_abund_df$year - mean(hist_abund_df$year))/(sd(hist_abund_df$year))),
  n_pred_yr = nrow(pred_df),
  pred_year = ((pred_df$pred_year - mean(hist_abund_df$year))/(sd(hist_abund_df$year))),

  n_index_yr = length(index_year),#which(pred_df$pred_year %in% hist_abund_df$year)),
  index_yr = index_year,#which(pred_df$pred_year %in% hist_abund_df$year),
  zero = rep(0,nrow(hist_abund_df)),
#
  n_ssta_vec = length(seq(-0.8,1.6,length.out = 100)),
  ssta_july_vec= seq(-0.8,1.6,length.out = 100),

  n_pred_yr_sst = length(index_july),
#

  min_z0 = -0.3, max_z0 =0.1,   # min_z0 = -0.8, max_z0 =0.4,
  min_z1 = -0.8, max_z1 =0.5,       #  min_z1 = -3, max_z1 = 2,
  min_z2 = 0.2, max_z2 =2,       #min_z2 = -2, max_z2 = 8,
  min_z3 = -1.4, max_z3 =0.2,        # min_z3 = -6, max_z3 =2,
  
  min_sd_prop = 0, max_sd_prop= 0.2




)

#########set MCMC controls##################
## ## ## ### ##### ##### ###### ###
n.iter2 <- 100000
n.chains2 <- 5
n.thin2 <- 20
n.burn2 <- round(n.iter2*0.2)

## ######################### ##
### set parameters to monitor
## ######################### ##
monitor2 <- c("g0",
              "g1",
             # "mu_sst_yearly",
              "sst_july_anom",
              "sst_july",
              "r_squ_sst_model",
              "b0",
              "b1",
              "b2",
  #            "sd_e0",
              "pred_count",
              "r_squ_exp_mod",
              "prop_anom",
             #"mu_pred_anom",
              "z0",
              "z1",
              "z2",
              "z3",
              "sd_vec_anom_prop",
              "predict_ssta_anom_prop",
             
              "count_modeled",
              "r_squ_ecol",
              "grow_rate",
              #"test_fit_counts",
              "b_p_value_counts",
             "b_p_value_sst",
              "b_p_value_prop_anom"
             
             )


## ####################### ##
#### run JAGS model ####
## ####################### ##
require(R2jags)
#library(jagsUI)

out2 <- jags(data = data_list2,
             # inits = inits,
             parameters.to.save = monitor2,
             model.file = "pup_gfs_trend_anomaly_ssta_modeled_count_1990-2019_normal_trunc_rep_rate_hyp_normal.txt",
             n.chains = n.chains2,
             n.burnin = n.burn2, n.iter = n.iter2)#,
             #parallel = TRUE, n.cores=2)
# Visualize results:
require(MCMCvis)
# Summarize results:
model_summary2 <- MCMCsummary(out2, params = "all",
                              probs = c(0.025, 0.5, 0.975), round = 8, func = function(x) ecdf(x)(0))
# # Add the n.eff in %:
# model_summary2$n.eff.perc <- round((model_summary2$n.eff/out2$mcmc.info$n.samples)*100,digits = 2) 
# model_summary2$n.eff.perc <- round((model_summary2$n.eff/out2$BUGSoutput$n.sims)*100,digits = 2) 

# # Convert funct in percent:
 # model_summary2$prob_less_t_zero <- ceiling(model_summary2$func*100)
 # model_summary2$prob_more_t_zero <- floor(abs(model_summary2$func-1)*100)

#model_summary2 <- as.data.frame(out2$summary)


#### MODEL RESULTS ########
# Summarize results:
#model_summary_dic2 <- print(out2, dig=3)


# Plot  posteriors:

MCMCtrace(object=out2,
          params = c("b0",
                     "b1",
                     "b2",
                     #"sd_e0",
                     "r_squ_exp_mod",
                     "b_p_value_counts",
                     
                     "grow_rate",
                     "g0",
                     "g1",
                     "r_squ_sst_model",
                     "b_p_value_sst",
                     "z0",
                     "z1",
                     "z2",
                     "z3",
                     "sd_vec_anom_prop",
                     
                     "r_squ_ecol",
                     "b_p_value_prop_anom",
                     "deviance"),
          main_den = c(bquote(beta[0]),
                       bquote(beta[1]),
                       bquote(beta[2]),
                       #bquote(sigma~error),
                       bquote("Bayesian R"[exp]^2),
                       bquote("Bayesian p-value"[Pup~trend]),
                       bquote("Growth rate"[exp]),
                       bquote(gamma[0]),
                       bquote(gamma[1]),
                       bquote("Bayesian R"[SST]^2),
                       bquote("Bayesian p-value"[SSTA]),
                       bquote(zeta[0]),
                       bquote(zeta[1]),
                       bquote(zeta[2]),
                       bquote(zeta[3]),
                       bquote(sigma^2~Pa),
                       bquote("Bayesian R"[ecol]^2),
                       bquote("Bayesian p-value"[ecol]),
                       bquote("DIC")
                       
                       ),
          xlab_den = "",
          xlab_tr = NULL,
          sz_main_txt = 1.5,
          n.eff = F,
          type = 'density',
          ind = TRUE,
          pdf = T,
          filename = "posteriors_of_interest_model_2_yearly_sst_McCue_dnormal_trunc_hyper_mean_100k_noerror.pdf")


# # Save summary as CSV table:
 posterior_statistics<- as.data.frame(model_summary2)
  write.csv(posterior_statistics,
           file = "table_results_gfs_pup_ssta_effect_3rd_1990-2020_McCue_normal_reproductive_rate_trunc_BPvalue_1M_not.csv")
# # # #
# save(out2,data_list2,model_summary2, file = "model_pup_gfs_trend_anomaly_ssta_1990-2020_McCue_normal_trunc_BPvalue_100k.RData")
# # 
# 
# load("model_pup_gfs_trend_anomaly_ssta_1990-2020_McCue_normal_trunc_BPvalue_1M.RData")

#### Inferred pup counts: ####
# Create the dataframe:
## For Guadalupe Island:

 for (i in seq(1, nrow(pred_df))) {
   pred_df[i, "pred_med_count"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "50%"]
   pred_df[i, "pred_lwr_count"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "2.5%"]
   pred_df[i, "pred_upr_count"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "97.5%"]
  # pred_df[i, "pred_lwr_count_50"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "25%"]
  # pred_df[i, "pred_upr_count_50"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "75%"]
   pred_df[i, "pred_sd_count"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "sd"]
   pred_df[i, "pred_mean_count"] <-  model_summary2[paste("pred_count[",i,"]",sep=""), "mean"]
 }
 
 for (i in seq(1, nrow(pred_df))) {
   pred_df[i, "med_count_modeled"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "50%"]
   pred_df[i, "lwr_count_modeled"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "2.5%"]
   pred_df[i, "upr_count_modeled"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "97.5%"]
  # pred_df[i, "lwr_count_modeled_50"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "25%"]
   #pred_df[i, "upr_count_modeled_50"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "75%"]
   pred_df[i, "sd_count_modeled"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "sd"]
   pred_df[i, "mean_count_modeled"] <-  model_summary2[paste("count_modeled[",i,"]",sep=""), "mean"]
 }
 
 pred_df$lwr_count_modeled[which(pred_df$lwr_count_modeled < 0)]<-0
 
 # for (i in seq(1, nrow(hist_abund_df))) {
 #   hist_abund_df[i, "med_predict_anom"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "50%"]
 #   hist_abund_df[i, "lwr_predict_anom"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "2.5%"]
 #   hist_abund_df[i, "upr_predict_anom"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "97.5%"]
 # }
 # 
 hist_abund_df$med_count_modeled <- pred_df$med_count_modeled[index_year]
 hist_abund_df$lwr_count_modeled <- pred_df$lwr_count_modeled[index_year]
 hist_abund_df$upr_count_modeled <- pred_df$upr_count_modeled[index_year]
 hist_abund_df$sd_count_modeled <- pred_df$sd_count_modeled[index_year]
 
 r_squ_exp_mod<- model_summary2["r_squ_exp_mod",]
 
 #### Inferred sst anomaly mean(1yr 1950) ####
 
 mean_sst_df_july<- mean_year_sst_df[index_july,]
 # Add the anomalies:
 for (i in seq(1, nrow(mean_sst_df_july))) {
   mean_sst_df_july[i, "anom_sst_med"] <- model_summary2[paste("sst_july_anom[",i,"]",sep=""), "50%"]
   mean_sst_df_july[i, "anom_sst_mean"] <- model_summary2[paste("sst_july_anom[",i,"]",sep=""), "mean"]
   mean_sst_df_july[i, "anom_sst_sd"] <- model_summary2[paste("sst_july_anom[",i,"]",sep=""), "sd"]
   mean_sst_df_july[i, "anom_sst_lwr"] <- model_summary2[paste("sst_july_anom[",i,"]",sep=""), "2.5%"]
   mean_sst_df_july[i, "anom_sst_upr"] <- model_summary2[paste("sst_july_anom[",i,"]",sep=""), "97.5%"]
 }
 
 
 # Add the anomalies:
 for (i in seq(1, nrow(mean_sst_df_july))) {
   mean_sst_df_july[i, "yearly_sst_med"] <- model_summary2[paste("sst_july[",i,"]",sep=""), "50%"]
   mean_sst_df_july[i, "yearly_sst_mean"] <- model_summary2[paste("sst_july[",i,"]",sep=""), "mean"]
   mean_sst_df_july[i, "yearly_sst_sd"] <- model_summary2[paste("sst_july[",i,"]",sep=""), "sd"]
   mean_sst_df_july[i, "yearly_sst_lwr"] <- model_summary2[paste("sst_july[",i,"]",sep=""), "2.5%"]
   mean_sst_df_july[i, "yearly_sst_upr"] <- model_summary2[paste("sst_july[",i,"]",sep=""), "97.5%"]
  # mean_year_sst_df[i, "yearly_sst_lwr_50"] <- model_summary2[paste("mu_sst_yearly[",i,"]",sep=""), "25%"]
   #mean_year_sst_df[i, "yearly_sst_upr_50"] <- model_summary2[paste("mu_sst_yearly[",i,"]",sep=""), "75%"]
 }
 
 
   # mean_sst_df_july$mu_sst_med <- mean_year_sst_df$yearly_sst_med[index_july]
   # mean_sst_df_july$mu_sst_lwr <- mean_year_sst_df$yearly_sst_lwr[index_july]
   # mean_sst_df_july$mu_sst_upr <- mean_year_sst_df$yearly_sst_upr[index_july]
   # 
 
 
 r_squ_sst<- model_summary2["r_squ_sst_model",]
 
 # Condition:
 mean_sst_df_july$condition <- rep(NA,nrow(mean_sst_df_july))
 mean_sst_df_july$condition[which(mean_sst_df_july$mean_sst > mean_sst_df_july$yearly_sst_upr)] <- "Warm"
 mean_sst_df_july$condition[which(mean_sst_df_july$mean_sst < mean_sst_df_july$yearly_sst_lwr)] <- "Cold"
 
 # mean_sst_df_july$yearly_sst_med<- mean_year_sst_df$yearly_sst_med[index_july]
 # mean_sst_df_july$condition <- mean_year_sst_df$condition[index_july]
 # 
 ###Add sst anomaly to observation #### 
 
 hist_abund_df$med_ssta_anom <- mean_sst_df_july[data_list2$index_yr,"anom_sst_med"]
 hist_abund_df$lwr_ssta_anom <- mean_sst_df_july[data_list2$index_yr,"anom_sst_lwr"]
 hist_abund_df$upr_ssta_anom <- mean_sst_df_july[data_list2$index_yr,"anom_sst_upr"]
 hist_abund_df$condition <- mean_sst_df_july[data_list2$index_yr,"condition"]
 ##### Model anomaly expected count by ssta ####
 for (i in seq(1, nrow(hist_abund_df))) {
   
   hist_abund_df[i, "med_anom_prop"] <-  model_summary2[paste("prop_anom[",i,"]",sep=""), "50%"]
   hist_abund_df[i, "lwr_anom_prop"] <-  model_summary2[paste("prop_anom[",i,"]",sep=""), "2.5%"]
   hist_abund_df[i, "upr_anom_prop"] <-  model_summary2[paste("prop_anom[",i,"]",sep=""), "97.5%"]
   hist_abund_df[i, "sd_anom_prop"] <-  model_summary2[paste("prop_anom[",i,"]",sep=""), "sd"]
   hist_abund_df[i, "mean_anom_prop"] <-  model_summary2[paste("prop_anom[",i,"]",sep=""), "mean"]    
   
   hist_abund_df[i, "med_anom_prop_pred"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "50%"]
   hist_abund_df[i, "lwr_anom_prop_pred"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "2.5%"]
   hist_abund_df[i, "upr_anom_prop_pred"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "97.5%"]
   hist_abund_df[i, "sd_anom_prop_pred"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "sd"]
   hist_abund_df[i, "mean_anom_prop_pred"] <-  model_summary2[paste("pred_prop_anom[",i,"]",sep=""), "mean"]  
 }
 
 ## For this model, only anomalies from July each year:
 ssta_effect_df <- data.frame(
   #ssta_vec= rep(NA,length=100)
   ssta_vec = data_list2$ssta_july_vec
 )
 
 
 for (i in seq(1, nrow(ssta_effect_df))) {
   
   ssta_effect_df[i, "pred_med_anom_prop"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "50%"]
   ssta_effect_df[i, "pred_lwr_anom_prop"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "2.5%"]
   ssta_effect_df[i, "pred_upr_anom_prop"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "97.5%"]
  # ssta_effect_df[i, "pred_lwr_anom_prop_25"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "25%"]
   #ssta_effect_df[i, "pred_upr_anom_prop_75"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "75%"]
   ssta_effect_df[i, "pred_sd_anom_prop"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "sd"]
   ssta_effect_df[i, "pred_mean_anom_prop"] <-  model_summary2[paste("predict_ssta_anom_prop[",i,"]",sep=""), "mean"]    
   
     
   
 }
 
 ssta_effect_df$ssta_anom <- data_list2$ssta_july_vec
 r_squ_ecol <- model_summary2["r_squ_ecol",]
 


 
 ### JAGS MODEL model accuracy ####
#  sink("pup_gfs_trend_model_accuracy_count_modeled_vs_observed.txt")
#  cat(
#  "model {
#  
#   for(r in 1:n_count){
#     count_modeled[r] ~ dnorm(mu_reg[r],1/pow(sd_count[r], 2))
#    
#     mu_reg[r] <- eta0 + eta1 * pup_production[r]
#     error_vec_regr[r]<- count_modeled[r] - mu_reg[r]
#  }
# 
#  r_squ_regres<- pow(sd(mu_reg), 2)
#                 / (pow(sd(mu_reg), 2)
#                 + (pow(sd(error_vec_regr), 2)))
#  
#  for(s in 1:n_vec_count){
#  pred_vec_count[s] <- eta0 + eta1 * vec_count[s]
#  }
#   #priors regression #
#  eta0 ~ dunif(min_eta0, max_eta0)
#  eta1 ~ dunif(min_eta1, max_eta1)
#  #sd_reg ~ dunif(10,15)
#  
#    }",
# fill=TRUE)
# sink()
# 
# #### data list 3 ####
#  data_list3<-  list(
#    n_count = nrow(hist_abund_df),#which(pred_df$pred_year %in% hist_abund_df$year)),
#    pup_production = hist_abund_df$med_pup_production,#which(pred_df$pred_year %in% hist_abund_df$year),
#    count_modeled = hist_abund_df$med_count_modeled,
#    sd_count = hist_abund_df$sd_count_modeled,
#    n_vec_count = length(seq(0,20000,length.out = 100)),
#    vec_count = seq(0,20000,length.out = 100),
#   min_eta0 =-100, max_eta0 =200,
#   min_eta1 = 0.9, max_eta1= 1.1
#  
# )
# 
# ####monitor 3 ####
# monitor3<- c("eta0",
#              "eta1",
#              # "mu_reg",
#              "r_squ_regres",
#              "pred_vec_count"
# )
# 
# #### run JAGS model ####
# ## ####################### ##
# require(R2jags)
# #library(jagsUI)
# 
# out3 <- jags(data = data_list3,
#              # inits = inits,
#              parameters.to.save = monitor3,
#              model.file = "pup_gfs_trend_model_accuracy_count_modeled_vs_observed.txt",
#              n.thin = n.thin2, n.chains = n.chains2,
#              n.burnin = n.burn2, n.iter = n.iter2)
#              #parallel = TRUE, n.cores=3)
# 
# #save(out3,data_list3, file = "model_pup_gfs_trend_anomaly_ssta_model_accuracy_1990-2020_monthly_McCue.RData")
# 
# # Visualize results:
# require(MCMCvis)
# # Summarize results:
# model_summary3 <- MCMCsummary(out3, params = "all",
#                               probs = c(0.025, 0.5,  0.975), round = 4, func = function(x) ecdf(x)(0))
# # # Add the n.eff in %:
# model_summary3$n.eff.perc <- round((model_summary3$n.eff/out3$BUGSoutput$n.sims)*100,digits = 2)
# # # Convert funct in percent:
# model_summary3$prob_less_t_zero <- ceiling(model_summary3$func*100)
# model_summary3$prob_more_t_zero <- floor(abs(model_summary3$func-1)*100)
# 
# # Plot  posteriors:
# # posterior_statistics2<- as.data.frame(model_summary3)
# # write.csv(posterior_statistics2,
# #           file = "table_results_gfs_pup_model_accuracy_1990-2020_negbin_MNPL.csv")
# #
# 
# # # #
# MCMCtrace(out3,
#           params = c("eta0","eta1", "r_squ_regres","deviance"),
#           main_den = c(bquote(eta[0]),
#                        bquote(eta[1]),
#                        bquote("Bayesian R"[pred]^2),
#                        bquote("DIC")),
#           xlab_den = "",
#           xlab_tr = NULL,
#           sz_main_txt = 1.5,
#           n.eff = F,
#           type = 'density',
#           ind = TRUE,
#           pdf = T,
#           filename = "posteriors_of_interest_model3_yearly_McCue_normal_trend.pdf")
# 
# ## For this model, dummy count vector 0-12000:
# model_pred_df <- data.frame(
#   count_vec= rep(NA,length=100)
# )
# 
# 
# for (i in seq(1, nrow(model_pred_df))) {
#   model_pred_df[i, "med_model_reg"] <-  model_summary3[paste("pred_vec_count[",i,"]",sep=""), "50%"]
#   model_pred_df[i, "lwr_model_reg"] <-  model_summary3[paste("pred_vec_count[",i,"]",sep=""), "2.5%"]
#   model_pred_df[i, "upr_model_reg"] <-  model_summary3[paste("pred_vec_count[",i,"]",sep=""), "97.5%"]
# }
# 
# model_pred_df$count_vec <- data_list3$vec_count
# r_squ_accuracy <- model_summary3["r_squ_regres",]
 #### Graph Pup trend by island ####
 require(ggplot2)

ssta_model_plot<- 
  ggplot()+ 
  # geom_linerange(data = mean_sst_df_july,
  #                aes(x= time,
  #                    ymin = month_sst_med,
  #                    ymax = mean_sst,
  #                    color= condition),
  #                size = 0.25,
  #                alpha = 0.4
  # )+
  
  
  # bayesian results: 
  geom_ribbon(data= mean_sst_df_july,
              aes(x = time,
                  ymin = yearly_sst_lwr,
                  ymax = yearly_sst_upr
              ),
              fill = "gray81",
              color = NA,
              alpha = 0.4
  )+
  
  geom_path(data = mean_sst_df_july,
            aes(x = time,
                y = yearly_sst_med
            ),
            color = "gray10",#01665e
            alpha = 1,
            size = 1.5)+
  
  # #observation 
  # # Dots:
  # 
  
  geom_linerange(data = mean_sst_df_july[c(3:nrow(mean_sst_df_july)-1),],
                 aes(x= time,
                     ymin = yearly_sst_med,
                     ymax = mean_sst),
                 color= "gray10",
                 size = 0.25,
                 alpha = 0.75
  )+
  geom_point(data = mean_sst_df_july[c(3:nrow(mean_sst_df_july)-1),],
             aes(x = time,
                 y = mean_sst,
                 fill = condition
                 # color = condition
             ),
             color = "black",
             shape = 21,
             size = 2.5,
             alpha = 1,
             stroke = 0.5)+
  
  scale_fill_manual(values = c("#0088CC","#E92E20"),
                    breaks = c("Cold","Warm"),
                    na.value = "gray70",
                    guide = guide_legend(override.aes =
                                           list(size = 3))
  )+
  scale_color_manual(values = c("#0088CC","#E92E20"),
                     breaks = c("Cold","Warm"),
                     na.value = "black",
                     guide = guide_legend(show= F)
  )+
  # scale_color_manual(values = c("blue4","darkred"),
  #                    guide = guide_legend(show=FALSE)
  # )+
  
  geom_text(aes(x = mean_sst_df_july$time[which(mean_sst_df_july$year == "2007")],
                y = 19.35),
            fontface = "bold",
            family= "serif",
            color = "gray50",
            label = "italic(mu[SST] == gamma[0]+ gamma[1]*m)",
            parse = TRUE,
            size = 5,
            alpha=1
  )+
  geom_text(aes(x = mean_sst_df_july$time[which(mean_sst_df_july$year == "2007")],
                y = 19.1),
            fontface = "bold",
            color = "gray50",
            family = "serif",
            label = paste("italic(BR)^{2}==",
                          round(r_squ_sst["50%"],
                                digits = 3),
                          "~(",
                          round(r_squ_sst["2.5%"],
                                digits = 3),
                          " - ",
                          round(r_squ_sst["97.5%"],
                                digits = 3),
                          ")",
                          sep = ""),
            parse = TRUE,
            size = 4,
            alpha= 1)+
  
 
  # Settings:
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size=12, margin = margin(t = 0, r = 12, b = 0, l = 0)),
    
    legend.position = c(0.91,0.12),  # c(left, bottom)
    legend.text = element_text(size=12),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    legend.box.background = element_rect(colour = "black"),
    legend.box.margin = unit(c(0.001, 0.001, 0.001, 0.001),units="cm"), 
    strip.text = (element_text(size = 12)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length=unit(0.2, "cm"),
    panel.grid = (element_line(size=0.3)),
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.2, 0.45, 0, 0.012),units="cm"), #top, right, bottom, and left 
    strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
  )+
  scale_x_continuous(limits = c(44025.5,54983.5),
                     breaks = seq(44025.5,54983.5, 365*5),
                     labels = seq(1990, 2020, 5),
                     expand = c(0, 0))+

  scale_y_continuous(#limits = c(17.75,20.6),
    breaks = seq(16,20.5,0.5),
    expand = c(0, 0)
  )+
  coord_cartesian(xlim = c(44010,54983.5),
                  ylim = c(16.5,20))+
  
  labs(x="",
       y="SST GFS-pregnacy period (°C)")


### SAVE AS JOURNAL FIGURE: ###
tiff(filename="Pup_gfs_trend_ssta_yearly_McCue_normal_trunc_mean_100k_BPvalue_1.tiff",
     width=17 ,height=24.5*0.4,units="cm",
     bg="white",
     res=600,
     compression = c("lzw")) 
print(ssta_model_plot) 
dev.off() 


# pred_df<- pred_df[which(mean_sst_df_july$anom_sst_med >= min(hist_abund_df$med_ssta_anom)),]
# 
# index_sst<- which(mean_sst_df_july$anom_sst_med >= min(hist_abund_df$med_ssta_anom) & 
#                     mean_sst_df_july$anom_sst_med <= max(hist_abund_df$med_ssta_anom) ) 


#### Model adjustment ####
##### Model anomaly expected count by ssta ####
require(ggplot2)

require("ggrepel")
###########Plot from ecological model ##########
pred_df$pred_upr_count[which(pred_df$pred_upr_count > 22000)]<-22000
pup_trend_island<- 
  ggplot()+ 
  
  
  # bayesian results: 
  geom_ribbon(data= pred_df,
              aes(x = pred_year,
                  ymin = log(pred_lwr_count),
                  ymax = log(pred_upr_count)
              ),
              fill = "#f6e8c3",#"#6B00B8",#"gray81",
              #color = "gray",
              alpha = 0.7
  )+
  
  
  geom_errorbar(data = pred_df[c(3:nrow(pred_df)-1),],#pred_df[index_sst[c(3:length(index_sst)-1)],],#[c(3:nrow(pred_df)-1),],
                aes(x =pred_year,
                    ymin = log(lwr_count_modeled),
                    ymax = log(upr_count_modeled)
                ),
                color = "#01665e",#8c510a
                alpha= 1,
                size = 0.35,
                width = 0
  )+
  geom_point(data = pred_df[c(3:nrow(pred_df)-1),],#[which(pred_df$pred_year %in% hist_abund_df$year),],
             aes(x =pred_year,
                 y = log(med_count_modeled)
             ),
             color = "#01665e",#8c510a
             fill =  "#5ab4ac",         #"#D0F1BF",
             alpha= 1,
             shape = 21,
             size = 4.5,
             stroke = 0.6)+


  
  
  geom_path(data = pred_df,
            aes(x = pred_year,
                y = log(pred_med_count)),
            color = "#8c510a",#"#6B00B8",#01665e
            alpha= 0.75,
            size = 1.5)+
  
  #observation 
  # Dots:
  geom_errorbar(data = hist_abund_df,#pred_df[index_sst[c(3:length(index_sst)-1)],],#[c(3:nrow(pred_df)-1),],
                aes(x =year,
                    ymin = log(lwr_pup_production),
                    ymax = log(upr_pup_production)
                ),
                color = "#8c510a",#8c510a
                alpha= 1,
                size = 0.4,
                width = 0
  )+
  geom_point(data = hist_abund_df,
             aes(x = year,
                 y = log(mean_pup_production)
                 
             ),
             color = "#8c510a",
             fill= "#dfc27d",#6B00B8", #"black"
             shape = 21,
             size =3, 
             alpha = 1,
             stroke = 1)+
  
  
  geom_text(aes(x = 2000,
                y = 9.6),
            fontface = "bold",
            family= "serif",
            color = "gray50",
            alpha= 1,
            label = "italic(log(P) == beta[0]+ beta[1]*y + beta[2]*y^2)",
            parse = TRUE,
            size = 5
  )+
  geom_text(aes(x = 2000,
                y = 9.2),
            fontface = "bold",
            color = "gray50",
            alpha = 1,
            family = "serif",
            label = paste("italic(BR)^{2}==",
                          round(r_squ_exp_mod["50%"],
                                digits = 3),
                          "~(",
                          round(r_squ_exp_mod["2.5%"],
                                digits = 3),
                          " - ", 
                          round(r_squ_exp_mod["97.5%"],
                                digits = 3),
                          ")",
                          sep = ""),
            parse = TRUE,
            size = 4)+
  
  geom_text(aes(x=1991.08,
                y=9.85),
            label= "a)",
            fontface = "bold",
            size= 5)+
  # Settings:
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size=11, margin = margin(t = 0, r = 10, b = 0, l = 0)),
    
    legend.position ="bottom",
    legend.text = element_text(size=12),
    legend.key.height = unit(0.5,"cm"), 
    legend.title = element_text(size = 12),
    legend.background=element_rect(fill=alpha('white',0)),
    
    strip.text = (element_text(size = 12)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length=unit(.2, "cm"),
    panel.grid = (element_line(size=0.3)),
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.15, 0.45, 0.2, 0.1),units="cm"), #top, right, bottom, and left 
    strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
  )+
  scale_x_continuous(limits = c(1990,2020),
                     breaks = seq(1990, 2020, 5),
                     expand = c(0, 0))+
  # 
  scale_y_continuous(limits = c(7,10),
    breaks = seq(7,10,1),
    label = c("1","3","8","22"),
    expand = c(0, 0))+
   
  coord_cartesian(ylim= c(7,10))+
  labs(x="",
       y=bquote("Pup production (ind. * 1000)"))

ssta_effect_df$pred_lwr_anom_prop[ssta_effect_df$pred_lwr_anom_prop< -0.3] <- -0.3 
ssta_effect_df$pred_upr_anom_prop[ssta_effect_df$pred_upr_anom_prop> 0.6] <- 0.6 
plot_prop_anom_vs_ssta <- 
  ggplot()+
  geom_hline(aes(yintercept = 0),
             linetype="dashed",
             color="black",
             size= 0.25)+
  geom_vline(aes(xintercept=0), 
             linetype="dashed",
             color="black",
             size= 0.25)+
  #model prediction: 
  geom_ribbon(data = ssta_effect_df,
              aes(x= ssta_vec,
                  ymin = pred_lwr_anom_prop,
                  ymax = pred_upr_anom_prop),
              #fill = "#af8dc3",    
              #fill = "#d8b365",
              fill = "#c7eae5",
              alpha = 0.5)+
  
  
  # geom_path(data = ssta_effect_df,
  #           aes(x= ssta_anom,
  #               y = pred_med_anom_prop),
  #           color = "white",
  #           size = 1.5)+
  geom_path(data = ssta_effect_df,
            aes(x= ssta_vec,
                y = pred_med_anom_prop),
            color = "#5ab4ac",
            size = 1.5)+
  # # "Observations":
  
  
  geom_errorbar(data = hist_abund_df,
                aes(x = med_ssta_anom,
                    ymin = lwr_anom_prop,
                    ymax = upr_anom_prop),
                color = "#01665e",#8c510a
                alpha= 1,
                size = 0.35,
                width = 0
  )+
  geom_errorbarh(data = hist_abund_df,
                 aes(xmin = lwr_ssta_anom,
                     xmax = upr_ssta_anom,
                     y = med_anom_prop),
                 color = "#01665e",#8c510a
                 alpha= 1,
                 size = 0.35,
                 height = 0
  )+
  # geom_label(data = hist_abund_df,
  #                  aes(x = med_ssta_anom,
  #                      y = med_anom_prop,
  #                      label = year_lab),
  #                  #shape = 21,
  #                  color = "gray10",#8c510a
  #                  fill =  "#80ED99",
  #                  # force        = 1,
  #                  # nudge_x      = -0.05,
  #                  # direction    = "both",
  #                  # hjust        = 1,
#                  # segment.size = 0.25,
#                  # min.segment.length = 0,
#                  alpha= 0.8,
#                  size = 2.5
# )+
geom_text_repel(data = hist_abund_df[c(1,4,8,9),],
           aes(x = med_ssta_anom,
               y = med_anom_prop,
               label = year),
            color = "black",#8c510a
            force        = 1,
            nudge_y      = -0.1,
            nudge_x =  -0.1,
            direction    = "both",
            hjust        = 1,
           segment.size = 0.25,
           min.segment.length = 0.05,#"#D0F1BF",
           segment.linetype= 4,
           segment.alpha= 0.7,
           alpha= 1,
           size = 2.5
           )+
  geom_text_repel(data = hist_abund_df[c(2,7,10),],
                  aes(x = med_ssta_anom,
                      y = med_anom_prop,
                      label = year),
                  color = "black",#8c510a
                  force        = 1,
                  nudge_y      = -0.1,
                  nudge_x =  0.1,
                  direction    = "both",
                  hjust        = 1,
                  segment.size = 0.25,
                  min.segment.length = 0.05,
                  segment.linetype= 4,
                  segment.alpha= 0.7,#"#D0F1BF",
                  alpha= 1,
                  size = 2.5
  )+
  geom_text_repel(data = hist_abund_df[c(3,6,11),],
                  aes(x = med_ssta_anom,
                      y = med_anom_prop,
                      label = year),
                  color = "black",#8c510a
                  force        = 1,
                  nudge_y      = 0.1,
                  nudge_x =  0.1,
                  direction    = "both",
                  hjust        = 1,
                  segment.size = 0.25,
                  min.segment.length = 0.05,
                  segment.linetype= 4,
                  segment.alpha= 0.7,#"#D0F1BF",
                  alpha= 1,
                  size = 2.5
  )+
  geom_text_repel(data = hist_abund_df[c(5,12),],
                  aes(x = med_ssta_anom,
                      y = med_anom_prop,
                      label = year),
                  color = "black",#8c510a
                  force        = 1,
                  nudge_y      = 0.1,
                  nudge_x =  -0.1,
                  direction    = "both",
                  hjust        = 1,
                  segment.size = 0.25,
                  min.segment.length = 0.05,
                  segment.linetype= 4,
                  segment.alpha= 0.7,#"#D0F1BF",
                  alpha= 1,
                  size = 2.5
  )+
geom_point(data = hist_abund_df,
           aes(x = med_ssta_anom,
               y = med_anom_prop),
           shape = 21,
           color = "#01665e",#8c510a
           fill =  "#5ab4ac",
           stroke = 0.6,
           size = 4.2
)+
  
  geom_text(aes(x = 0.8,
                y = 0.5),
            fontface = "bold",
            color = "gray50",
            family = "serif",
            
            label = "italic(Pa == (zeta[0]+ zeta[1]*SST[A] + zeta[2]*SST[A]^{2} + zeta[3]*SST[A]^{3}))",
            parse = TRUE,
            size = 5
  )+
  geom_text(aes(x =0.9,
                y = 0.4),
            fontface = "bold",
            color = "gray50",
            family = "serif",
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
            size = 4)+
  
  geom_text(aes(x=-0.72,
                y=0.55),
            label= "b)",
            fontface = "bold",
            size= 5)+
  
  scale_x_continuous(#limits = c(-0.3,1.6),
    breaks = seq(-0.8, 1.6, 0.4),
    labels = seq(-0.8, 1.6, 0.4),
    expand = c(0, 0))+
  
  scale_y_continuous(#limits = c(-0.6,0.9),
    breaks = seq(-0.2, 0.5, 0.2),
    labels = seq(-0.2, 0.5, 0.2),
    expand = c(0, 0))+
  
  coord_cartesian(xlim=c(-0.8,1.6),
                  ylim= c(-0.3,0.6))+
  # Settings:
  # Settings:
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size=11),
    axis.title.x = element_text(size=12),
    legend.position = 'none',  # c(left, bottom)
    legend.text = element_text(size=12),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    # aspect.ratio = 1,
    strip.text = (element_text(size = 12)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length = unit(0.2, "cm"), 
    panel.grid = (element_line(size=0.35)),
    panel.grid.minor = element_blank(),
    
    plot.margin=unit(c(0.1, 0.5, 0.15, 0.1),units="cm"), #top, right, bottom, and left 
    strip.text.x = element_text(margin = margin(0.15,0.15,0.15,0.15, "cm"))  # top, right, bottom, left
  )+
  labs(x = expression(paste(SST," anomaly GFS-year mean (°C) ",
                            sep = "")),
       y = "Pup production proportional anomaly")

# reg_exp<- data.frame(pup_prod= c(0,hist_abund_df$med_pup_production,20000))   
# ##### Model accuracy plot ####
# 
# plot_obs_vs_pred_count<-
#   ggplot()+
#   geom_ribbon(data = model_pred_df,
#               aes(x= count_vec/1000,
#                   ymin = lwr_model_reg/1000,
#                   ymax = upr_model_reg/1000),
#               #fill = "#af8dc3",
#               #fill = "#d8b365",
#               fill = "gray81",
#               alpha = 0.8)+
#   
#   geom_path(data = model_pred_df,
#             aes(x= count_vec/1000,
#                 y= med_model_reg/1000),
#             color= "black",
#             size= 1.5,
#             # fill= "gray81",
#             alpha= 1)+
#   geom_line(data = reg_exp,
#             aes(x= pup_prod/1000,
#                 y= pup_prod/1000),
#             #method='lm',
#             color = "cyan",
#             size=0.5,
#             linetype = "solid"
#   )+
#   # geom_errorbar(data= hist_abund_df,
#   #               aes(x=med_pup_production/1000,
#   #                   ymin= lwr_count_modeled/1000,
#   #                   ymax= upr_count_modeled/1000),
#   #               size= 0.5,
#   #               color= "white")+
#   geom_errorbar(data= hist_abund_df,
#                 aes(x=med_pup_production/1000,
#                     ymin= lwr_count_modeled/1000,
#                     ymax= upr_count_modeled/1000),
#                 size= 0.25,
#                 width = 0,
#                 color= "black")+
#   # geom_point(data = hist_abund_df,
#   #            aes(x= med_pup_production/1000,
#   #                y= med_count_modeled/1000),
#   #            color= "white",
#   #            fill = "white",
#   #            shape=21,
#   #            size=4
#   # )+
#   geom_point(data = hist_abund_df,
#              aes(x= med_pup_production/1000,
#                  y= med_count_modeled/1000),
#              color= "gray30",
#              fill = "gray75",
#              shape=21,
#              size=4,
#              stroke= 1,
#              alpha=1)+
#   
#   
#   geom_text(aes(x = 12,
#                 y = 5),
#             fontface = "bold",
#             color = "gray50",
#             family = "serif",
#             
#             label = "italic(PP == eta[0]+eta[1]*P)",
#             parse = TRUE,
#             size = 5
#   )+
#   geom_text(aes(x = 12,
#                 y = 3),
#             fontface = "bold",
#             color = "gray50",
#             family = "serif",
#             label = paste("italic(BR)^{2}==",
#                           round(r_squ_accuracy["50%"],
#                                 digits = 3),
#                           "~(",
#                           round(r_squ_accuracy["2.5%"],
#                                 digits = 3),
#                           " - ",
#                           round(r_squ_accuracy["97.5%"],
#                                 digits = 3),
#                           ")",
#                           sep = ""),
#             parse = TRUE,
#             size = 4)+
#   
#   
#   geom_text(aes(x=0.5,
#                 y=16),
#             label= "c)",
#             fontface = "bold",
#             size= 5)+
#  
#   
#   scale_x_continuous(limits = c(0, 20),
#     breaks = seq(0,16,4),
#     labels = seq(0,16,4),
#     expand = c(0, 0))+
#   scale_y_continuous(limits = c(0,25),
#     breaks = seq(0, 16,4),
#     labels = seq(0, 16, 4),
#     expand = c(0,0))+
#   #
#   # coord_cartesian(xlim= c(0,16),
#   #                  ylim = c(0,16.6))+
#   # #
#   labs(x= "Pup production (ind. * 1000)",
#        y= "Predicted pup production (ind. * 1000)")+
#   
#   theme_bw()+
#   theme(
#     axis.text.x = element_text(size = 12),
#     axis.text.y = element_text(size = 12),
#     axis.title.y = element_text(size=11, margin = margin(t = 1, r = 11.5, b = 0, l = 0)),
#     axis.title.x = element_text(size=12),
#     # aspect.ratio = 1,
#     legend.position = 'none',  # c(left, bottom)
#     legend.text = element_text(size=12),
#     legend.key.height = unit(0.5,"cm"),
#     legend.title = element_blank(),
#     legend.background=element_rect(fill=alpha('white',0)),
#     
#     strip.text = (element_text(size = 12)),
#     axis.ticks = (element_line(size=0.35)),
#     axis.ticks.length = unit(0.2, "cm"),
#     panel.grid = (element_line(size=0.35)),
#     panel.grid.minor = element_blank(),
#     plot.margin=unit(c(0.2, 0.5, 0.1, 0.05),units="cm") #top, right, bottom, and left
#     # strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
#   )
#### ####
require(cowplot)
ecological_model_plot<-plot_grid(pup_trend_island,plot_prop_anom_vs_ssta,nrow = 2)#,plot_obs_vs_pred_count,nrow=3)
### SAVE AS JOURNAL FIGURE: ###
  tiff(filename="pup_trend_ssta_effect_model_accuracy_yearly_McCue_normal_trunc_100k_BPvalue1.tiff",
              width=17,height=24.5*0.8,units="cm",
               bg="white",
               res=600,
               compression = c("lzw"))
print(ecological_model_plot)
dev.off()
 
 write.csv(hist_abund_df, file = "hist_abund_df_e0_1m_all_results_1990-2020_second_negbin.csv")
 write.csv(mean_sst_df_july, file = "mean_sst_df_july_1m_1990-2020_second.csv")

 write.csv(mean_yearly_sst_df, file = "mean_sst_df_yearly_1m_1990-2020_second.csv") 
 write.csv(ssta_effect_df, file = "ssta_effect_df_100_vec.csv") 
 
 write.csv(pred_df, file = "pred_df_1m_1990-2020_second.csv")
write.csv(model_pred_df, file= "model_pred_df_1m_1990-2020.csv") 

hist_abund_df$sst_year_sd<- mean_sst_df_july$sd_sst[which(mean_sst_df_july$year %in% hist_abund_df$year)]

plot_sst_sd_pup_anom<-
  ggplot()+
  geom_hline(aes(yintercept = 0),
             linetype="dashed",
             color="black",
             size= 0.25)+
    geom_point(data = hist_abund_df,
               aes(x= sst_year_sd,
                   y= med_anom_prop),
               shape = 21,
               color = "black",
               fill = "orchid",
               size = 3)+ 
  geom_text(data = hist_abund_df,
            aes(x= sst_year_sd+0.022,
                y= med_anom_prop+0.01,
                label = year))+
  
  labs(x= "SD SST one-year running mean",
       y= "Pup production anomaly")+
  
  scale_x_continuous(limits = c(2.1, 2.9),
                     breaks = seq(2,2.8,0.2),
                     labels = seq(2,2.8,0.2),
                     expand = c(0, 0))+
  scale_y_continuous(limits = c(-0.2,0.4),
                     breaks = seq(-0.2, 0.4,0.1),
                     labels = seq(-0.2, 0.4,0.1),
                     expand = c(0,0))+
  
  theme_bw()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size=11, margin = margin(t = 1, r = 11.5, b = 0, l = 0)),
    axis.title.x = element_text(size=12),
    # aspect.ratio = 1,
    legend.position = 'none',  # c(left, bottom)
    legend.text = element_text(size=12),
    legend.key.height = unit(0.5,"cm"),
    legend.title = element_blank(),
    legend.background=element_rect(fill=alpha('white',0)),
    
    strip.text = (element_text(size = 12)),
    axis.ticks = (element_line(size=0.35)),
    axis.ticks.length = unit(0.2, "cm"),
    panel.grid = (element_line(size=0.35)),
    panel.grid.minor = element_blank(),
    plot.margin=unit(c(0.2, 0.5, 0.1, 0.05),units="cm") #top, right, bottom, and left
    # strip.text.x = element_text(margin = margin(0.1,0.1,0.1,0.1, "cm"))  # top, right, bottom, left
  )
  
  
tiff(filename="pup_prop_anomaly_vs_sd_sst_year_mean.tiff",
     width=17,height=24.5*0.8,units="cm",
     bg="white",
     res=600,
     compression = c("lzw"))
print(plot_sst_sd_pup_anom)
dev.off()
 
####Clean everithing
rm(list=ls())
graphics.off()
gc()

##abrir datos##
require(rpdo)
pdo<- data.frame(download_pdo())
hist(pdo$PDO)
#### MODEL IN JAGS LANGUAJE: ####
# #### Text file to save the model: ####
# sink("PDO_estimation_model.txt")
# 
# cat("model {
#     
#     #Likelihood of counts: 
#     for(i in 1:n_pdo) {
#     pdo[i] ~  dnorm(mu_pdo[i], 1/pow(sd_pdo[i], 2)) 
#     mu_pdo[i]<- alpha+ beta1 * year[i] + beta2 * pow(year[i],2) + beta3 * pow(year[i],3) + e0[i]  
#     e0[i] ~ dnorm(0, 1/pow(sd_e0, 2))
#     sd_pdo[i] ~ dunif(min_sd_pdo, max_sd_pdo)
#     }
#     
#     #Priors
#     #Previas para lo que no conocemos, o bien no hayamos definido como una función
#     
#     alpha ~ dunif(min_alpha, max_alpha)
#     beta1 ~ dunif(min_beta1, max_beta1)
#     beta2 ~ dunif(min_beta2, max_beta2)
#     beta3 ~ dunif(min_beta3, max_beta3)
#     sd_e0 ~ dunif(min_sd_e0, max_sd_e0)
#     }",
#     fill=TRUE)
# sink() # end of JAGS model
# 
# 
# data_list<- list(
#   n_pdo = nrow(pdo),
#   pdo = pdo$PDO,
#   year = (pdo$Year - mean(pdo$Year))/ 100, # Reescalamos la variable
#   
#   min_alpha = -2, max_alpha = 1,   #alpha esta en unidades de conteos (y)
#   min_beta1 = -3, max_beta1 = 2,
#   min_beta2 = 0, max_beta2 = 5,
#   min_beta3 = -10, max_beta3 = 20,
#   min_sd_e0 = 0, max_sd_e0 = 1,
#   min_sd_pdo= 0, max_sd_pdo = 20
# )
# 
# ####Valores para cadenas MCMC####
# n.chains <- 3
# n.iter <- 100000
# n.burnin <- n.iter*0.1
# n.thin <- 10
# 
# #### Parametros a monitorear ####
# monitor <- c("alpha",   #Solo monitoreas
#              "beta1",
#              "beta2",
#              "beta3",
#              "sd_e0"
#              
#              
#              )
# 
# ####Corremos el modelo#####
# require(R2jags)
# 
# out <- jags(data = data_list,
#             # inits = inits.list,
#             parameters.to.save = monitor,
#             model.file = "PDO_estimation_model.txt",
#             n.chains = n.chains,
#             n.thin = n.thin,
#             n.iter = n.iter,
#             n.burnin = n.burnin,
#             DIC = T)
# 
# #### MODEL RESULTS ########
# # Summarize posteriors and statistics:
# model_summary <- print(out, dig=3) # number of decimals for the output.
# 
# # Extract MCMC list: 
# mcmc_list <- as.mcmc(out) # For JAGS
# save(out,
#      model_summary,
#      mcmc_list,
#      file="PDO_estimation_model.RData")


# Libraries for diagnostics:
require(mcmcplots)
require(ggmcmc)  # For MCMC diagnostics
require(coda)    # For MCMC analysis
require(lattice) # For quick posterior ploting and diagnostics

# 
# # Densities:
 
pdo_model<-load("PDO_estimation_model.RData")
x11()
densityplot(mcmc_list)


count_predict <- 500

##HYPER-PREDICTION:
predict_df  <- data.frame(
  year_vec = seq(min(pdo$Year),
                 max(pdo$Year),
                 length.out = count_predict),
  
  year_vec_scaled = seq(min(data_list$year),
                        max(data_list$year),
                        length.out = count_predict))

predict_df$min_count = exp(model_summary$summary["alpha", "2.5%"]+ 
                                   model_summary$summary["beta1", "2.5%"]*predict_df$year_vec_scaled+
                                   model_summary$summary["beta2", "2.5%"]*(predict_df$year_vec_scaled^2)+
                                   model_summary$summary["beta3", "2.5%"]*(predict_df$year_vec_scaled^3))
                                   

predict_df$max_count = exp(model_summary$summary["alpha", "97.5%"]+ 
                                   model_summary$summary["beta1", "97.5%"]*predict_df$year_vec_scaled+
                                   model_summary$summary["beta2", "97.5%"]*(predict_df$year_vec_scaled^2)+
                                   model_summary$summary["beta3", "97.5%"]*(predict_df$year_vec_scaled^3))

predict_df$median_count = exp(model_summary$summary["alpha", "50%"]+ 
                                      model_summary$summary["beta1", "50%"]*predict_df$year_vec_scaled+
                                      model_summary$summary["beta2", "50%"]*(predict_df$year_vec_scaled^2)+
                                      model_summary$summary["beta3", "50%"]*(predict_df$year_vec_scaled^3))
                                      



require(ggplot2)

predict_pdo<-
  ggplot()+
  #hyper prediction
  geom_ribbon(data = predict_df,
              aes(x = year_vec,
                  ymin = min_count,
                  ymax = max_count),
              fill = "lightgray",
              color = "black",
              linetype = "dashed",
              size = 0.2,
              alpha = 0.75)+
  geom_path(data = predict_df,
            aes(x = year_vec,
                y = median_count),
            color = "black",
            size = 1) +

  geom_point(data = pdo,
             aes(x = Year,
                 y = PDO),
             fill = "white",
             color = "black",
             shape = 21,
             size = 1,
             stroke = 0.5)+

  #Observation:



   theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"))+
  scale_x_continuous(limits = c(min(pdo$Year),
                                max(pdo$Year)),
                    # breaks = seq(ceiling(min(pdo$Year)),
                        #          max(pdo$Year),
                         #         4),
                    # labels = as.character(abs(seq(ceiling(min(pup_gfs$year)),
                          #                         max(pup_gfs$year),
                           #                        4))),

                     expand = c(0,0)) +
  # scale_y_continuous(limits = c(0,max(pup_gfs$pup_count)+100),
  #                    expand = c(0,0)) +
  labs(y ="PDO", x= "Year")
#ggtitle("Non hierarchical without e0")




tiff(filename="regresion_count_GFS.tiff",
     width=15,height=15,
     units="cm",
     bg="white",
     res=300,
     compression = c("lzw"))
print(predict_regresion_count)
dev.off()


x11()
plot(predict_regresion_count)










library(powerSurvEpi)
###Sample size calculations assuming 80% power
ssizeEpiCont.default(power = 0.80,   #Postulated power
                     theta = 0.77,  # Hazard ratio
                     sigma2 = 0.9900048, #Variance of the covariate of interest
                     psi = 0.1362,    #proportion of subjects died
                     rho2 = 0,   #sqaure of the multiple correlation coefficient between covariate of interest and other covariates 
                     alpha = 0.05) #Type 1 error rate

###Power calculation for adult model###
powerEpiCont.default(n = 279,
                     theta = 0.77,
                     sigma2 = 0.9900048, 
                     psi = 0.1362,
                     rho2 = 0,
                     alpha = 0.05)
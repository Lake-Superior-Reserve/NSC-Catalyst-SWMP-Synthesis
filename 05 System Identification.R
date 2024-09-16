#Title: System Identification
#Project: NSC SWMP Synthesis Catalyst Class
#Funding: NERRS Science Collaborative
#Author(s): Dr. Kait Reinl; kreinl@wisc.edu; Paul Hanson, pchanson@wisc.edu
#Lake Superior National Estuarine Research Reserve, UW-Madison Division of Extension
#University of Wisconsin-Madison, Center for Limnology

#Data originated from NERRS CDMO: https://cdmo.baruch.sc.edu/ and 
#Data was compiled into csvs for all Reservesusing code from 
#https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/tree/main/R/Data_processing
#This repo is currently private until analyses are complete, but will be made public after
#publication. 

#This code uses data from Apalachicola Reserve. This folder has been updated to the
#repo. Code showing how data was split out for Reserves is in 'Data Processing'. 
#This code can also use data originating from the LAGOS project. This was included so that
#results generated from this code could be compared with results from another poject.

# This code determines whether a timeseries is linear or non-linear and has been adapted from
# code written by Tye Wagner.
# This code uses ideas from Clark, T. J., & Luis, A. D. (2020). Nonlinear population dynamics are ubiquitous in animals. 
# Nature ecology & evolution, 4(1), 75-81.
# And references therein.

#####################################################################################
# Setup the working environmennt

# Load necessary libraries
library(here)       # For setting file paths relative to the root project directory, useful for organizing projects
library(tidyverse)  # A collection of packages for data manipulation, visualization, and analysis (includes dplyr, ggplot2, etc.)
library(rEDM)       # For empirical dynamic modeling, used for nonlinear time series analysis and understanding system dynamics
library(astsa)      # Applied statistical time series analysis, offering tools for time series modeling and forecasting
library(tseries)    # For time series analysis, including functions for unit root tests, ARIMA models, and more
library(astrochron) # For stratigraphic series analysis, particularly useful in time series analysis of geological data

here()

# Source the helper file that has key functions
source('./SystemIDHelpers.R')


# The following packages *might* need to be installed on your computer
#install.packages('astsa',repos="https://CRAN.R-project.org")
#install.packages('rEDM',repos="https://CRAN.R-project.org")
#install.packages('tidyverse',repos="https://CRAN.R-project.org")
#install.packages('tseries',repos="https://CRAN.R-project.org")
#install.packages('astrochron',repos="https://CRAN.R-project.org")
#install.packages('viridis',,repos="https://CRAN.R-project.org")

# Number of random vectors for one of the statistical tests.
# Typically set to 1000, but that takes a long time to run. Set to 100 if just testing the script
nRandVectors = 100  

# Select the source of the timeseries data, which gets passed to the helper functions
# Options are SWMP, create, simulation.
WhichData = 'SWMP' 

###################################
# 0: Load and transform the data
###################################
# There are 2 possible data sources -- SWMP and LAGOS

if (WhichData=='SWMP'){
  # Load SWMP data; this is your data!
  # Need to create a data frame that has edmData$yearFrac and edmData$targetVar
  
  # You must define the observed SWMP variable of interest to analyze; following are examples
  myTargetVar = "turb_mean"   # linear, significant
  #myTargetVar = "ph_mean"     # linear, significant
  #myTargetVar = "do_mgl_mean" # nonlinear, signficant
  #myTargetVar = "spcond_mean" # linear, significant
  
  # Read in the SWMP data -- assumes the following data file
  dat2 <- read.csv("./ExampleData/StationExample.csv")
  # Load the specific variable for analysis
  # e.g., edmData = data.frame(station=dat2$station,year=dat2$year,month=dat2$month,targetVar=dat2$turb_mean)
  # Note, the following syntax allows for the use of user-entered target variable, myTargetVar
  eval(parse(text = paste('edmData = data.frame(station=dat2$station,year=dat2$year,month=dat2$month,targetVar=dat2$',
                          myTargetVar,')',sep="")))
  
  # Remove NAs from a variable, and retain time stamp
  edmData = na.omit(edmData)
  # Create a date column that's handy for plotting
  edmData$yearFrac = edmData$year + edmData$month/12
  
}

###################################
# Transform the data

# Normalize the data
edmData$targetVar_norm = zscore(edmData$targetVar)

# Detrend the data
edmData$targetVar_norm_detrend = astsa::detrend(edmData$targetVar_norm)

# Plot the different versions (i.e., original, normalized, detrended) of the data
myMin = min(edmData$targetVar_norm)
myMax = max(edmData$targetVar)
plot(edmData$yearFrac,edmData$targetVar,ylim=c(myMin,myMax),type='l',
     xlab='Year',ylab=myTargetVar)
abline(h=0,lty=2)
lines(edmData$yearFrac,edmData$targetVar_norm,col='green')
lines(edmData$yearFrac,edmData$targetVar_norm_detrend,col='blue')
legend('topright',legend=c('Original','Normalized','Detrended'),
       col=c('black','green','blue'),lty=c(1,1,1))

###################################
# 1. Calc stats for observed data
###################################

# Calculate E, embedding dimension selected as highest corr coeff, rho, 
# between pred and obs
edmData$E = simplex_extra_fun(edmData)

# Calculate Theta (non-linear tuning parameter), given E, again using rho to select
# A test for non-linear dynamics in the data
edmData$theta = theta_fun(edmData)

# Calculate MAE comparing theta==0 to theta for best MAE
# as MAE (thata==0) - MAE (besttheta/smallest MAE) = how much better S-map did than just simplex
# if MAE(theta==0) > MAE(best), then non-linear model helped
# Get MAE of linear model and lowest MAE of non-linear model
edm2MAEs = MAEsOnly_fun(edmData)
edmDataMAE = nonlin_fun(edmData) # Note that this is a scalar

###################################
# 2: Calc stats for null model
###################################

# Calculate null distribution of ΔMAE to compare our original ΔMAE against
# generate phase-randomized surrogates for assessing significance of nonlinear vs. linear model

# Get e.g., 1000, surrogate samples from the original timeseries
temp.series <- surrogates(edmData$targetVar_norm_detrend,nsim=nRandVectors,verbose=F)

# Plot one example randomized vector, along with the time series
# plot(edmData$yearFrac,edmData$targetVar_norm_detrend,type='l',xlab='Year',ylab=myTargetVar)
# points(edmData$yearFrac,temp.series[,1],pch=20,col='blue')
# legend('topright',legend=c('Original','Random'),
#        col=c('black','blue'),lty=c(1,NA),pch=c(NA,20))

# Calculate the MAE distribution for the null model
print("Calculating MAEs for null model. This can take awhile if null model has lots of samples...")
null_maes <- matrix(NA, ncol=nRandVectors, nrow=1)
for(j in 1:nRandVectors){
  # Simplex to get E
  t_simp <- simplex(time_series = temp.series[,j],
                    E=1:10)
  E <- t_simp$E[which.max(t_simp$rho)]
  # s_map to get ΔMAE
  t_s_map <- s_map(time_series = temp.series[,j],
                   E = E)
  # Paul's addition
  t_s_map_theta = t_s_map$theta[which.max(t_s_map$rho)]
  # Ty's version follows
  null_maes[1, j] <- as.numeric(t_s_map$mae)[which(as.numeric(t_s_map$theta)==0)] - 
    min(as.numeric(t_s_map$mae))
}

# Get the bounds of significance from the analysis of the null model
quant <- apply(null_maes, 1, quantile, 0.95)

# Merge with estimated MAEs from the observed data
# if edmDataMAE (which is theta(0) - theta(best)) > quant, then evidence of non-linearity
# if theta(best) == theta(0), then edmDataMAE will equal 0 and be less that quant and linear
MAEsSig <- data.frame(edmDataMAE, quant)

# Categorize lakes as linear or nonlinear based on significance of ΔMAE compared to null dist'n of ΔMAEs
linear_nonlinear <- MAEsSig %>%
  mutate(nonlin=ifelse(
    edmDataMAE > quant,
    "nonlinear",
    "linear"))

# Plot of the density of null maes plus quant plus edmDataMAE
myDensity = density(null_maes)
plot(density(null_maes),xlim=c(min(myDensity$x),max(max(myDensity$x),edmDataMAE)),main="",ylab="Density of null MAEs")
abline(v=quant,lty=2)
abline(v=edmDataMAE,lty=1,col='blue')
legendText = paste('MAE(l-nl) ',signif(edmDataMAE,3),', ',linear_nonlinear$nonlin,sep="")
legend('topright',c(legendText,"Quantile"),lty=c(1,2),col=c('blue','black'))

###################################
# 3. Calculate predictability 
###################################

# Use forecasting to see whether predictions and observations are significantly related, given the model
dat3 <- edmData
# Determine rho, which is the out of sample forecast skill
dat4 <- dat3 %>%
  mutate(rho=forecast_fun(.data)) %>%
  mutate(p=pred_fun(.data))

# Get one record per lake for merging with significance of nonlinearity
dat5 <- dat4 %>%
  filter(row_number()==1) %>%
  mutate(linear_nonlinear)

# Determine whether classification is significant
dat5 <- dat5 %>%
  mutate(nonlin = factor(nonlin)) %>%
  mutate(significant = factor(ifelse(p < 0.05, "sig", "not sig")) )

# Run S-map in model fitting mode (stats_only = FALSE) to get predictions
modelPrediction <- s_map(time_series = edmData$targetVar_norm_detrend,
                E = min(edmData$E),
                theta = min(edmData$theta),
                tp = 1,
                stats_only = FALSE)

# Get length of prediction vector, which can differ from the observation vector
nModel = length(modelPrediction$model_output[[1]]$Predictions)
nObs = length(edmData$yearFrac)
if (nModel>nObs){
  nStart=1
  nFinish=nObs
}else{
  nStart = nObs-nModel+1
  nFinish = nModel
}

# Plot the observations and predictions from the model 
plot(edmData$yearFrac[nStart:length(edmData$yearFrac)], 
     modelPrediction$model_output[[1]]$Observations[1:nFinish],type='l',xlab='Time', 
     ylab=paste(myTargetVar,'Model results'))
lines(edmData$yearFrac[nStart:length(edmData$yearFrac)],
      modelPrediction$model_output[[1]]$Predictions[1:nFinish],col='blue')
legend('topright',legend=c('Observed','Predicted',paste('rho:',signif(dat5$rho,3),', p:',signif(dat5$p,3))),
       lty=c(1,1,NA),col=c('black','blue'))

# Print the results
print('_________________________________________')
print(paste('Results for ',myTargetVar,sep=""))
print(paste('Simplex & s_map, E: ', dat5$E, ', theta: ', dat5$theta, 
            ', MAEs(lin,nonlin,diff): ',signif(edm2MAEs[1],3), ', ', signif(edm2MAEs[2],3),', ',signif(edm2MAEs[1]-edm2MAEs[2],3) ,sep=""))
print(paste('    Model fit, rho: ', signif(dat5$rho,3), ', p: ', signif(dat5$p,3),sep=""))
print(paste('Linear/nonlin, sig: ',dat5$nonlin, ', ',dat5$significant,sep=""))
      


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

library(package=tidyverse)
library(package=rEDM)
library(package=astsa)
library(package=tseries)
library(astrochron)

# The following packages *might* need to be installed on your computer
#install.packages('astsa',repos="https://CRAN.R-project.org")
#install.packages('rEDM',repos="https://CRAN.R-project.org")
#install.packages('tidyverse',repos="https://CRAN.R-project.org")
#install.packages('tseries',repos="https://CRAN.R-project.org")
#install.packages('astrochron',repos="https://CRAN.R-project.org")
#install.packages('viridis',,repos="https://CRAN.R-project.org")

# Number of random vectors for the null model statistical test
# Typically set to 1000, but that takes a long time to run. Set to 100 if just testing the script
nRandVectors = 100

# Significance threshold for p in final prediction
pSigValue = 0.05 

# Setup structure to hold the stats results
edmDataStats = data.frame(bestE=NA, bestTheta=NA, MAEtheta0=NA, MAEbestTheta=NA, MAEdiff=NA)

###################################
# 0: Load and transform the data
###################################

# Load SWMP data; this is your data!
# Need to create a data frame that has edmData$yearFrac and edmData$targetVar

# You must define the observed SWMP variable of interest to analyze; following are examples
#myTargetVar = "turb_mean"   # linear, significant
#myTargetVar = "ph_mean"     # linear, significant
myTargetVar = "do_mgl_mean" # nonlinear, signficant
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

###################################
# Transform the data -- standard normal and detrend

# Normalize the data

edmData$targetVar_norm = (edmData$targetVar - mean(edmData$targetVar))/sd(edmData$targetVar)

# Detrend the data
edmData$targetVar_norm_detrend = astsa::detrend(edmData$targetVar_norm)

# Plot the different versions (i.e., original, normalized, detrended) of the data
myMin = min(edmData$targetVar_norm) # for y scale
myMax = max(edmData$targetVar) # for y scale
plot(edmData$yearFrac,edmData$targetVar,ylim=c(myMin,myMax),type='l',
     xlab='Year',ylab=myTargetVar)
abline(h=0,lty=2)
lines(edmData$yearFrac,edmData$targetVar_norm,col='green')
lines(edmData$yearFrac,edmData$targetVar_norm_detrend,col='blue')
legend('topright',legend=c('Original','Normalized','Detrended'),
       col=c('black','green','blue'),lty=c(1,1,1))

###################################
# 1. Calc system identification 
#    stats for observed data
###################################

# Setup data frame needed for rEDM
mySimplexData = data.frame(myTime = edmData$yearFrac,
                           Observations = edmData$targetVar_norm_detrend,
                           Predictions = NA)

# For the package, setup the length of the vector for observations and predictions,
# e.g., '1 35' (as a string). A second string could be setup to differentiate 
# training and test data. Here, we will use the same string (i.e., training/test
# data are the same)
vectorRange = c(1,length(mySimplexData$Observations))
vectorRange = paste(vectorRange[1],vectorRange[2])

# Determine embeded dimensions and best E
myEmbedDimension = EmbedDimension( dataFrame = mySimplexData, lib = vectorRange, pred = vectorRange,
                                   columns = "Observations", target = "Observations" )
# Determine best E as highest rho
bestE = myEmbedDimension$E[which.max(myEmbedDimension$rho)]
edmDataStats$bestE = bestE

# Example of using Simplex with best E from EmbedDimensions
mySimplexBestE = Simplex( dataFrame = mySimplexData, lib = vectorRange, pred = vectorRange,
                          E = bestE, columns = "Observations", target = "Observations" )

# Plot the predictions and observations from best E
plot(mySimplexBestE$myTime,mySimplexBestE$Observations,type='l',xlab='Time',ylab='Timeseries')
lines(mySimplexBestE$myTime,mySimplexBestE$Predictions,col='red')
legend('topleft',legend=c('Obs','Pred'),col=c('black','red'),lty=c(1,1))
# 
# Compute the error, given best E
myErrorSimplexBestE = ComputeError( mySimplexBestE$Observations, mySimplexBestE$Predictions)

# Determine whether non-linear model improves predictions
# Calculate MAE comparing theta==0 to theta for best MAE;
# as MAE (thata==0) - MAE (besttheta/smallest MAE) = how much better S-map did than just simplex
# if MAE (theta==0) > MAE(best), then non-linear model helped
# Get MAE of linear model and lowest MAE of non-linear model

# Predict nonlinear using SMap and get the best theta and rho
theta.rho = PredictNonlinear( dataFrame = mySimplexData, lib=vectorRange, pred=vectorRange,
                              E = bestE, embedded = FALSE, columns = "Observations", target = "Observations" )

# Best theta is the one with the highest rho
myBestTheta = theta.rho$Theta[which.max(theta.rho$rho)]
edmDataStats$bestTheta = myBestTheta

# Use the best theta to generate predictions
mySMapBestTheta = SMap( dataFrame = mySimplexData, lib=vectorRange, pred=vectorRange, theta = myBestTheta,
                        E = bestE, embedded = FALSE, columns = "Observations", target = "Observations" )

# For comparison, generate predictions when theta = 0
mySMapTheta0 = SMap( dataFrame = mySimplexData, lib=vectorRange, pred=vectorRange, theta = 0,
                     E = bestE, embedded = FALSE, columns = "Observations", target = "Observations" )

# Plot predictions and observations, using best Theta and Theta=0
plot(mySMapBestTheta$predictions$myTime,mySMapBestTheta$predictions$Observations,type='l',
     xlab = 'Time',ylab = 'Obs and Predicts')
lines(mySMapBestTheta$predictions$myTime,mySMapBestTheta$predictions$Predictions,col='red')
lines(mySMapTheta0$predictions$myTime,mySMapTheta0$predictions$Predictions,col='blue')
legend('topleft',legend=c('Observations','Pred, best theta','Pred, theta=0'),
       col=c('black','red','blue'),lty=c(1,1,1))

# Calculate MAEs for best theta and theta = 0
myErrorSMapBestTheta = ComputeError(mySMapBestTheta$predictions$Observations,mySMapBestTheta$predictions$Predictions)
myErrorSMapTheta0 = ComputeError(mySMapTheta0$predictions$Observations,mySMapTheta0$predictions$Predictions)
# Store the MAEs and difference
edmDataStats$MAEbestTheta = myErrorSMapBestTheta$MAE
edmDataStats$MAEtheta0 = myErrorSMapTheta0$MAE
edmDataStats$MAEdiff = myErrorSMapTheta0$MAE - myErrorSMapBestTheta$MAE

###################################
# 2: Calc stats for null model
###################################

# Calculate null distribution of ΔMAE to compare our original ΔMAE against
# generate phase-randomized surrogates for assessing significance of nonlinear vs. linear model

# Get e.g., 1000 (defined by nRandVectors), surrogate samples from the original timeseries
temp.series <- surrogates(edmData$targetVar_norm_detrend,nsim=nRandVectors,verbose=F)

# Plot one example randomized vector, along with the time series
# plot(edmData$yearFrac,edmData$targetVar_norm_detrend,type='l',xlab='Year',ylab=myTargetVar)
# points(edmData$yearFrac,temp.series[,1],pch=20,col='blue')
# legend('topright',legend=c('Original','Random'),
#        col=c('black','blue'),lty=c(1,NA),pch=c(NA,20))

# Calculate the MAE distribution for the null model
print(paste("Calculating",nRandVectors,
            "MAEs for null model. This can take awhile if null model has lots of samples..."))
null_maes <- matrix(NA, ncol=nRandVectors, nrow=1)
for(j in 1:nRandVectors){
  # Setup data frame by using the jth column of randomized data
  myRandData = data.frame(myTime = edmData$yearFrac,
                          Observations = temp.series[,j],
                          Predictions = NA)
  
  # Determine embeded dimensions and best E
  myEmbedDimensionRand = EmbedDimension( dataFrame = myRandData, lib = vectorRange, pred = vectorRange,
                                         columns = "Observations", target = "Observations",showPlot = FALSE)
  bestERand = myEmbedDimensionRand$E[which.max(myEmbedDimensionRand$rho)]
  
  # Predict nonlinear using SMap and get the best theta and rho
  theta.rhoRand = PredictNonlinear( dataFrame = myRandData, lib=vectorRange, pred=vectorRange,
                                    E = bestERand, embedded = FALSE, columns = "Observations", target = "Observations",showPlot = FALSE )
  myBestThetaRand = theta.rhoRand$Theta[which.max(theta.rhoRand$rho)]
  
  # Use the best theta to generate predictions
  mySMapBestThetaRand = SMap( dataFrame = myRandData, lib=vectorRange, pred=vectorRange, theta = myBestThetaRand,
                              E = bestERand, embedded = FALSE, columns = "Observations", target = "Observations",showPlot = FALSE )
  
  # For comparison, generate predictions when theta = 0
  mySMapTheta0Rand = SMap( dataFrame = myRandData, lib=vectorRange, pred=vectorRange, theta = 0,
                           E = bestERand, embedded = FALSE, columns = "Observations", target = "Observations",showPlot = FALSE )
  
  # Calculate MAEs for best theta and theta = 0
  myErrorSMapBestThetaRand = ComputeError(mySMapBestThetaRand$predictions$Observations,mySMapBestThetaRand$predictions$Predictions)
  myErrorSMapTheta0Rand = ComputeError(mySMapTheta0Rand$predictions$Observations,mySMapTheta0Rand$predictions$Predictions)
  
  # The difference between MAE for theta=0 and bestTheta
  null_maes[1, j] = myErrorSMapTheta0Rand$MAE - myErrorSMapBestThetaRand$MAE
}

# Get the bounds of significance from the analysis of the null model
quant <- apply(null_maes, 1, quantile, 0.95)

# Merge with estimated MAEs from the observed data
# if edmDataMAE (which is theta(0) - theta(best)) > quant, then evidence of non-linearity
# if theta(best) == theta(0), then edmDataMAE will equal 0 and be less that quant and linear
# MAEsSig <- data.frame(edmDataMAE, quant)
edmDataStats$quant=quant

# Categorize lakes as linear or nonlinear based on significance of ΔMAE compared to null dist'n of ΔMAEs
# linear_nonlinear <- edmDataStats %>%
#   mutate(nonlinear=ifelse(
#     MAEdiff > quant,
#     "nonlinear",
#     "linear"))

edmDataStats <- edmDataStats %>%
  mutate(nonlinear=ifelse(
    MAEdiff > quant,
    "nonlinear",
    "linear"))

# Plot of the density of null maes plus quant plus edmDataMAE
myDensity = density(null_maes)
plot(density(null_maes),xlim=c(min(myDensity$x),max(max(myDensity$x),edmDataStats$MAEdiff)),main="",ylab="Density of null MAEs")
abline(v=edmDataStats$quant,lty=2)
abline(v=edmDataStats$MAEdiff,lty=1,col='blue')
legendText = paste('MAE(l-nl) ',signif(edmDataStats$MAEdiff,3),', ',edmDataStats$nonlinear,sep="")
legend('topright',c(legendText,"Quantile"),lty=c(1,2),col=c('blue','black'))

###################################
# 3. Calculate predictability 
###################################

# Use forecasting to see whether predictions and observations are significantly related, given the model
dat3 <- edmData
# Determine rho, which is the out of sample forecast skill

myTempSMap = SMap( dataFrame = mySimplexData, lib=vectorRange, pred=vectorRange, theta = edmDataStats$bestTheta,
                   E = min(edmDataStats$bestE), embedded = FALSE, columns = "Observations", target = "Observations",showPlot = FALSE )

myTempStats = ComputeError(myTempSMap$predictions$Observations,myTempSMap$predictions$Predictions)

# Run the R cor.test to get the p-value
myTempStats2 = cor.test(myTempSMap$predictions$Observations, myTempSMap$predictions$Predictions)

dat4 <- dat3 %>%
  mutate(rho=myTempStats$rho) %>%
  mutate(p=myTempStats2$p.value)

# Get one record per lake for merging with significance of nonlinearity
dat5 <- dat4 %>%
  filter(row_number()==1) %>%
  mutate(nonlin=edmDataStats$nonlinear)

# Determine whether classification is significant
dat5 <- dat5 %>%
  mutate(nonlin = factor(nonlin)) %>%
  mutate(significant = factor(ifelse(p < pSigValue, "sig", "not sig")) )

# Run S-map in model fitting mode (stats_only = FALSE) to get predictions
modelPrediction <- myTempSMap$predictions

# Get length of prediction vector, which can differ from the observation vector
nModel = length(modelPrediction$Predictions)
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
     modelPrediction$Observations[1:nFinish],type='l',xlab='Time', 
     ylab=paste(myTargetVar,'Model results'))
lines(edmData$yearFrac[nStart:length(edmData$yearFrac)],
      modelPrediction$Predictions[1:nFinish],col='blue')
legend('topright',legend=c('Observed','Predicted',paste('rho:',signif(dat5$rho,3),', p:',signif(dat5$p,3))),
       lty=c(1,1,NA),col=c('black','blue'))

# Print the results
print('_________________________________________')
print(paste('Results for ',myTargetVar,sep=""))
#print(paste('Simplex & s_map, E: ', dat5$E, ', theta: ', dat5$theta, 
#            ', MAEs(lin,nonlin,diff): ',signif(edm2MAEs[1],3), ', ', signif(edm2MAEs[2],3),', ',signif(edm2MAEs[1]-edm2MAEs[2],3) ,sep=""))
# Print the MAEs to screen
print(paste('MAEs for bestE:',signif(myErrorSimplexBestE$MAE,3),
            ', bestTheta:',signif(myErrorSMapBestTheta$MAE,3),
            'Theta0:',signif(myErrorSMapTheta0$MAE,3)))
print(paste('    Model fit, rho: ', signif(dat5$rho,3), ', p: ', signif(dat5$p,3),sep=""))
print(paste('Linear/nonlin, sig result: ',dat5$nonlin, ', ',dat5$significant,sep=""))



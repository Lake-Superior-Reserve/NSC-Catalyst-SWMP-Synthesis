#Title: Helper Functions for System Identification
#Project: NSC SWMP Synthesis Catalyst Class
#Funding: NERRS Science Collaborative
#Author(s): Dr. Kait Reinl; kreinl@wisc.edu; Paul Hanson, pchanson@wisc.edu
#Lake Superior National Estuarine Research Reserve, UW-Madison Division of Extension
#University of Wisconsin-Madison, Center for Limnology

# This code has functions that support the System Identification.R code. 
# Code has been adapted from that written by Ty Wagner.
# This code uses ideas from Clark, T. J., & Luis, A. D. (2020). Nonlinear population dynamics are ubiquitous in animals. 
# Nature ecology & evolution, 4(1), 75-81.
# And references therein.

#####################################################################################
# Helper functions for System ID

# Stdz function
zscore <- function(x){
  (x - mean(x))/sd(x)
}

# FUNCTION - Calculate E 
simplex_extra_fun<-function(x){
  output <- Simplex(time_series = x$targetVar_norm_detrend,
                    lib=c(1,length(x$targetVar_norm_detrend)),
                    pred=c(1,length(x$targetVar_norm_detrend)),
                    E=1:10,
                    tau=-1)
  output$E[which.max(output$rho)]
  #output$E[which.min(output$mae)]
}

# FUNCTION = CALCULATE THETA - NONLINEARITY
theta_fun <- function(x){
  output <- s_map(time_series = x$targetVar_norm_detrend,
                  norm = 2,
                  lib=c(1,length(x$targetVar_norm_detrend)),
                  pred=c(1,length(x$targetVar_norm_detrend)),
                  E = min(x$E))
  output$theta[which.max(output$rho)]
  #output$theta[which.min(output$mae)]
}

# FUNCTION = CALCULATE NONLINEARITY FROM THETA's MAE
nonlin_fun <- function(x){
  output <- s_map(time_series = x$targetVar_norm_detrend,
                  norm = 2,
                  lib=c(1,length(x$targetVar_norm_detrend)),
                  pred=c(1,length(x$targetVar_norm_detrend)),
                  E = min(x$E))
  as.numeric(output$mae)[which(as.numeric(output$theta)==0)] - min(as.numeric(output$mae)) # This from tye's code
}

# FUNCTION = CALCULATE return linear and non-linear MAEs
MAEsOnly_fun <- function(x){
  output <- s_map(time_series = x$targetVar_norm_detrend,
                  norm = 2,
                  lib=c(1,length(x$targetVar_norm_detrend)),
                  pred=c(1,length(x$targetVar_norm_detrend)),
                  E = min(x$E))
  c(as.numeric(output$mae)[which(as.numeric(output$theta)==0)],min(as.numeric(output$mae))) # This from tye's code
}

# FUNCTION: CALCULATE Tp - FORECAST SKILL
forecast_fun <- function(x) {
  output <- s_map(time_series = x$targetVar_norm_detrend,
                  E = min(x$E),
                  theta = min(x$theta),
                  tp = 1)
  
  as.numeric(output$rho)[which(output$tp==1)]
}

#FUNCTION: Calculate p values for forecast skill
pred_fun <- function(x) {
  output <- s_map(time_series = x$targetVar_norm_detrend,
                  E = min(x$E),
                  theta = min(x$theta),
                  tp = 1)
  
  output$p_val[which(output$tp==1)]
}

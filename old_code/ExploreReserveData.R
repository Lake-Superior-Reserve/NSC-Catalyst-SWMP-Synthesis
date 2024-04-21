# Script to explore your reserve's data
# Zoo 955, Spring 2024

###########################
# Begin user input section

<<<<<<< Updated upstream
# Set the working directory to wherever this script is located
setwd("/Users/paul/Dropbox/Hanson/Teaching/955 2024")
setwd("C:/Users/kreinl1/OneDrive/OneDrive - UW-Madison/GitHub/NSC-Catalyst-SWMP-Synthesis")

# Identify the meteorological (met), nutrient (nut), and water quality (WQ) data files for your reserve
metFile = "./APA/met_apa.csv"
nutFile = "./APA/nut_apa.csv"
wqFile  = "./APA/wq_apa.csv"
=======
# Identify the meteorological (met), nutrient (nut), and water quality (WQ) data files for your reserve
metFile = "input_data/met_grb.csv"
nutFile = "input_data/nut_grb.csv"
wqFile  = "input_data/wq_grb.csv"
>>>>>>> Stashed changes

# End user input section
###########################

# Load the data into separate data frames
datMet = read.csv(metFile)
datNut = read.csv(nutFile)
datWQ  = read.csv(wqFile)

# Create a handy year fraction variable for each data frame for plotting
datMet$YearFrac = datMet$year + datMet$month/12
datNut$YearFrac = datNut$year + datNut$month/12
datWQ$YearFrac  = datWQ$year + datWQ$month/12

# Each reserve can have multiple sampling stations
# Determine the number of stations for each data frame
uMet = unique(datMet$station)
uNut = unique(datNut$station)
uWQ  = unique(datWQ$station)

# Print out the unique stations for each data frame
cat('Unique met stations: ',uMet,'/n')
cat('Unique nut stations: ',uNut,'/n')
cat('Unique WQ  stations: ',uWQ,'/n')

# Cycle through the met data and plot in natural order
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datMet)[2]-4-1)/3, ' plots...',sep=""))
# Note that the first 4 columns do not contain observational data
for (i in 5:dim(datMet)[2]){
  thisVar = colnames(datMet[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    plot(datMet$YearFrac,datMet[,i],type='l',
         xlab="Year",ylab=paste("Met: ", thisVar,sep=""))
  }
}

# For nutrient and water quality data, there can be several sites
# Setup color scheme for plotting multiple sites per panel
myCol = c('black','red','blue','green')

# Cycle through the nutrient data and plot
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datNut)[2]-4-1)/3, ' plots...',sep=""))
# Cycle through the variables
for (i in 5:dim(datNut)[2]){
  thisVar = colnames(datNut[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    # Cycle through the sites
    myXLim = c(min(datNut$YearFrac,na.rm=TRUE),max(datNut$YearFrac,na.rm=TRUE))
    myYLim = c(min(datNut[,i],na.rm=TRUE),max(datNut[,i],na.rm=TRUE))
    if (any(is.infinite(myYLim))){ # In this case, there are no data and min/max produces Inf
      myYLim = c(0,0)
    }
    for (j in 1:length(uNut)){
      #thisNut = uNut[i]
      whichRows = which(datNut$station==uNut[j])
      if (j==1){
        plot(datNut$YearFrac[whichRows],datNut[whichRows,i],type='l',col=myCol[j],
             xlab="Year",ylab=paste("Nut: ", thisVar,sep=""))#,main=paste('Site ',siteName))
      }else{
        lines(datNut$YearFrac[whichRows],datNut[whichRows,i],col=myCol[j])
      }
    }
    legend('topleft',legend=uNut,lty=c(1,1,1,1,1),col=myCol)
  }
}

# Cycle through the water quality (WQ) data and plot
par(mfrow=c(3,1),lend=2,mai = c(0.25,0.75, 0.08, 0.05),oma = c(2,1,0.2,0.2), cex = 0.8)

print(paste('Generating ', (dim(datWQ)[2]-4-1)/3, ' plots...',sep=""))
# Cycle through the variables
for (i in 5:dim(datWQ)[2]){
  thisVar = colnames(datWQ[i])
  if (thisVar != 'YearFrac'){ # Only plot if it's actually data
    # Cycle through the sites
    # Set the overall X and Y limits
    myXLim = c(min(datWQ$YearFrac,na.rm=TRUE),max(datWQ$YearFrac,na.rm=TRUE))
    myYLim = c(min(datWQ[,i],na.rm=TRUE),max(datWQ[,i],na.rm=TRUE))
    if (any(is.infinite(myYLim))){ # In this case, there are no data and min/max produces Inf
      myYLim = c(0,0)
    }
    for (j in 1:length(uWQ)){ # For each unique site (uWQ), plot the water quality line
      whichRows = which(datWQ$station==uWQ[j])
      if (j==1){
        plot(datWQ$YearFrac[whichRows],datWQ[whichRows,i],xlim=myXLim,ylim=myYLim,type='l',col=myCol[j],
             xlab="Year",ylab=paste("WQ: ", thisVar,sep=""))#,main=paste('Site ',siteName))
      }else{
        lines(datWQ$YearFrac[whichRows],datWQ[whichRows,i],col=myCol[j])
      }
    }
    legend('topleft',legend=uWQ,lty=c(1,1,1,1,1),col=myCol)
  }
}


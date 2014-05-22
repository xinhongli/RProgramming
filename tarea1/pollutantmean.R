# TODO: Add comment
# 
# Author: pacha
###############################################################################

#Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' 
#argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows
		
		## pollutantmean <- function(directory, pollutant, id = 1:332) {
		## 'directory' is a character vector of length 1 indicating
		## the location of the CSV files
		
		## 'pollutant' is a character vector of length 1 indicating
		## the name of the pollutant for which we will calculate the
		## mean; either "sulfate" or "nitrate".
		
		## 'id' is an integer vector indicating the monitor ID numbers
		## to be used
		
		## Return the mean of the pollutant across all monitors list
		## in the 'id' vector (ignoring NA values)
		## }
		
pollutantmean <- function(specdata,pollutant,id=1:332)
	{
	specdata <- "/Users/pacha/Dropbox/R/R\ Programming/tarea1/specdata/"
	data=numeric()
	for(i in id)
		{
		newRead=read.csv(paste(specdata,formatC(i,width=3,flag="0"),".csv",sep=""))
		data=c(data,newRead[[pollutant]])
		}
	return(print(mean(data,na.rm=TRUE),digits=4))
	}

#example output from https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2Fpollutantmean-demo.html
#(to test if the function looks good)

#pollutantmean("specdata", "sulfate", 1:10) ## [1] 4.064
#pollutantmean("specdata", "nitrate", 70:72) ## [1] 1.706
#pollutantmean("specdata", "nitrate", 23) ## [1] 1.281
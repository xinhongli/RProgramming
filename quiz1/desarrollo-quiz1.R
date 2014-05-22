# TODO: Add comment
# 
# Author: pacha
###############################################################################

#cargar y ver archivo
mydata=read.csv("/Users/pacha/Dropbox/R/R\ Programming/quiz1/hw1_data.csv")
mydata

#Question 1
#R was developed by statisticians working at

#The University of Auckland

#Question 2
#The definition of free software consists of four freedoms (freedoms 0 through 3). Which of the following is NOT one of the freedoms that are part of the definition?

#The freedom to run the program, for any purpose. 			
#The freedom to improve the program, and release your improvements to the public, so that the whole community benefits. 			
#The freedom to restrict access to the source code for the software. *
#The freedom to study how the program works, and adapt it to your needs.

#Question 3
#In R the following are all atomic data types EXCEPT 

#integer 			
#complex 			
#numeric 			
#list *

#Question 4
#If I execute the expression x <- 4 in R, what is the class of the object `x' as determined by the `class()' function?

x <- 4
class(x)

#Question 5
#What is the class of the object defined by x <- c(4, TRUE)?

x <- c(4, TRUE)
class(x)

#Question 6
#If I have two vectors x <- c(1,3, 5) and y <- c(3, 2, 10), what is produced by the expression cbind(x, y)?

x <- c(1,3, 5)
y <- c(3, 2, 10)
cbind(x, y)

#Question 7
#A key property of vectors in R is that

#elements of a vector can be of different classes* 			
#a vector cannot have have attributes like dimensions
#elements of a vector all must be of the same class 			
#elements of a vector can only be character or numeric

#Question 8
#Suppose I have a list defined as x <- list(2, "a", "b", TRUE). What does x[[2]] give me?

x <- list(2, "a", "b", TRUE)
x[[2]]

#Question 9
#Suppose I have a vector x <- 1:4 and a vector y <- 2. What is produced by the expression x + y?

x <- 1:4
y <- 2
x + y

#Question 10
#Suppose I have a vector x <- c(17, 14, 4, 5, 13, 12, 10) and I want to set all elements of this vector that are greater than 10 to be equal to 4. What R code achieves this?

x <- c(17, 14, 4, 5, 13, 12, 10)
x[x >= 11] <- 4

#Question 11
#In the dataset provided for this Quiz, what are the column names of the dataset?

names(mydata)

#Question 12
#Extract the first 2 rows of the data frame and print them to the console. What does the output look like?

mydata[c(1,2),]

#Question 13
#How many observations (i.e. rows) are in this data frame?

nrow(mydata)

#Question 14
#Extract the last 2 rows of the data frame and print them to the console. What does the output look like?

mydata[c(152,153),]

#Question 15
#What is the value of Ozone in the 47th row?

mydata[47,]

#Question 16
#How many missing values are in the Ozone column of this data frame?

#para 1 variable
sum(is.na(mydata$Ozone))

#para toda la matriz
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss(mydata)

#Question 17
#What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.

#forma 1
mean(mydata$Ozone,na.rm=TRUE)

#forma 2
sum(mydata$Ozone,na.rm=TRUE)/sum(!is.na(mydata$Ozone))

#Question 18
#Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?

mean(mydata$Solar.R [mydata$Ozone>31 & mydata$Temp>90], na.rm=TRUE)

#Question 19
#What is the mean of "Temp" when "Month" is equal to 6? 

mean(mydata$Temp [mydata$Month==6], na.rm=TRUE)

#Question 20
#What was the maximum ozone value in the month of May (i.e. Month = 5)?

max(mydata$Ozone [mydata$Month==5], na.rm=TRUE)

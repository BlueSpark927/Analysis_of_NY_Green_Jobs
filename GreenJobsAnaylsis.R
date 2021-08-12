#Set working directory (MAC)
#setwd("~/Desktop/foldername")

#Set working directory (PC)
setwd("C:/Users/anson/Downloads/STAT295FinalProject")

##Packages
library(readxl)
library(readr)
library(qualityTools)
library(ggplot2)
library(dplyr)
library(DescTools)

#Import Excel File
df = read_excel("GreenJobs.xls")

#Keeping all the rows in the spreadsheet
df2 = df[c(2:29219),]

#Keeping the columns for INTEREST RATE
df3 = df2[c(8)]

#Mean of INTEREST RATE
mean(df3$'INTEREST RATE')

#Median of INTEREST RATE
median(df3$'INTEREST RATE')

#Range of INTEREST RATE
range(df3$'INTEREST RATE')

#Standard Dev. of INTEREST RATE Adj
N = length(df3$'INTEREST RATE')
S= sqrt(sum((df3$'INTEREST RATE' - mean(df3$'INTEREST RATE'))^2)/(N - 1)); S

#Standard Dev. of INTEREST RATE Adj
sd(df3$'INTEREST RATE')

#variance of INTEREST RATE Adj
N = length(df3$'INTEREST RATE')
S_sq= sum((df3$'INTEREST RATE' - mean(df3$'INTEREST RATE'))^2)/(N - 1); S_sq

#variance Adj
var(df3$'INTEREST RATE')

#Standard Dev. Unadj
sigma = sqrt(sum((df3$'INTEREST RATE' - mean(df3$'INTEREST RATE'))^2)/(N))
sigma

#Variance Unadj
sigma_sq = sum((df3$'INTEREST RATE' - mean(df3$'INTEREST RATE'))^2)/(N)
sigma_sq

#mode
Mode(df3$`INTEREST RATE`)

##############################################################################################################################################################
#Histogram 

hist(df3$'INTEREST RATE')

hist(df3$'INTEREST RATE', main="Interest Rates for Green Job Loans", 
     xlab="Interest Rates", col="dark green")

#Data Summary
summary(df3$'INTEREST RATE')

###############################################################################################################################################################
##To create a reduced target population of size N=8
#I took one each randomly from when each loan was set up (2012 to 2019) 
# Each of the Rows are match by their respective year
#Rows: 74, 16139 , 17042, 8482 ,8944, 25513, 20787, 27868
df4 = data.frame(c(3.49, 2.99 ,3.49, 3.99 ,3.49, 2.99, 3.49, 7.99 ))

hist(df4$'INTEREST RATE')

hist(df4$'INTEREST RATE', main="Interest Rates for Green Job Loans (Reduced Target Population)", 
     xlab="Interest Rates", col="dark blue")

#Mean of INTEREST RATE
mean(df4$'INTEREST RATE')

#Median of INTEREST RATE
median(df4$'INTEREST RATE')

#Range of INTEREST RATE
range(df4$'INTEREST RATE')

#Standard Dev. of INTEREST RATE Adj
N = length(df4$'INTEREST RATE')
S= sqrt(sum((df4$'INTEREST RATE' - mean(df4$'INTEREST RATE'))^2)/(N - 1)); S

#Standard Dev. of INTEREST RATE Adj
sd(df4$'INTEREST RATE')

#variance of INTEREST RATE Adj
N = length(df4$'INTEREST RATE')
S_sq= sum((df4$'INTEREST RATE' - mean(df4$'INTEREST RATE'))^2)/(N - 1); S_sq

#variance Adj
var(df4$'INTEREST RATE')

#Standard Dev. Unadj
sigma = sqrt(sum((df4$'INTEREST RATE' - mean(df4$'INTEREST RATE'))^2)/(N))
sigma

#Variance Unadj
sigma_sq = sum((df4$'INTEREST RATE' - mean(df4$'INTEREST RATE'))^2)/(N)
sigma_sq

#Data Summary
summary(df4$'INTEREST RATE')

#New Column Name
names(df4)[1] <- "INTEREST RATE Sample From Each Completed Year"

#Create a sampling distribution of size n=3
all_combos <- t(combn(df4$`INTEREST RATE Sample From Each Completed Year`,3)); all_combos

#Convert to a data frame
sampl_distr = data.frame(all_combos)

#Calculate sample means for each sample to create sampling distribution
sampl_distr$mean = rowMeans(sampl_distr, na.rm=TRUE)

#Create a frequency histogram of your sampling distribution of the sample mean
hist(sampl_distr$mean)

hist(sampl_distr$mean, main="Interest Rates for Green Job (Sample Distribution)", 
     xlab="Interest Rates", col="green")






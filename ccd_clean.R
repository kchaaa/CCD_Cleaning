# Info --------------------------------------------------------------------------------------------------------------
# 0 = No Occurences
# -1, M = Data Missing
# -2, N, NA = Data NA
# -9 = Data Not Meeting NCES data quality Standards (so prob make it same as NA)
# Cleaning: -99 = NA or Missing

# Setup: Prep --------------------------------------------------------------------------------------------------------------
# rm(list=ls())
# setwd("/Users/crpe/Documents/CCD_Cleaning") # MAC

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Functions ------------------------------------------------------------------------------
read.function <- function(filename){
  data <- read.delim(filename, header = TRUE, sep="\t", na.strings = c('M', 'N'))
}

# Setup: Column Names ------------------------------------------------------------------------------
# We want to create a standard column name list for all of the different years. 
# Main Difference: they add a num for the year the end (ex 10, 09)
# Read in one of the files 
ccd_ex <- read.function("data/sc092a.txt")
# Using the file, get the column names (excluding the first 4 since they don't have the year)
columns <- colnames(ccd_ex[,5:ncol(ccd_ex)])
# Substring the column names so that it doesn't include the 09
columns_fixed <- substring(columns, 1, nchar(columns) - 2)
# Append the new list to include the first 4 columns of the original file
columns_list <- c(colnames(ccd_ex[,1:4]), columns_fixed)

# 2009-10 ------------------------------------------------------------------------------
# Read in the file
ccd_10 <- read.function("data/sc092a.txt")

# Set the column names 
colnames(ccd_10) <- columns_list

# Use for Testing/Error Checking
# temp <- ccd_10
# sapply(ccd_10, mode) # checks the mode
# sapply(ccd_10, class) # checks the class

# Turn columns into numeric columns
ccd_10$LEVEL <- as.numeric(ccd_10$LEVEL)
ccd_10$TITLEI <- as.numeric(ccd_10$TITLEI)
ccd_10$STITLI <- as.numeric(ccd_10$STITLI)
ccd_10$MAGNET <- as.numeric(ccd_10$MAGNET)
ccd_10$CHARTR <- as.numeric(ccd_10$SEASCH)
ccd_10$SHARED <- as.numeric(ccd_10$SHARED)

# Changes the numeric NAs into -99
ccd_10[c("LEVEL", "TITLEI", "STITLI", "MAGNET", "SEASCH", "SHARED")][is.na(ccd_10[c("LEVEL", "TITLEI", "STITLI", "MAGNET", "SEASCH", "SHARED")])] <- -99 
ccd_10[ccd_10 == -2] <- -99
ccd_10[ccd_10 == -1] <- -99

# Add YEAR column
ccd_10$YEAR <- 2010

# Writes to .csv file
write.csv(ccd_10, 'cleaned_data/CCD_200910.csv')


# 2008-09 ------------------------------------------------------------------------------
ccd_09 <- read.function("data/sc081b.txt")


# 2007-08 ------------------------------------------------------------------------------
ccd_08 <- read.function("data/sc071b.txt")


# 2006-07 ------------------------------------------------------------------------------
ccd_07_ai <- read.delim("data/Sc061cai.dat", header = FALSE, sep="\t", as.is=FALSE)
ccd_07_kn <- read.delim("data/Sc061ckn.dat", header=TRUE)
ccd_07_ow <- read.delim("data/Sc061cow.dat", header=TRUE)


# 2005-06 ------------------------------------------------------------------------------
ccd_06_ai <- read.delim("data/Sc051aai.dat", header=TRUE, sep=".")
ccd_06_kn <- read.delim("data/Sc051akn.dat", header=TRUE)
ccd_06_ow <- read.delim("data/Sc051aow.dat", header=TRUE)


# 2004-05 ------------------------------------------------------------------------------
ccd_05_ai <- read.delim("data/Sc041bai.dat", header=TRUE, sep=".")
ccd_05_kn <- read.delim("data/Sc041bkn.dat", header=TRUE)
ccd_05_ow <- read.delim("data/Sc041bow.dat", header=TRUE)


# 2003-04 ------------------------------------------------------------------------------
ccd_04_ai <- read.function("data/Sc031aai.txt")
ccd_04_kn <- read.function("data/Sc031akn.txt")
ccd_04_ow <- read.function("data/Sc031aow.txt")




# 2002-03 ------------------------------------------------------------------------------
ccd_03_ai <- read.function("data/Sc021aai.txt")
ccd_03_kn <- read.function("data/Sc021akn.txt")
ccd_03_ow <- read.function("data/Sc021aow.txt")


# 2001-02 ------------------------------------------------------------------------------
ccd_02_ai <- read.delim("data/Sc011aai.dat", header=TRUE, sep=".")
ccd_02_kn <- read.delim("data/Sc011akn.dat", header=TRUE)
ccd_02_ow <- read.delim("data/Sc011aow.dat", header=TRUE)


# 2000-01 ------------------------------------------------------------------------------
ccd_01_ai <- read.delim("data/Sc001aai.dat", header=TRUE, sep=".")
ccd_01_kn <- read.delim("data/Sc001akn.dat", header=TRUE)
ccd_01_ow <- read.delim("data/Sc001aow.dat", header=TRUE)


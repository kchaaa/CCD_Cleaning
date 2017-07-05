# Info --------------------------------------------------------------------------------------------------------------
# 0 = No Occurences
# -1, M = Data Missing
# -2, N, NA = Data NA
# -9 = Data Not Meeting NCES data quality Standards (so prob make it same as NA)
# Cleaning: -99 = NA or Missing

# Setup: Prep --------------------------------------------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/crpe/Documents/CCD_Cleaning") # MAC

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Functions ------------------------------------------------------------------------------
read.function <- function(filename){
  data <- read.delim(filename, header = TRUE, sep="\t", na.strings = c('M', 'N'))
}

na_to_neg99 <- function(df) {
  # Turn columns into numeric columns
  df$LEVEL <- as.numeric(df$LEVEL)
  df$TITLEI <- as.numeric(df$TITLEI)
  df$STITLI <- as.numeric(df$STITLI)
  df$MAGNET <- as.numeric(df$MAGNET)
  df$CHARTR <- as.numeric(df$CHARTR)
  df$SHARED <- as.numeric(df$SHARED)
  
  # Changes the numeric NAs into -99
  df[na_list][is.na(df[na_list])] <- -99 
  df[df == -2] <- -99
  df[df == -1] <- -99
  
  return(df)
}

# We want to create a standard column name list for all of the different years. 
# Main Difference: -they add a num for the year the end (ex 10, 09)
#                  -AND THEY ALL DON'T INCLUDE THE SAME INFO GAHH   
# Because of that, this only applies to 2008, 2009, 2010
change_colnames <- function(df) {
  # Get the column names (excluding the first 4 since they don't have the year)
  columns <- colnames(df[,5:ncol(df)])
  # Substring the column names so that it doesn't include the 09
  columns_fixed <- substring(columns, 1, nchar(columns) - 2)
  # Append the new list to include the first 4 columns of the original file
  columns_list <- c(colnames(df[,1:4]), columns_fixed)
  # Set the column names 
  colnames(df) <- columns_list
  
  return(df)
}

# Setup: Lists ------------------------------------------------------------------------------
# List of Columns to Turn NA => -99
na_list <- c("LEVEL", "TITLEI", "STITLI", "MAGNET", "CHARTR", "SHARED")

# 2009-10 ------------------------------------------------------------------------------
# Read in the file
ccd_10 <- read.function("data/sc092a.txt")

# Change column names
ccd_10 <- change_colnames(ccd_10)

# temp <- ccd_10 # store into temp to avoid having to re-read the data
# sapply(ccd_10, mode) # checks the mode
# sapply(ccd_10, class) # checks the class

# Turn columns into numeric columns
ccd_10$LEVEL <- as.numeric(ccd_10$LEVEL)
ccd_10$TITLEI <- as.numeric(ccd_10$TITLEI)
ccd_10$STITLI <- as.numeric(ccd_10$STITLI)
ccd_10$MAGNET <- as.numeric(ccd_10$MAGNET)
ccd_10$CHARTR <- as.numeric(ccd_10$CHARTR)
ccd_10$SHARED <- as.numeric(ccd_10$SHARED)

# Changes the numeric NAs into -99
ccd_10[na_list][is.na(ccd_10[na_list])] <- -99 
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
# Read in .dat for states A-I
ccd_07_ai <- read_table("data/Sc061cai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_07_kn <- read_table("data/Sc061ckn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_07_ow <- read_table("data/Sc061cow.dat", col_names = FALSE, col_types = NULL)



# 2005-06 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_06_ai <- read_table("data/Sc051aai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_06_kn <- read_table("data/Sc051akn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_06_ow <- read_table("data/Sc051aow.dat", col_names = FALSE, col_types = NULL)



# 2004-05 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_05_ai <- read_table("data/Sc041bai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_05_kn <- read_table("data/Sc041bkn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_05_ow <- read_table("data/Sc041bow.dat", col_names = FALSE, col_types = NULL)



# 2003-04 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_04_ai <- read_table("data/Sc031aai.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states K-N
ccd_04_kn <- read_table("data/Sc031akn.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states O-W
ccd_04_ow <- read_table("data/Sc031aow.txt", col_names = FALSE, col_types = NULL)



# 2002-03 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_03_ai <- read_table("data/Sc021aai.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states K-N
ccd_03_kn <- read_table("data/Sc021akn.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states O-W
ccd_03_ow <- read_table("data/Sc021aow.txt", col_names = FALSE, col_types = NULL)



# 2001-02 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_02_ai <- read_table("data/Sc011aai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_02_kn <- read_table("data/Sc011akn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_02_ow <- read_table("data/Sc011aow.dat", col_names = FALSE, col_types = NULL)



# 2000-01 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_01_ai <- read_table("data/Sc001aai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_01_kn <- read_table("data/Sc001akn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_01_ow <- read_table("data/Sc001aow.dat", col_names = FALSE, col_types = NULL)



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

## TESTING PURPOSES:
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

# Gets rid of commas
ccd_10$SCHNAM <- gsub(",", " ", ccd_10$SCHNAM)
ccd_10$LEANM <- gsub(",", " ", ccd_10$LEANM)

# Writes to .csv file
write.csv(ccd_10, 'cleaned_data/CCD_200910.csv')

# 2008-09 ------------------------------------------------------------------------------
ccd_09 <- read.function("data/sc081b.txt")

# Change column names
ccd_09 <- change_colnames(ccd_09)

## TESTING PURPOSES:
# temp <- ccd_10 # store into temp to avoid having to re-read the data
# sapply(ccd_10, mode) # checks the mode
# sapply(ccd_10, class) # checks the class

# Turn columns into numeric columns
ccd_09$LEVEL <- as.numeric(ccd_09$LEVEL)
ccd_09$TITLEI <- as.numeric(ccd_09$TITLEI)
ccd_09$STITLI <- as.numeric(ccd_09$STITLI)
ccd_09$MAGNET <- as.numeric(ccd_09$MAGNET)
ccd_09$CHARTR <- as.numeric(ccd_09$CHARTR)
ccd_09$SHARED <- as.numeric(ccd_09$SHARED)

# Changes the numeric NAs into -99
ccd_09[na_list][is.na(ccd_09[na_list])] <- -99 
ccd_09[ccd_09 == -2] <- -99
ccd_09[ccd_09 == -1] <- -99

# Add YEAR column
ccd_09$YEAR <- 2009

# Gets rid of commas
ccd_09$SCHNAM <- gsub(",", " ", ccd_09$SCHNAM)
ccd_09$LEANM <- gsub(",", " ", ccd_09$LEANM)



# Writes to .csv file
write.csv(ccd_09, 'cleaned_data/CCD_200809.csv')

# Merge
ccd_master_list <- merge(ccd_10, ccd_09, all = TRUE)

# 2007-08 ------------------------------------------------------------------------------
ccd_08 <- read.function("data/sc071b.txt")

# Change column names
ccd_08 <- change_colnames(ccd_08)

## TESTING PURPOSES:
# temp <- ccd_08 # store into temp to avoid having to re-read the data
# sapply(ccd_08, mode) # checks the mode
# sapply(ccd_08, class) # checks the class

# Turn columns into numeric columns
ccd_08$LEVEL <- as.numeric(ccd_08$LEVEL)
ccd_08$TITLEI <- as.numeric(ccd_08$TITLEI)
ccd_08$STITLI <- as.numeric(ccd_08$STITLI)
ccd_08$MAGNET <- as.numeric(ccd_08$MAGNET)
ccd_08$CHARTR <- as.numeric(ccd_08$CHARTR)
ccd_08$SHARED <- as.numeric(ccd_08$SHARED)

# Changes the numeric NAs into -99
ccd_08[na_list][is.na(ccd_08[na_list])] <- -99 
ccd_08[ccd_08 == -2] <- -99
ccd_08[ccd_08 == -1] <- -99

# Add YEAR column
ccd_08$YEAR <- 2008

# Gets rid of commas
ccd_08$SCHNAM <- gsub(",", " ", ccd_08$SCHNAM)
ccd_08$LEANM <- gsub(",", " ", ccd_08$LEANM)

# Writes to .csv file
write.csv(ccd_08, 'cleaned_data/CCD_200708.csv')

# Merge
ccd_master_list <- merge(ccd_master_list, ccd_08, all = TRUE)

# 2006-07 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_07_ai <- read_table("data/Sc061cai.dat", col_names = FALSE, col_types = NULL)
ccd_07_ai2 <- transform(ccd_07_ai, X12= colsplit(X12, split = " ", names = c('a', 'b')))

# Read in .dat for states K-N
ccd_07_kn <- read_table("data/Sc061ckn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_07_ow <- read_table("data/Sc061cow.dat", col_names = FALSE, col_types = NULL)


# Merge All of Them Into One
ccd_07 <- merge(ccd_07_ai, ccd_07_kn, all = TRUE)
ccd_07 <- merge(ccd_07, ccd_07_ow, all = TRUE)

# Turn into NAs
ccd_07[ccd_07 == -2] <- -99
ccd_07[ccd_07 == -1] <- -99

# Add YEAR column
ccd_07$YEAR <- 2007

# Writes to .csv file
# write.csv(ccd_07, 'cleaned_data/CCD_200607.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_07, all = TRUE)

# 2005-06 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_06_ai <- read_table("data/Sc051aai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_06_kn <- read_table("data/Sc051akn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_06_ow <- read_table("data/Sc051aow.dat", col_names = FALSE, col_types = NULL)


# Merge All of Them Into One
ccd_06 <- merge(ccd_06_ai, ccd_06_kn, all = TRUE)
ccd_06 <- merge(ccd_06, ccd_06_ow, all = TRUE)

# Turn into NAs
ccd_06[ccd_06 == -2] <- -99
ccd_06[ccd_06 == -1] <- -99

# Add YEAR column
ccd_06$YEAR <- 2006

# Writes to .csv file
# write.csv(ccd_06, 'cleaned_data/CCD_200506.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_06, all = TRUE)

# 2004-05 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_05_ai <- read_table("data/Sc041bai.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states K-N
ccd_05_kn <- read_table("data/Sc041bkn.dat", col_names = FALSE, col_types = NULL)


# Read in .dat for states O-W
ccd_05_ow <- read_table("data/Sc041bow.dat", col_names = FALSE, col_types = NULL)


# Merge All of Them Into One
ccd_05 <- merge(ccd_05_ai, ccd_05_kn, all = TRUE)
ccd_05 <- merge(ccd_05, ccd_05_ow, all = TRUE)

# Turn into NAs
ccd_05[ccd_05 == -2] <- -99
ccd_05[ccd_05 == -1] <- -99

# Add YEAR column
ccd_05$YEAR <- 2005

# Writes to .csv file
# write.csv(ccd_05, 'cleaned_data/CCD_200405.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_05, all = TRUE)

# 2003-04 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_04_ai <- read_table("data/Sc031aai.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states K-N
ccd_04_kn <- read_table("data/Sc031akn.txt", col_names = FALSE, col_types = NULL)


# Read in .txt for states O-W
ccd_04_ow <- read_table("data/Sc031aow.txt", col_names = FALSE, col_types = NULL)


# Merge All of Them Into One
ccd_04 <- merge(ccd_04_ai, ccd_04_kn, all = TRUE)
ccd_04 <- merge(ccd_04, ccd_04_ow, all = TRUE)

# Turn into NAs
ccd_04[ccd_04 == -2] <- -99
ccd_04[ccd_04 == -1] <- -99

# Add YEAR column
ccd_04$YEAR <- 2004

# Writes to .csv file
# write.csv(ccd_04, 'cleaned_data/CCD_200304.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_04, all = TRUE)

# 2002-03 ------------------------------------------------------------------------------
# Read in .txt for states A-I
ccd_03_ai <- read_table("data/Sc021aai.txt", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_03_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_03_ai <- separate(ccd_03_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_ai <- separate(ccd_03_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_ai <- separate(ccd_03_ai, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_ai <- separate(ccd_03_ai, X7.b, c("X7.b","X7.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_ai <- separate(ccd_03_ai, X8, c("X8.a","X8.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_ai <- separate(ccd_03_ai, X8.b, c("X8.b","X8.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_ai <- separate(ccd_03_ai, X8.c, c("X8.c","X8.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_03_ai[ccd_03_ai == -2] <- -99
ccd_03_ai[ccd_03_ai == -1] <- -99
ccd_03_ai[ccd_03_ai == 'M'] <- -99
ccd_03_ai[ccd_03_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE+LCITY', 'LSTATE', 'LZIP', 'LZIP4+OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_ai$YEAR <- 2003
# Gets rid of commas
ccd_03_ai$SCHNAM <- gsub(",", " ", ccd_03_ai$SCHNAM)
ccd_03_ai$LEANM <- gsub(",", " ", ccd_03_ai$LEANM)

# Read in .txt for states K-N
ccd_03_kn <- read_table("data/Sc021akn.txt", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_03_kn <- separate(ccd_03_kn, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_kn <- separate(ccd_03_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_kn <- separate(ccd_03_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_kn <- separate(ccd_03_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_kn <- separate(ccd_03_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_kn <- separate(ccd_03_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_kn <- separate(ccd_03_kn, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
ccd_03_kn <- separate(ccd_03_kn, X13, c("X13.a","X13.b"), sep = 3) # separates something
ccd_03_kn <- separate(ccd_03_kn, X13.b, c("X13.a","X13.b"), sep = 4) # separates something
# Turn NA/Missing => -99
ccd_03_kn[ccd_03_kn == -2] <- -99
ccd_03_kn[ccd_03_kn == -1] <- -99
ccd_03_kn[ccd_03_kn == 'M'] <- -99
ccd_03_kn[ccd_03_kn == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4+OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_kn$YEAR <- 2003
# Gets rid of commas
ccd_03_kn$SCHNAM <- gsub(",", " ", ccd_03_kn$SCHNAM)
ccd_03_kn$LEANM <- gsub(",", " ", ccd_03_kn$LEANM)


# Read in .txt for states O-W
ccd_03_ow <- read_table("data/Sc021aow.txt", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_03_ow$X5 <- NULL
# SEPARATES COLUMNS
ccd_03_ow <- separate(ccd_03_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_03_ow <- separate(ccd_03_ow, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_03_ow <- separate(ccd_03_ow, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_03_ow <- separate(ccd_03_ow, X8.b, c("X8.b","X8.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_03_ow <- separate(ccd_03_ow, X10, c("X10.a","X10.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_03_ow <- separate(ccd_03_ow, X10.b, c("X10.b","X10.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_03_ow <- separate(ccd_03_ow, X10.c, c("X10.c","X10.d"), sep = -2) # separates something
ccd_03_ow <- separate(ccd_03_ow, X13, c("X13.a","X13.b"), sep = 3) # separates something
# Turn NA/Missing => -99
ccd_03_ow[ccd_03_ow == -2] <- -99
ccd_03_ow[ccd_03_ow == -1] <- -99
ccd_03_ow[ccd_03_ow == 'M'] <- -99
ccd_03_ow[ccd_03_ow == 'N'] <- -99
# Set COLNAMES
colnames(ccd_03_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4+OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_03_ow$YEAR <- 2003
# Gets rid of commas
ccd_03_ow$SCHNAM <- gsub(",", " ", ccd_03_ow$SCHNAM)
ccd_03_ow$LEANM <- gsub(",", " ", ccd_03_ow$LEANM)



# Merge All of Them Into One
ccd_03 <- merge(ccd_03_ai, ccd_03_kn, all = TRUE)
ccd_03 <- merge(ccd_03, ccd_03_ow, all = TRUE)

# Turn into NAs
ccd_03[ccd_03 == -2] <- -99
ccd_03[ccd_03 == -1] <- -99

# Add YEAR column
ccd_03$YEAR <- 2003


# Writes to .csv file
# write.csv(ccd_03, 'cleaned_data/CCD_200203.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_03, all = TRUE)

# 2001-02 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_02_ai <- read_table("data/Sc011aai.dat", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_02_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_02_ai <- separate(ccd_02_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_02_ai <- separate(ccd_02_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_ai <- separate(ccd_02_ai, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_ai <- separate(ccd_02_ai, X8.b, c("X8.b","X8.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_ai <- separate(ccd_02_ai, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_ai <- separate(ccd_02_ai, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_ai <- separate(ccd_02_ai, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_ai[ccd_02_ai == -2] <- -99
ccd_02_ai[ccd_02_ai == -1] <- -99
ccd_02_ai[ccd_02_ai == 'M'] <- -99
ccd_02_ai[ccd_02_ai == 'N'] <- -99
# Set COLNAMES
colnames(ccd_02_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE+LCITY', 'LSTATE', 'LZIP', 'LZIP4+OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_02_ai$YEAR <- 2002
# Gets rid of commas
ccd_02_ai$SCHNAM <- gsub(",", " ", ccd_02_ai$SCHNAM)
ccd_02_ai$LEANM <- gsub(",", " ", ccd_02_ai$LEANM)


# Read in .dat for states K-N
ccd_02_kn <- read_table("data/Sc011akn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_02_kn <- separate(ccd_02_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_02_kn <- separate(ccd_02_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_kn <- separate(ccd_02_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_kn <- separate(ccd_02_kn, X7.b, c("X7.b","X7.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_kn <- separate(ccd_02_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_kn <- separate(ccd_02_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_kn <- separate(ccd_02_kn, X9.c, c("X9.c","X9.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_kn[ccd_02_kn == -2] <- -99
ccd_02_kn[ccd_02_kn == -1] <- -99
ccd_02_kn[ccd_02_kn == 'M'] <- -99
ccd_02_kn[ccd_02_kn == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_02_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_02_kn$YEAR <- 2002
# Gets rid of commas
ccd_02_kn$SCHNAM <- gsub(",", " ", ccd_02_kn$SCHNAM)
ccd_02_kn$LEANM <- gsub(",", " ", ccd_02_kn$LEANM)


# Read in .dat for states O-W
ccd_02_ow <- read_table("data/Sc011aow.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_02_ow <- separate(ccd_02_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_02_ow <- separate(ccd_02_ow, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_02_ow <- separate(ccd_02_ow, X6, c("X6.a","X6.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_02_ow <- separate(ccd_02_ow, X6.b, c("X6.b","X6.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_02_ow <- separate(ccd_02_ow, X7, c("X7.a","X7.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_02_ow <- separate(ccd_02_ow, X7.b, c("X7.b","X7.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
ccd_02_ow <- separate(ccd_02_ow, X7.c, c("X7.c","X7.d"), sep = -2) # separates something
# Turn NA/Missing => -99
ccd_02_ow[ccd_02_ow == -2] <- -99
ccd_02_ow[ccd_02_ow == -1] <- -99
ccd_02_ow[ccd_02_ow == 'M'] <- -99
ccd_02_ow[ccd_02_ow == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_02_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE+MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4', 'LSTREE', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_02_ow$YEAR <- 2002
# Gets rid of commas
ccd_02_ow$SCHNAM <- gsub(",", " ", ccd_02_ow$SCHNAM)
ccd_02_ow$LEANM <- gsub(",", " ", ccd_02_ow$LEANM)

# Merge All of Them Into One
ccd_02 <- merge(ccd_02_ai, ccd_02_kn, all = TRUE)
ccd_02 <- merge(ccd_02, ccd_02_ow, all = TRUE)

# Turn into NAs
ccd_02[ccd_02 == -2] <- -99
ccd_02[ccd_02 == -1] <- -99


# Add YEAR column
ccd_02$YEAR <- 2002


# Writes to .csv file
# write.csv(ccd_02, 'cleaned_data/CCD_200102.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_02, all = TRUE)

# 2000-01 ------------------------------------------------------------------------------
# Read in .dat for states A-I
ccd_01_ai <- read_table("data/Sc001aai.dat", col_names = FALSE, col_types = NULL)

# Gets rid of column X4
ccd_01_ai$X4 <- NULL
# SEPARATES COLUMNS
ccd_01_ai <- separate(ccd_01_ai, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_01_ai <- separate(ccd_01_ai, X6, c("X6.a","X6.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_ai <- separate(ccd_01_ai, X8, c("X8.a","X8.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_ai <- separate(ccd_01_ai, X8.b, c("X8.b","X8.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_ai <- separate(ccd_01_ai, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_ai <- separate(ccd_01_ai, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_ai[ccd_01_ai == -2] <- -99
ccd_01_ai[ccd_01_ai == -1] <- -99
ccd_01_ai[ccd_01_ai == 'M'] <- -99
ccd_01_ai[ccd_01_ai == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_ai) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE+LCITY', 'LSTATE', 'LZIP', 'LZIP4+OTHERS',
                         'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 
                         'X15', 'X16', 'X17', 'X18', 'X19', 
                         'X20', 'X21', 'X22', 'X23', 'X24', 
                         'X25', 'X26', 'X27', 'X28', 'X29', 
                         'X30', 'X31', 'X32', 'X34', 'X35', 
                         'X36', 'X37', 'X38', 'X39', 'X40',
                         'X41', 'X42', 'X43')
# Add YEAR column
ccd_01_ai$YEAR <- 2001
# Gets rid of commas
ccd_01_ai$SCHNAM <- gsub(",", " ", ccd_01_ai$SCHNAM)
ccd_01_ai$LEANM <- gsub(",", " ", ccd_01_ai$LEANM)


# Read in .dat for states K-N
ccd_01_kn <- read_table("data/Sc001akn.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_01_kn <- separate(ccd_01_kn, X1, c("X1.a","X1.b"), sep = -6) # separates STID from NCESSCH
ccd_01_kn <- separate(ccd_01_kn, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_kn <- separate(ccd_01_kn, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_kn <- separate(ccd_01_kn, X7.b, c("X7.b","X7.c"), sep = 5) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_kn <- separate(ccd_01_kn, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_kn <- separate(ccd_01_kn, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_kn[ccd_01_kn == -2] <- -99
ccd_01_kn[ccd_01_kn == -1] <- -99
ccd_01_kn[ccd_01_kn == 'M'] <- -99
ccd_01_kn[ccd_01_kn == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_kn) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_01_kn$YEAR <- 2001
# Gets rid of commas
ccd_01_kn$SCHNAM <- gsub(",", " ", ccd_01_kn$SCHNAM)
ccd_01_kn$LEANM <- gsub(",", " ", ccd_01_kn$LEANM)


# Read in .dat for states O-W
ccd_01_ow <- read_table("data/Sc001aow.dat", col_names = FALSE, col_types = NULL)

# SEPARATES COLUMNS
ccd_01_ow <- separate(ccd_01_ow, X1, c("X1.a","X1.b"), sep = -4) # separates STID from NCESSCH
ccd_01_ow <- separate(ccd_01_ow, X5, c("X5.a","X5.b"), sep = 10) # separates PHONE from MSTREE
ccd_01_ow <- separate(ccd_01_ow, X7, c("X7.a","X7.b"), sep = 2) # separates MSTATE from MZIP+MZIP4+LSTREE+LCITY
ccd_01_ow <- separate(ccd_01_ow, X7.b, c("X7.b","X7.c"), sep = 6) # separates MZIP from MZIP4+LSTREE+LCITY
ccd_01_ow <- separate(ccd_01_ow, X9, c("X9.a","X9.b"), sep = 2) # separates LSTATE from LZIP+LZIP4+OTHERS
ccd_01_ow <- separate(ccd_01_ow, X9.b, c("X9.b","X9.c"), sep = 5) # separates LZIP from LZIP4+OTHERS
# Turn NA/Missing => -99
ccd_01_ow[ccd_01_ow == -2] <- -99
ccd_01_ow[ccd_01_ow == -1] <- -99
ccd_01_ow[ccd_01_ow == 'M'] <- -99
ccd_01_ow[ccd_01_ow == 'N'] <- -99
# Changes COLNAMES
colnames(ccd_01_ow) <- c('NCESSCH', 'STID', 'SEASCH', 'LEANM', 'SCHNAM', 
                         'PHONE', 'MSTREE', 'MCITY', 'MSTATE', 'MZIP', 
                         'MZIP4+LSTREE', 'LCITY', 'LSTATE', 'LZIP', 'LZIP4', 'X16', 'X17')
# Add YEAR column
ccd_01_ow$YEAR <- 2001
# Gets rid of commas
ccd_01_ow$SCHNAM <- gsub(",", " ", ccd_01_ow$SCHNAM)
ccd_01_ow$LEANM <- gsub(",", " ", ccd_01_ow$LEANM)


# Merge All of Them Into One
ccd_01 <- merge(ccd_01_ai, ccd_01_kn, all = TRUE)
ccd_01 <- merge(ccd_01, ccd_01_ow, all = TRUE)

# Add YEAR column
ccd_01$YEAR <- 2001


# Writes to .csv file
# write.csv(ccd_01, 'cleaned_data/CCD_200001.csv')

# Merge
# ccd_master_list <- merge(ccd_master_list, ccd_01, all = TRUE)



library(dplyr)
library(tidyverse)
library(readxl)

# replace with file path to your data (provided it has information on tenure start date)
file_path <- 'combined_data.csv'
#file_path <- 'XXXX'
df0 <- read.csv(file_path, stringsAsFactors=T)
df0 <- df0[!is.na(df0$StartYear), ]
# df0 <- df0[-which(df0$Start.Date == ""), ]
sultan_info <- read.csv('ottoman_sultan_tenures.csv', stringsAsFactors=T)
sultan_info <- sultan_info %>% rename(Sultan = 'ï..Sultan')

# substrRight <- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }
# 
# df0['Start Year'] <- as.numeric(substrRight(as.character(df0$Start.Date), 4))

df0['StartYear'] <- as.numeric(df0$StartYear)

merge_sultan <- function(start_year) {
  for (row in 1:nrow(sultan_info)) {
    start <- as.numeric(sultan_info[row, 'Start'])[1]
    end <- as.numeric(sultan_info[row, 'End'])[1]
    if (start_year >= start && start_year <= end) {
      return (as.character(sultan_info[row, 'Sultan'])[1])
    }
  }
}

df0$Sultan <- sapply(df0$StartYear,  merge_sultan)



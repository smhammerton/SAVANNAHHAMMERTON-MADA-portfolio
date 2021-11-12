###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(tidyverse) #for data processing
library(here) #to set paths
library(data.table)

#load data
rawdata <- readRDS('data/raw_data/SympAct_Any_Pos.Rda')

#take a look at the data
dplyr::glimpse(rawdata)

View(rawdata)

#Remove all variables that have Score or Total or FluA or FluB or Dxname or Activity in their name. 
#Also remove the variable Unique.Visit

#removing all variables containing "Score"
datremovescore <- rawdata %>% 
  select(-contains("Score"))
glimpse(datremovescore) #checking

#removing all variables containing "Total"
datremovetotal <- datremovescore %>% 
  select(-contains('Total'))
glimpse(datremovetotal) #checking 

#removing all variables containing "FluA"
datremoveflua <- datremovetotal %>% 
  select(-contains("FluA"))
glimpse(datremoveflua) #checking

#removing all variables containing "FluB"
datremoveflub <- datremoveflua %>% 
  select(-contains("FluB"))
glimpse(datremoveflub) #checking

#removing all variables containing "Dxname"
datremovedx <- datremoveflub %>% 
  select(-contains("DxName"))
glimpse(datremovedx) #checking 

#removing all variables containing "activity"
datremoveact <- datremovedx %>% 
  select(-contains('Activity'))
glimpse(datremoveact) #checking

#removing unique.visit
datremoveunique <- datremoveact %>% 
  select(!Unique.Visit)
glimpse(datremoveunique) #checking

#checking for NAs
naniar::gg_miss_var(datremoveunique)

#since bodytemp is the only variable with NA values, I will just filter out all NA bodytemp values in the final dataset
analysis.data <- datremoveunique %>% 
  filter(!is.na(BodyTemp))

#checking to make sure dataset has 32 variables with 730 observations
dim(analysis.data) %>% print()

#save to new Rds file 
save_data_location <- here::here('data/processed_data/processeddata.rds')
saveRDS(analysis.data, file = save_data_location)


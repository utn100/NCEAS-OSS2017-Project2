
#OSS 2017 Social & Physical Vulnerabilities
#Compile data into a master dataset
#Author: Vanessa Tobias <vanessadtobias@gmail.com>

#### ISSUES TO FIX ####
# DATA TO ADD:
# Sea Level Rise Data
#  - get sea levels for each county*year combination
# Demographic Data
# Social Capital Indices

#### LOAD PACKAGES ####
library(tidyverse)

#### HOUSEKEEPING ####
getwd()
#working directory should be aurora.nceas.ucsb.edu:/home/shares/oss2017/social/DATA

#### READ IN DATA ####

#function to make GEOID into a character vector with the leading zero for Alabama:
# This function was written for use if GEOID is an integer in the dataset.
# If GEOID isn't an integer, use with caution, especially if GEOID is a factor.
fix.geo <- function(df){
  df$GEOID <- as.character(df$GEOID)
  df$GEOID[which(nchar(df$GEOID) == 4)] <- paste0(0, df$GEOID[which(nchar(df$GEOID) == 4)])
  return(df$GEOID)
}

# LAND USE ####
landUse <- read.csv("lulc_all_perc.csv", header = TRUE)   #read data
names(landUse)                                            #check names of variables
class(landUse$GEOID)                                      #check GEOID - it's not a character so it needs to be fixed
landUse$GEOID <- fix.geo(landUse)                         #see function fix.geo (above)

#create table of descriptive names for land use classes:
landUseCat <- data.frame(cat.num = 0:24,
                         cat.class = c("Background",
                                       "Unclassified", #(Cloud, Shadow, etc)
                                       "High_Intensity_Developed",
                                       "Medium_Intensity_Developed",
                                       "Low_Intensity_Developed",
                                       "Open_Space_Developed",
                                       "Cultivated_Land",
                                       "Pasture_Hay",
                                       "Grassland",
                                       "Deciduous_Forest",
                                       "Evergreen_Forest",
                                       "Mixed_Forest",
                                       "Scrub_Shrub",
                                       "Palustrine_Forested_Wetland",
                                       "Palustrine_Scrub_Shrub_Wetland",
                                       "Palustrine_Emergent_Wetland",
                                       "Estuarine_Forested_Wetland",
                                       "Estuarine_Scrub_Shrub_Wetland",
                                       "Estuarine_Emergent_Wetland",
                                       "Unconsolidated_Shore",
                                       "Bare_Land",
                                       "Water",
                                       "Palustrine_Aquatic_Bed",
                                       "Estuarine_Aquatic_Bed",
                                       "Tundra"
                         ),
                         cat.class.short = c("background",
                                              "unclassified",
                                              "dev_high",
                                              "dev_med",
                                              "dev_low",
                                              "open_sp_dev",
                                              "cult",
                                              "pasture",
                                              "grassland",
                                              "forest_decid",
                                              "forest_evergr",
                                              "forest_mixed",
                                              "scrub_srub",
                                              "pal_forest_wet",
                                              "pal_scrub_wet",
                                              "pal_emerg_wet",
                                              "est_forest_wet",
                                              "est_scrub_wet",
                                              "est_emerg_wet",
                                              "unconsol_shore",
                                              "bare_land",
                                              "water",
                                              "pal_aq_bed",
                                              "est_aq_bed",
                                              "tundra"))
View(landUseCat)
#rename columns with descriptive names
# "perc" indicates that these are percent area numbers
names(landUse)[2:25] <- paste("perc", landUseCat$cat.class.short[1:24], sep = "_")
head(landUse)

# ELEVAION ####
elevation <- read.csv("elevation_counties.csv", header = TRUE)
elevation$year <- as.integer(rep(2010, length(elevation$GEOID))) #for now. Actual year = 2013
elevation$GEOID <- fix.geo(elevation)
class(elevation$GEOID)

# SEA LEVEL ####
#seaLevel <- read.csv("/home/shares/oss2017/social/Sea_Level_Data/SL_Counties_Final__CSV_Jul_21_2017.csv",
#                     header = TRUE)

seaLevel <- read.csv("/home/shares/oss2017/social/Sea_Level_Data/sea_level.csv")
View(seaLevel)
names(seaLevel)
class(seaLevel$GEOID)
seaLevel$GEOID <- fix.geo(seaLevel)
#!!!What year(s) is the SLR data for?
# !!!for now we're calling it 2010 so it will join the other 2010 data:
#seaLevel$year <- as.integer(rep(2010, length(seaLevel$GEOID)))

# CLIMATE ####
climate <- read.csv("/home/shares/oss2017/social/NOAA_climatedata/climate.csv",
                    header = TRUE)
names(climate)[9] <- "year"   #rename the year variable so it's easier to merge
#GENERATE THE GEOID COLUMN:
#make the countyfp be 3 characters long
climate$COUNTYFP <- as.character(climate$COUNTYFP)
climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 2)] <- paste0("0", climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 2)])
climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 1)] <- paste0("00", climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 1)])
#make the statefp be two characters long
climate$STATEFP <- as.character(climate$STATEFP)
climate$STATEFP[which(nchar(climate$STATEFP) == 1)] <- paste0("0", climate$STATEFP[which(nchar(climate$STATEFP) == 1)])
#concatenate the state and county fp to make the GEOID
climate$GEOID <- paste0(climate$STATEFP, climate$COUNTYFP)
class(climate$GEOID)
class(climate$year)

# DEMOGRAPHICS ####
demography <- read.csv("/home/shares/oss2017/social/DATA/Dependent_variables.csv",
                 header = TRUE,
                 stringsAsFactors = FALSE)
#make the column names match for the merge:
names(demography)
names(demography)[1] <- "year"
names(demography)[3] <- "GEOID"
#fix the GEOID:
demography$GEOID <- fix.geo(demography)
#fix the population (it's a character with a comma instead of a number)
demography$Pop <- as.numeric(gsub(",", "", demography$Pop))
class(demography$Pop)

# SOCIAL CAPITAL - BONDING ####
bonding <- read.csv("/home/shares/oss2017/social/DATA/SoCI_bond_final.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
names(bonding)
names(bonding)[2] <- "GEOID"
bonding$GEOID <- fix.geo(bonding)
class(bonding$GEOID)
hist(bonding$SoCI_bond)

# SOCIAL CAPITAL - BRIDGING ####
bridging <- read.csv("/home/shares/oss2017/social/DATA/SoCI_bridge_2010.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE)
names(bridging)
names(bridging)[2] <- "GEOID"
bridging$GEOID <- fix.geo(bridging)
class(bridging$GEOID)


# Surface Water Data ####
surfWater <- read.csv("/home/shares/oss2017/social/DATA/CopyOfUSGS_Flow_Data.csv",
                      header = TRUE,
                      stringsAsFactors = FALSE)
class(surfWater$GEOID)
surfWater$GEOID <- fix.geo(surfWater)
names(surfWater)[9] <- "year"

#### MERGE DATASETS ####
#build master dataset off climate because it has the complete record of county*year
# climate, landUse, & elevation have an extra column that we don't need (column 1) so I'm leaving it out
master <- merge(climate[,c(2:6, 9:14)], landUse[,2:27], all.x = TRUE, by = c("GEOID", "year"))

master <- merge(master, elevation[,2:8], all.x = TRUE, by = c("GEOID", "year"))

master <- merge(master, seaLevel[,c(5, 19:20)], all.x = TRUE, by = c("GEOID", "year"))

master <- merge(master, demography, all.x = TRUE, by = c("GEOID", "year"))
#[which(demography$year < 2017)]

master <- merge(master, bonding[, 2:4], all.x = TRUE, by = c("GEOID", "year"))

master <- merge(master, bridging[, 2:4], all.x = TRUE, by = c("GEOID", "year"))

master <- merge(master, surfWater[, 5:9], all.x = TRUE, by = c("GEOID", "year"))

#check the master dataset:
View(master[which(master$year == 2010),])
names(master)

#calculate a couple of interesting variables:
master$pop_change1 <- NA
for(i in unique(master$year)){
  master$pop_change1[which(master$year == i)] <- (master$Pop[which(master$year == (i+1))] - master$Pop[which(master$year == i)])/master$Pop[which(master$year == i)]
}

master$pop_change2 <- NA
for(i in unique(master$year)){
  master$pop_change2[which(master$year == i)] <- (master$Pop[which(master$year == (i+2))] - master$Pop[which(master$year == i)])/master$Pop[which(master$year == i)]
}

master$perc_total_dev <- master$perc_dev_high + master$perc_dev_med + master$perc_dev_low + master$perc_open_sp_dev
master$perc_total_est_wet <- with(master, perc_est_forest_wet +
                                    perc_est_scrub_wet +
                                    perc_est_emerg_wet)

#write.csv(master, "master_20170725.csv")
#write.csv(master, "master_20170726.csv")
write.csv(master, "master_20170727.csv")
write.csv(master, "master.csv")


#PULL OUT YEARS FOR THE DATA SUMMARY FIGURE:
#climate
unique(master$year[which(!is.na(master$ALAND.x)==TRUE)])
#elevation
unique(master$year[which(!is.na(master$mean.el)==TRUE)])
#landUse
unique(master$year[which(!is.na(master$cat2)==TRUE)])
#seaLevel
unique(master$year[which(!is.na(master$Sea_level.mm)==TRUE)])
#demography
unique(master$year[which(!is.na(master$Pop)==TRUE)])
#bonding
unique(master$year[which(!is.na(master$SoCI_bond)==TRUE)])
#bridging
unique(master$year[which(!is.na(master$SoCI_bridge)==TRUE)])
#surfWater
unique(master$year[which(!is.na(master$MeanFlow_ft3_s)==TRUE)])

# SLE <- full_join(elevation, seaLevel, by = "GEOID")
# names(SLE)
# 
# plot(SLE$Statmean , SLE$sd.el)

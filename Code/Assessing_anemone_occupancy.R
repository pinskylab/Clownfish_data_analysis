#Assess anemone occupancy data in preparation for field season 2018
# Goals: assess whether we have good enough estimates of the following that we don't need a survey for them in 2018 (or to determine what kind of survey we might need)
# 1) proportion anemones occupied by A. clarkii (vs. occupied by other clownfish or unoccupied)
# 2) approximate turn-over rates between occupancy by A. clarkii to unoccupied or to other clownfish 
# 3) 
# Want occupancy information for doing metapopulation analyses and models at a variety of spatial scales, including treating the anemone as the patch (where colonization/extinction would become relevant)

#################### Set-up: ####################
#Load relevant libraries
#Load relevant libraries
library(RCurl) #allows running R scripts from GitHub
#library(RMySQL) #might need to load this to connect to the database?
library(dplyr)
library(tidyr)
library(lubridate)
#library(dbplyr)
library(ggplot2)
#library(cowplot)
#library(fields)
library(here)

#################### Functions: ####################
# Functions and constants from my GitHub function/constant collection
#script <- getURL("https://raw.githubusercontent.com/pinskylab/Clownfish_data_analysis/master/Code/Common_constants_and_functions.R", ssl.verifypeer = FALSE)
script <- getURL("https://raw.githubusercontent.com/pinskylab/Clownfish_data_analysis/master/Code/Common_constants_and_functions.R?token=AH_ZQJT5uCEwjgDGYOneY0W6Zdjol5axks5alHmBwA%3D%3D", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Functions from Michelle's GitHub
#helper functions - do various tasks w/database (like assigning dates and site to fish and such)
script <- getURL("https://raw.githubusercontent.com/mstuart1/helpers/master/scripts/helpers.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

#################### Running things: ####################
leyte <- read_db("Leyte") 

anems <- leyte %>% #pull out all anem info
  tbl('anemones') %>%
  select(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, notes) %>%
  collect() 

fish <- leyte %>% #pull out all fish info
  tbl('clownfish') %>%
  select(fish_table_id, anem_table_id, fish_spp, size, notes) %>%
  collect()

dives <- leyte %>% #pull out all dives info
  tbl('diveinfo') %>%
  select(dive_table_id, dive_type, date, site, gps) %>%
  collect()

dives$year <- as.integer(substring(dives$date,1,4)) #add year to dives

# #join together
# allfish <- left_join(allfish_fish, allfish_anems, by="anem_table_id")
# allfish <- left_join(allfish, allfish_dives, by="dive_table_id")

# Anems with no clownfish (from KC 2017 survey): in anem table but with no anem_id and no anem_table_id in clownfish table? (based on notes from 1/17/18 talk)
# filter out anems without anem_ids (suggesting no clownfish...)
anems_noC <- anems %>% filter(is.na(anem_id)) #anems with no anem_id in anem table
anems_noC_dives <- dives %>% filter(dive_table_id %in% anems_noC$dive_table_id) #pull out the relevant dive info
anems_noC_fish <- fish %>% filter(anem_table_id %in% anems_noC$anem_table_id) #pull out any clownfish entries if there are any... (not expecting there to be - but looks like there are... a lot, like 3700... hmmm)

anems_noC_all <- left_join(anems_noC, anems_noC_fish, by = "anem_table_id") #this has multiple rows per anem_id too...
anems_noC_all <- left_join(anems_noC_all, anems_noC_dives, by = "dive_table_id")

# all anems - this has each fish as a row, though... right? not sure this is useful...
anems_all <- left_join(anems, fish, by="anem_table_id")
anems_all <- left_join(anems_all, dives, by="dive_table_id")

##### For 2015, what is the breakdown of anems occupied by each spp of clownfish or not occupied?
anems_all_2015 <- anems_all %>% filter(year == 2015)

# want to pull out all distinct anem_ids so not have multiple rows but not for the NAs (since those are likely different ones)
#test3 <- anems_all_2015 %>% distinct(anem_id, obs_time, date, site) #testing different ways of filtering...
#test4 <- anems_all_2015 %>% distinct(anem_id)
#anems_ids_2015 <- as.data.frame(table(anems_all_2015$anem_id))
#test6 <- as.data.frame(table(test5$anem_table_id))
#test5 %>% filter(anem_table_id == 3143)

# distinct obs (so eliminated multiple rows with measurements of fish, still has multiple rows per anem_table_id if multiple species of clownfish recorded)
anems_2015_distinctmostly <- anems_all_2015 %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year)

# filter for anem_table_ids that have multiple rows in anems_2015_distinctmostly
anems_2015_multiples <- as.data.frame(table(anems_2015_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
anems_2015_multiples2 <- anems_2015_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
anems_2015_twoCspp <- anems_2015_distinctmostly %>% filter(anem_table_id %in% anems_2015_multiples2$Var1) #pull out the info for those... 

# so now find the total number of anems sampled in 2015 (since some have two clownfish spp on them)
total2015anems <- length(anems_2015_distinctmostly$anem_table_id) - length(anems_2015_twoCspp$anem_table_id) #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them

# check that some of the anems sampled in 2015 were not occupied by clownfish
anems_2015_noC <- anems_2015_distinctmostly %>% filter(is.na(fish_spp))

# percentage of anems sampled in 2015 that are occupied by the different clownfish spp or not at all
anems_2015_Cspp <- as.data.frame(table(anems_2015_distinctmostly$fish_spp, useNA = "ifany"))
anems_2015_Cspp$Perc <- anems_2015_Cspp$Freq/total2015anems

## What about all years?
anems_all_distinctmostly <- anems_all %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year) #pull out distinct rows
anems_all_multiples <- as.data.frame(table(anems_all_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
anems_all_multiples2 <- anems_all_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
table(anems_all_multiples2$Freq) #one has three... should investigate more
anems_all_twoCspp <- anems_all_distinctmostly %>% filter(anem_table_id %in% anems_all_multiples2$Var1) #pull out the info for those... 

# total number of anems sampled each year
anems_2017_distinctmostly <- anems_all_distinctmostly %>% filter(year == 2017)
anems_2017_multiples <- as.data.frame(table(anems_2017_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
anems_2017_multiples2 <- anems_2017_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
table(anems_2017_multiples2$Freq) #one has three... should investigate more
anems_2017_twoCspp <- anems_2017_distinctmostly %>% filter(anem_table_id %in% anems_2017_multiples2$Var1) #pull out the info for those... 

total2017anems <- length(anems_2017_distinctmostly$anem_table_id) - length(anems_2017_twoCspp$anem_table_id) - 1 #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them

# percentage of anems sampled in 2017 that are occupied by the different clownfish spp or not at all
anems_2017_Cspp <- as.data.frame(table(anems_2017_distinctmostly$fish_spp, useNA = "ifany"))
anems_2017_Cspp$Perc <- anems_2017_Cspp$Freq/total2017anems


# total number of anems sampled each year
anems_2016_distinctmostly <- anems_all_distinctmostly %>% filter(year == 2016)
anems_2016_multiples <- as.data.frame(table(anems_2016_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
anems_2016_multiples2 <- anems_2016_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
table(anems_2016_multiples2$Freq) #one has three... should investigate more
anems_2016_twoCspp <- anems_2016_distinctmostly %>% filter(anem_table_id %in% anems_2016_multiples2$Var1) #pull out the info for those... 

total2016anems <- length(anems_2016_distinctmostly$anem_table_id) - length(anems_2016_twoCspp$anem_table_id) #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them

# percentage of anems sampled in 2017 that are occupied by the different clownfish spp or not at all
anems_2016_Cspp <- as.data.frame(table(anems_2016_distinctmostly$fish_spp, useNA = "ifany"))
anems_2016_Cspp$Perc <- anems_2016_Cspp$Freq/total2016anems

#table(fish$fish_spp)
##### What proportion of anemones are occupied? (and how that changes by site, time, etc.)


##### What proportion of anemones are occupied by A. clarkii? What about proportion of occupied anemones? (and how those change by site, time, etc.)


##### Can we track individual anemones over time and see if their occupancy and by which species changes?

#Notes on data collection (should check with Michelle/Malin that this is right)
# anemones with ids are only those that have been seen with A. clarkii on them, right? 
# anemones that have ids and are later seen without clarkii are logged: with other clownfish spp. it goes in the fish table with spp, right? What about no fish?
# are anemones without tags with either other species of clownfish or no fish logged at all? (either in the database or on the maps?)
# 


#################### Plots ####################
# Breakdown of anemone occupancy by species in 2015 (>100% coverage b/c a few anemones occupied by two spp)
pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2015_Freq.pdf")) # raw counts
ggplot(data=anems_2015_Cspp, aes(x=Var1, y=Freq)) +
  geom_col() +
  labs(x='species',y='# anemones') +
  ggtitle('2015 anemones') +
  theme_bw() 
dev.off()

pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2015_Perc.pdf")) # percentage 
ggplot(data=anems_2015_Cspp, aes(x=Var1, y=Perc)) +
  geom_col() +
  labs(x='species', y='percent occupancy') +
  ggtitle('2015 anemones') +
  theme_bw() 
dev.off()

# 2016
pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2016_Freq.pdf")) # raw counts
ggplot(data=anems_2016_Cspp, aes(x=Var1, y=Freq)) +
  geom_col() +
  labs(x='species',y='# anemones') +
  ggtitle('2016 anemones') +
  theme_bw() 
dev.off()

pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2016_Perc.pdf")) # percentage 
ggplot(data=anems_2016_Cspp, aes(x=Var1, y=Perc)) +
  geom_col() +
  labs(x='species', y='percent occupancy') +
  ggtitle('2016 anemones') +
  theme_bw() 
dev.off()

# 2017
pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2017_Freq.pdf")) # raw counts
ggplot(data=anems_2017_Cspp, aes(x=Var1, y=Freq)) +
  geom_col() +
  labs(x='species',y='# anemones') +
  ggtitle('2017 anemones') +
  theme_bw() 
dev.off()

pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2017_Perc.pdf")) # percentage 
ggplot(data=anems_2017_Cspp, aes(x=Var1, y=Perc)) +
  geom_col() +
  labs(x='species', y='percent occupancy') +
  ggtitle('2017 anemones') +
  theme_bw() 
dev.off()
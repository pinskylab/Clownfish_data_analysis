#Assess anemone occupancy data in preparation for field season 2018
# Goals: assess whether we have good enough estimates of the following that we don't need a survey for them in 2018 (or to determine what kind of survey we might need)
# 1) proportion anemones occupied by A. clarkii (vs. occupied by other clownfish or unoccupied)
# 2) approximate turn-over rates between occupancy by A. clarkii to unoccupied or to other clownfish 
# 3) 
# Want occupancy information for doing metapopulation analyses and models at a variety of spatial scales, including treating the anemone as the patch (where colonization/extinction would become relevant)

#################### Set-up: ####################
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

# Function that makes data frame with percentage of anems occupied by the various clownfish spp, can specify year and season

findCompOccupancyByYear <- function(year_val, month_vals, divetype_vals, allanemsdf) {
  #filter out just the relevant year and months
  allanemsdf_filt <- allanemsdf %>% filter(year == year_val) # filter by year
  allanemsdf_filt <- allanemsdf_filt %>% filter(month %in% month_vals) #filter by months
  allanemsdf_filt <- allanemsdf_filt %>% filter(divetype %in% divetype_vals) #filter by dive types
  
  #limit to one obs per anemone_table_id (think this through a bit more - ideally this gets one row per fish spp per anem; are there some anems showing up more than that? like if they were visited multiple times per season?)
  allanemsdf_distinct <- allanemsdf_filt %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year, month)
  
  #filter for anem_table_ids that have multiple rows (multiple spp on that anem)
  allanemsdf_mult <- as.data.frame(table(allanemsdf_distinct$anem_table_id)) #make a table of the anem_table_id frequencies
  allanemsdf_mult2 <- allanemsdf_mult %>% filter(Freq >= 2) #filter out just those that appear at least twice
  
  #check that it's just fish_spp that is the different and not anything else...
  sppcheck <- allanemsdf_distinct %>% filter(anem_table_id %in% allanemsdf_mult2$Var1) #pull out just the entries with the anem_table_ids that appear at least twice
  sppcheck_distinct <- sppcheck %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, dive_type, date, site, gps, year, month) #pull out distinct rows for all columns except fish_spp (b/c we assume that is the one differing between the two rows for each anem_table_id)
  n_nonfishsppdiffs <- length(allanemsdf_mult2$Var1) - length(sppcheck_distinct$anem_table_id) #should be 0 if fish_spp is the only thing changing
  if (n_nonfishsppdiffs != 0) { #print a message if the number of obs not accounted for by fish_spp isn't 0 (so something other than fish_spp is different)
    print("Something other than fish_spp is causing multiple rows per anem_table_id.")
  } else {
    print("Fish spp accounted for multiple anem_table_id observations.")
  }
  
  #check that anems aren't being included multiple times (multiple obs per season, say) 
  anem_ids_mult <- as.data.frame(table(sppcheck_distinct$anem_id))
  anem_ids_mult2 <- anem_ids_mult %>% filter(Freq >= 2) #filter out just the anem_ids observed more than once
  n_multanemids <- length(anem_ids_mult2$Var1) #find out how many anems are observed more than once
  print(n_multanemids) 
  #FIGURE OUT HOW TO HANDLE MULTIPLE ANEM SIGHTINGS!
  #test2 <- allfishdf_distinct %>% filter(anem_id == 553)
  
  #find the total number of anems sampled that year and season (since some have multiple clownfish spp on them)
  n_2visits <- allanemsdf_mult2 %>% filter(Freq == 2)
  n_3visits <- allanemsdf_mult2 %>% filter(Freq == 3)
  n_4visits <- allanemsdf_mult2 %>% filter(Freq == 4)
  
  totalanems <- length(allanemsdf_distinct$anem_table_id) - length(n_2visits$Var1) - 2*length(n_3visits$Var1) - 3*length(n_4visits$Var1)
  
  #find frequency and percentage of each spp on anems 
  anems_Cspp <- as.data.frame(table(allanemsdf_distinct$fish_spp, useNA = "ifany"))
  anems_Cspp$Perc <- anems_Cspp$Freq/totalanems
  
  return(anems_Cspp)
  
}

#################### Running things: ####################
# Pull out things from the database
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
dives$month <- as.integer(substring(dives$date,6,7)) #add month to dives

# join together
allanems <- left_join(anems, fish, by = "anem_table_id") #join anems and fish - now data frame has has one row per fish
allanems <- left_join(allanems, dives, by = "dive_table_id") #join dives in too

### Composition of occupancy across time
# Find dive types, months for each season
anems_2012 <- allanems %>% filter(year == 2012) #D,E,M dive types; 5 month
anems_2013 <- allanems %>% filter(year == 2013) #0,C dive types; 5,6 month
anems_2014 <- allanems %>% filter(year == 2014) #0,C dive types; 6,7 month
anems_2015 <- allanems %>% filter(year == 2015) #A,C dive types; 1,2,5,6 month
anems_2016 <- allanems %>% filter(year == 2016) #C,R dive types; 5,6 month
anems_2017 <- allanems %>% filter(year == 2017) #0,A,C,R dive types; 5,6 month

months_all <- c(1,2,3,4,5,6,7,8,9,10,11,12)
dives_all <- c("A","C","D","E","F","M","R","0") 
dives_no0 <- c("A","C","D","E","F","M","R") 
dives_nor0orR <-  c("A","C","D","E","F","M") 
#A=anemone survey dive, C=clownfish collection dive, D=clownfish collection dive with transet (2012), E=clownfish collection with transect and mapping fish survey (2012), 
#F=fish transet with quadrats for coral survey, M=mapping fish survey with transect (2012), R=recapture survey, 0=none of the above
winter_2015 <- c(1,2) #this is the season that was focused anemone surveys
spring_2015 <- c(5,6)
dives_2015 <- c("A","C")

# Do occupancy calcs
anemOcc_2012 <- findCompOccupancyByYear(2012, months_all, dives_all, allanems)
anemOcc_2013 <- findCompOccupancyByYear(2013, months_all, dives_all, allanems)
anemOcc_2013_no0 <- findCompOccupancyByYear(2013, months_all, dives_no0, allanems)
anemOcc_2014 <- findCompOccupancyByYear(2014, months_all, dives_all, allanems)
anemOcc_2014_no0 <- findCompOccupancyByYear(2014, months_all, dives_no0, allanems)
anemOcc_2015winter <- findCompOccupancyByYear(2015, winter_2015, dives_2015, allanems)
anemOcc_2015spring <- findCompOccupancyByYear(2015, spring_2015, dives_2015, allanems)
anemOcc_2016 <- findCompOccupancyByYear(2016, months_all, dives_all, allanems)
anemOcc_2016_no0R <- findCompOccupancyByYear(2016, months_all, dives_no0orR, allanems)
anemOcc_2017 <- findCompOccupancyByYear(2017, months_all, dives_all, allanems)
anemOcc_2017_no0R <- findCompOccupancyByYear(2017, months_all, dives_no0orR, allanems)

# add some things to make dataframes play nicely, join some together to make plotting easier\
anemOcc_2012$Season <- rep("All", length(anemOcc_2012$Var1), 1)
anemOcc_2012$Divetype <- rep("All", length(anemOcc_2012$Var1), 1)
anemOcc_2012$Year <- rep(2012, length(anemOcc_2012$Var1), 1)
anemOcc_2013$Season <- rep("All", length(anemOcc_2013$Var1), 1)
anemOcc_2013$Divetype <- rep("All", length(anemOcc_2013$Var1), 1)
anemOcc_2013$Year <- rep(2013, length(anemOcc_2013$Var1), 1)
anemOcc_2013_no0$Season <- rep("All", length(anemOcc_2013$Var1), 1)
anemOcc_2013_no0$Divetype <- rep("No0", length(anemOcc_2013$Var1), 1)
anemOcc_2013_no0$Year <- rep(2013, length(anemOcc_2013$Var1), 1)
anemOcc_2014$Season <- rep("All", length(anemOcc_2014$Var1), 1)
anemOcc_2014$Divetype <- rep("All", length(anemOcc_2014$Var1), 1)
anemOcc_2014$Year <- rep(2014, length(anemOcc_2014$Var1), 1)
anemOcc_2014_no0$Season <- rep("All", length(anemOcc_2014_no0$Var1), 1)
anemOcc_2014_no0$Divetype <- rep("No0", length(anemOcc_2014_no0$Var1), 1)
anemOcc_2014_no0$Year <- rep(2014, length(anemOcc_2014_no0$Var1), 1)
anemOcc_2015winter$Season <- rep("Winter", length(anemOcc_2015winter$Var1), 1)
anemOcc_2015spring$Season <- rep("Spring", length(anemOcc_2015spring$Var1), 1)
anemOcc_2016$Season <- rep("All", length(anemOcc_2016$Var1), 1)
anemOcc_2016$Divetype <- rep("All", length(anemOcc_2016$Var1), 1)
anemOcc_2016$Year <- rep(2016, length(anemOcc_2016$Var1), 1)
anemOcc_2016_no0R$Season <- rep("All", length(anemOcc_2016_no0R$Var1), 1)
anemOcc_2016_no0R$Divetype <- rep("No0R", length(anemOcc_2016_no0R$Var1), 1)
anemOcc_2016_no0R$Year <- rep(2016, length(anemOcc_2016_no0R$Var1), 1)
anemOcc_2017$Season <- rep("All", length(anemOcc_2017$Var1), 1)
anemOcc_2017$Divetype <- rep("All", length(anemOcc_2017$Var1), 1)
anemOcc_2017$Year <- rep(2017, length(anemOcc_2017$Var1), 1)
anemOcc_2017_no0R$Season <- rep("All", length(anemOcc_2017_no0R$Var1), 1)
anemOcc_2017_no0R$Divetype <- rep("No0R", length(anemOcc_2017_no0R$Var1), 1)
anemOcc_2017_no0R$Year <- rep(2017, length(anemOcc_2017_no0R$Var1), 1)

#bind them all together
anemOcc <- rbind(anemOcc_2012, anemOcc_2013, anemOcc_2013_no0, anemOcc_2014, anemOcc_2014_no0, anemOcc_2015winter, anemOcc_2015spring,
                 anemOcc_2016, anemOcc_2016_no0R, anemOcc_2017, anemOcc_2017_no0R)


# ########### OLD CODE BELOW HERE
# 
# anems_2015_distinctmostly <- anems_all_2015 %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year)
# 
# # filter for anem_table_ids that have multiple rows in anems_2015_distinctmostly
# anems_2015_multiples <- as.data.frame(table(anems_2015_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
# anems_2015_multiples2 <- anems_2015_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
# anems_2015_twoCspp <- anems_2015_distinctmostly %>% filter(anem_table_id %in% anems_2015_multiples2$Var1) #pull out the info for those... 
# 
# # so now find the total number of anems sampled in 2015 (since some have two clownfish spp on them)
# total2015anems <- length(anems_2015_distinctmostly$anem_table_id) - length(anems_2015_twoCspp$anem_table_id) #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them
# 
# # check that some of the anems sampled in 2015 were not occupied by clownfish
# anems_2015_noC <- anems_2015_distinctmostly %>% filter(is.na(fish_spp))
# 
# # percentage of anems sampled in 2015 that are occupied by the different clownfish spp or not at all
# anems_2015_Cspp <- as.data.frame(table(anems_2015_distinctmostly$fish_spp, useNA = "ifany"))
# anems_2015_Cspp$Perc <- anems_2015_Cspp$Freq/total2015anems
# 
# 
# ### What species are together in anems that have multiple?
# 
# # #join together
# # allfish <- left_join(allfish_fish, allfish_anems, by="anem_table_id")
# # allfish <- left_join(allfish, allfish_dives, by="dive_table_id")
# 
# # Anems with no clownfish (from KC 2017 survey): in anem table but with no anem_id and no anem_table_id in clownfish table? (based on notes from 1/17/18 talk)
# # filter out anems without anem_ids (suggesting no clownfish...)
# anems_noC <- anems %>% filter(is.na(anem_id)) #anems with no anem_id in anem table
# anems_noC_dives <- dives %>% filter(dive_table_id %in% anems_noC$dive_table_id) #pull out the relevant dive info
# anems_noC_fish <- fish %>% filter(anem_table_id %in% anems_noC$anem_table_id) #pull out any clownfish entries if there are any... (not expecting there to be - but looks like there are... a lot, like 3700... hmmm)
# 
# anems_noC_all <- left_join(anems_noC, anems_noC_fish, by = "anem_table_id") #this has multiple rows per anem_id too...
# anems_noC_all <- left_join(anems_noC_all, anems_noC_dives, by = "dive_table_id")
# 
# # all anems - this has each fish as a row, though... right? not sure this is useful...
# anems_all <- left_join(anems, fish, by="anem_table_id")
# anems_all <- left_join(anems_all, dives, by="dive_table_id")
# 
# ##### For 2015, what is the breakdown of anems occupied by each spp of clownfish or not occupied?
# anems_all_2015 <- anems_all %>% filter(year == 2015)
# 
# # want to pull out all distinct anem_ids so not have multiple rows but not for the NAs (since those are likely different ones)
# #test3 <- anems_all_2015 %>% distinct(anem_id, obs_time, date, site) #testing different ways of filtering...
# #test4 <- anems_all_2015 %>% distinct(anem_id)
# #anems_ids_2015 <- as.data.frame(table(anems_all_2015$anem_id))
# #test6 <- as.data.frame(table(test5$anem_table_id))
# #test5 %>% filter(anem_table_id == 3143)
# 
# # distinct obs (so eliminated multiple rows with measurements of fish, still has multiple rows per anem_table_id if multiple species of clownfish recorded)
# anems_2015_distinctmostly <- anems_all_2015 %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year)
# 
# # filter for anem_table_ids that have multiple rows in anems_2015_distinctmostly
# anems_2015_multiples <- as.data.frame(table(anems_2015_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
# anems_2015_multiples2 <- anems_2015_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
# anems_2015_twoCspp <- anems_2015_distinctmostly %>% filter(anem_table_id %in% anems_2015_multiples2$Var1) #pull out the info for those... 
# 
# # so now find the total number of anems sampled in 2015 (since some have two clownfish spp on them)
# total2015anems <- length(anems_2015_distinctmostly$anem_table_id) - length(anems_2015_twoCspp$anem_table_id) #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them
# 
# # check that some of the anems sampled in 2015 were not occupied by clownfish
# anems_2015_noC <- anems_2015_distinctmostly %>% filter(is.na(fish_spp))
# 
# # percentage of anems sampled in 2015 that are occupied by the different clownfish spp or not at all
# anems_2015_Cspp <- as.data.frame(table(anems_2015_distinctmostly$fish_spp, useNA = "ifany"))
# anems_2015_Cspp$Perc <- anems_2015_Cspp$Freq/total2015anems
# 
# ## What about all years?
# anems_all_distinctmostly <- anems_all %>% distinct(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp, obs_time, fish_spp, dive_type, date, site, gps, year) #pull out distinct rows
# anems_all_multiples <- as.data.frame(table(anems_all_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
# anems_all_multiples2 <- anems_all_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
# table(anems_all_multiples2$Freq) #one has three... should investigate more
# anems_all_twoCspp <- anems_all_distinctmostly %>% filter(anem_table_id %in% anems_all_multiples2$Var1) #pull out the info for those... 
# 
# # total number of anems sampled each year
# anems_2017_distinctmostly <- anems_all_distinctmostly %>% filter(year == 2017)
# anems_2017_multiples <- as.data.frame(table(anems_2017_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
# anems_2017_multiples2 <- anems_2017_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
# table(anems_2017_multiples2$Freq) #one has three... should investigate more
# anems_2017_twoCspp <- anems_2017_distinctmostly %>% filter(anem_table_id %in% anems_2017_multiples2$Var1) #pull out the info for those... 
# 
# total2017anems <- length(anems_2017_distinctmostly$anem_table_id) - length(anems_2017_twoCspp$anem_table_id) - 1 #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them
# 
# # percentage of anems sampled in 2017 that are occupied by the different clownfish spp or not at all
# anems_2017_Cspp <- as.data.frame(table(anems_2017_distinctmostly$fish_spp, useNA = "ifany"))
# anems_2017_Cspp$Perc <- anems_2017_Cspp$Freq/total2017anems
# 
# 
# # total number of anems sampled each year
# anems_2016_distinctmostly <- anems_all_distinctmostly %>% filter(year == 2016)
# anems_2016_multiples <- as.data.frame(table(anems_2016_distinctmostly$anem_table_id)) #make a table of the anem_table_id frequencies
# anems_2016_multiples2 <- anems_2016_multiples %>% filter(Freq >= 2) #filter out just those that appear at least twice
# table(anems_2016_multiples2$Freq) #one has three... should investigate more
# anems_2016_twoCspp <- anems_2016_distinctmostly %>% filter(anem_table_id %in% anems_2016_multiples2$Var1) #pull out the info for those... 
# 
# total2016anems <- length(anems_2016_distinctmostly$anem_table_id) - length(anems_2016_twoCspp$anem_table_id) #total number of anems surveyed in 2015 (should check...): subtracted the number that have two species of clownfish on them
# 
# # percentage of anems sampled in 2017 that are occupied by the different clownfish spp or not at all
# anems_2016_Cspp <- as.data.frame(table(anems_2016_distinctmostly$fish_spp, useNA = "ifany"))
# anems_2016_Cspp$Perc <- anems_2016_Cspp$Freq/total2016anems

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
ggplot(data=anemOcc_2015, aes(x=Var1, y=Freq)) +
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
# Figure out which fish have been tagged but not clipped

#################### Set-up: ####################
# Load relevant libraries
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

# Load database files (for while in Philippines)
load(file = here("Data", "anem_db.RData"))
load(file = here("Data", "fish_db.RData"))
load(file = here("Data", "dives_db.RData"))
load(file = here("Data", "gps_db.RData"))

# Rename loaded files to make them consistent with other code
dives <- dives_db
anems <- anem_db
fish <- fish_db
gps <- gps_db

#################### Functions: ####################


#################### Running things: ####################
##### Consolidate database info
allfish_fish <- fish %>%
  select(fish_table_id, anem_table_id, fish_spp, sample_id, cap_id, anem_table_id, recap, tag_id, color, size) %>%
  collect() 

allfish_anems <- anems %>%
  select(anem_table_id, dive_table_id, anem_obs, anem_id, old_anem_id) %>%
  collect() %>%
  filter(anem_table_id %in% allfish_fish$anem_table_id)

allfish_dives <- dives %>%
  select(dive_table_id, dive_type, date, site, gps) %>%
  collect() %>%
  filter(dive_table_id %in% allfish_anems$dive_table_id)

# pull out just the year and put that in a separate column
allfish_dives$year <- as.integer(substring(allfish_dives$date,1,4))

#join together
allfish <- left_join(allfish_fish, allfish_anems, by="anem_table_id")
allfish <- left_join(allfish, allfish_dives, by="dive_table_id")

##### Different ways tagged fish could be missing a tag
# WAY 1: they have a tag_id, are a first-time capture (so NA or N in recap), but fail to get a clip
tagged <- allfish %>% filter(!is.na(tag_id)) #pull out all tagged fish
tagged_noClip <- tagged %>% filter(recap != "Y" | is.na(recap)) %>% filter(is.na(sample_id)) #filter out fish that aren't recaps and don't have a sample id

#show abbreviated version of it so can see sites - at Gabas, Magbangon, Visca, Wangag
tagged_noClip %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date)

# WAY 2: fish that have a Y for recapture but have only been scanned once (so were accidentally marked as a recap)
taggedY <- allfish %>% filter(recap == "Y") #pull out all recap = Y fish
recaps1Scan <- allfish %>% filter(tag_id %in% taggedY$tag_id) %>% group_by(tag_id) %>% summarize(nscans = n()) %>% filter(nscans == 1) #for fish with those tags, which ones were only scanned once?
recaps1ScanInfo <- allfish %>% filter(tag_id %in% recaps1Scan$tag_id) # pull out the info for those

#show abbreviated version of it so can see sites - one at Sitio Baybayon, one at Wangag
recaps1ScanInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date)

# ONES THAT MICHELLE HAD FROM LAST YEAR - DNA sequencing failed
tags_needclips <-c(985153000375914, 985153000404510, 986112100164794) #Palanas, Wangag, Magbangon
tags_SitioBaybayon <-c(985153000371791, 985153000373315, 985153000370707, 985153000373354, 985153000372065, 985153000373695, 
                       985153000373978, 985153000371016, 985153000375325, 985153000372250, 985153000373100, 985153000372731,
                       985153000375532, 985153000371164, 985153000374934)


tags_needclipsInfo <- allfish %>% filter(tag_id %in% tags_needclips) %>% group_by(tag_id)
tags_needclipsInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date) %>% group_by(tag_id)

tags_SitioBaybayonInfo <- allfish %>% filter(tag_id %in% tags_SitioBaybayon) 
tags_SitioBaybayonInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date) 

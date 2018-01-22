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
##### Functions from Michelle's GitHub
#helper functions - do various tasks w/database (like assigning dates and site to fish and such)
script <- getURL("https://raw.githubusercontent.com/mstuart1/helpers/master/scripts/helpers.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

#################### Running things: ####################
leyte <- read_db("Leyte") 

anems <- leyte %>%
  tbl('anemones') %>%
  select(anem_table_id, dive_table_id, anem_id, anem_obs, old_anem_id, anem_spp) %>%
  collect() 

fish <- leyte %>%
  tbl('clownfish') %>%
  select(fish_table_id, anem_table_id, fish_spp, size, notes) %>%
  collect()


table(fish$fish_spp)
##### What proportion of anemones are occupied? (and how that changes by site, time, etc.)


##### What proportion of anemones are occupied by A. clarkii? What about proportion of occupied anemones? (and how those change by site, time, etc.)


##### Can we track individual anemones over time and see if their occupancy and by which species changes?

#Notes on data collection (should check with Michelle/Malin that this is right)
# anemones with ids are only those that have been seen with A. clarkii on them, right? 
# anemones that have ids and are later seen without clarkii are logged: with other clownfish spp. it goes in the fish table with spp, right? What about no fish?
# are anemones without tags with either other species of clownfish or no fish logged at all? (either in the database or on the maps?)
# 




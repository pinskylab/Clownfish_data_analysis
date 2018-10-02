# Some data characterization and plots - haven't tried any of this...

#################### Set-up: ####################
#Load relevant libraries
library(RCurl) #allows running R scripts from GitHub
library(RMySQL) #might need to load this to connect to the database?
library(dplyr)
library(tidyr)
library(lubridate)
#library(dbplyr)
library(ggplot2)
library(here)

#################### Functions: ####################
# Functions and constants from my GitHub function/constant collection - could maybe just source this?
source(file=here::here('Code', 'Common_constants_and_functions.R'))

# Functions from Michelle's GitHub helpers script, updated version in field repository
script <- getURL("https://raw.githubusercontent.com/pinskylab/field/master/scripts/field_helpers.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

#################### Running things: ####################
##### Pulling and setting up data
leyte <- read_db("Leyte")

#pull all clownfish observations
allfish_fish <- leyte %>%  
  tbl("clownfish") %>%
  select(fish_table_id, anem_table_id, fish_spp, sample_id, gen_id, anem_table_id, recap, tag_id, color, size) %>%
  collect() #%>%
#filter(!is.na(tag_id)) 

#and their corresponding anemones
allfish_anems <- leyte %>%  
  tbl("anemones") %>%
  select(anem_table_id, dive_table_id, anem_obs, anem_id, old_anem_id) %>%
  collect() %>%
  filter(anem_table_id %in% allfish_fish$anem_table_id)

#and the corresponding dives
allfish_dives <- leyte %>%  
  tbl("diveinfo") %>%
  select(dive_table_id, dive_type, date, site, gps) %>%
  collect() %>%
  filter(dive_table_id %in% allfish_anems$dive_table_id)

# pull out just the year and put that in a separate column
allfish_dives$year <- as.integer(substring(allfish_dives$date,1,4))

#join together
allfish <- left_join(allfish_fish, allfish_anems, by="anem_table_id")
allfish <- left_join(allfish, allfish_dives, by="dive_table_id")

allfish$size <- as.numeric(allfish$size) #make size numeric (rather than a chr) so can do means and such

##### Sort fish with tag_ids, sample_ids, gen_ids 
# Tagged fish caught or tagged by year (excluding recaptures w/in a year)
allfish_tag <- allfish %>% 
  filter(!is.na(tag_id)) %>%
  distinct(tag_id, year, .keep_all = TRUE)

# Fish with a sample id 
allfish_sample <- allfish %>%
  filter(!is.na(sample_id)) %>%
  distinct(sample_id, year, .keep_all = TRUE)

# Fish with a genotype id
allfish_gen <- allfish %>%
  filter(!is.na(gen_id)) %>%
  distinct(gen_id, year, .keep_all = TRUE)

# Fish with both tag and genotype
allfish_gentag <- allfish %>%
  filter(!is.na(gen_id) & !is.na(tag_id)) %>%
  distinct(gen_id, tag_id, year, .keep_all = TRUE)

##### Summarize number of fish with each data type per year, join together for plotting ease
tag_df <- data.frame(allfish_tag %>% group_by(year) %>% summarize(nfish = n()))
tag_df$data_type <- rep("tag", length(tag_df$nfish))
gen_df <- data.frame(allfish_gen %>% group_by(year) %>% summarize(nfish = n()))
gen_df$data_type <- rep("gen", length(gen_df$nfish))
gentag_df <- data.frame(allfish_gentag %>% group_by(year) %>% summarize(nfish = n()))
gentag_df$data_type <- rep("gen+tag", length(gentag_df$nfish))
sample_df <- data.frame(allfish_sample %>% group_by(year) %>% summarize(nfish = n()))
sample_df$data_type <- rep("sample", length(sample_df$nfish))

# Data frame with 0s for each year without that data type, to make plot bars more even (will have to update once 2016-2018 genetic data gets processed)
missing_data <- data.frame(year = c(2012,2013,2014,2017,2018,2012,2013,2014,2017,2018), data_type = c("tag", "tag", "tag", "gen", "gen", "gen+tag", "gen+tag", "gen+tag", "gen+tag", "gen+tag"), nfish = rep(0,10))

# Joining for plotting ease
fish_datatypes <- rbind(tag_df, gen_df, gentag_df, sample_df, missing_data)

#################### Plots: ####################
# Number of fish with each data type by year (recaptures by any single data type within a year removed)
pdf(file = here::here('Plots/Data_characterization', 'MarkedFishByYear.pdf'))
ggplot(data = fish_datatypes, aes(x=year, y=nfish, fill=data_type)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle('Number of fish with sample, genotype, and/or tag by year') +
  theme_bw()
dev.off()

# Number of fish tagged or recaptured by year (recaptures within a year removed)
pdf(file = here::here('Plots/Data_characterization', 'FishWithTagsByYear.pdf'))
ggplot(data = allfish_tag, aes(x=year)) +
  geom_histogram(binwidth=1, col='white') + 
  ggtitle('Number of tagged fish (recaptured or tagged) by year') +
  theme_bw()
dev.off()

#################### Saving output: ####################
save(fish_datatypes, file=here::here('Data', 'fish_datatypes.RData'))

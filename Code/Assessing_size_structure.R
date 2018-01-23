#Assess size structure data in preparation for field season 2018
# Goals: assess whether we have good enough estimates of the following that we don't need a survey for them in 2018 (or to determine what kind of survey we might need)
# 1) size structure histograms - does it vary substantially among sites/years? 
# What other things to look at? Standing size structure @ survey time in anemones? How that varies?
# Estimates of transition rates/survival between size classes?
# Size as relates to hierarchical position (particularly sex and if part of breeding pair)
# Size structure for pre-fin clip size and post-fin clip size (expect it to but much clearer for the latter as compared to the former)

# Some other questions that might be good to ask:
# Do we get the same size structure from different types of surveys (like C and A, for size ranges where those overlap)?

#To-dos:
## Pre-field season:
# Assess whether the distributions across sites or across years or across study types are the same
# Size structure for pre-fin clip size and post-fin clip size (expect it to but much clearer for the latter as compared to the former)
## Later:
# Can we estimate transition rates, survival between size classes?


#################### Set-up: ####################
#Load relevant libraries
library(RCurl) #allows running R scripts from GitHub
library(RMySQL) #might need to load this to connect to the database?
library(dplyr)
library(tidyr)
library(lubridate)
#library(dbplyr)
library(ggplot2)
#library(cowplot)
#library(fields)
library(here)

##### Reminder of dive info:
# dive type options:
# A - anemone survey dive
# C - clownfish collection dive
# D - clownfish collection with transect (2012)
# E - clownfish collection with transect and mapping fish survey (2012)
# F - fish transect with quadrats for coral survey
# M - mapping fish survey with transect (2012)
# R - recapture survey

##### Vector and indices to reference sites easily in plots and filtering and such (can call things like site_vec[Cabatoan] or site_vec[1]))
site_vec <- c("Cabatoan", "Caridad Cemetery", "Caridad Proper", "Elementary School", "Gabas", "Haina", "Hicgop South",
              "Magbangon", "Palanas", "Poroc Rose", "Poroc San Flower", "San Agustin", "Sitio Baybayon", "Sitio Lonas", 
              "Sitio Tugas", "Tamakin Dacot", "Visca", "Wangag")
Cabatoan <- 1
CaridadCemetery <- 2
CaridadProper <- 3
ElementarySchool <- 4
Gabas <- 5
Haina <- 6
HicgopSouth <- 7
Magbangon <- 8
Palanas <- 9
PorocRose <- 10
PorocSanFlower <- 11
SanAgustin <- 12
SitioBaybayon <- 13
SitioLonas <- 14
SitioTugas <- 15
TamakinDacot <- 16
Visca <- 17
Wangag <- 18

#other thresholds or constants
clip_min <- 3.5 #minimum size for fin-clip (need to double-check this...)
tag_min <- 6.0 #minimum size for tagging (need to double-check)

#################### Functions: ####################
##### Functions from Michelle's GitHub
#helper functions - do various tasks w/database (like assigning dates and site to fish and such)
script <- getURL("https://raw.githubusercontent.com/mstuart1/helpers/master/scripts/helpers.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

#################### Running things: ####################
##### Pull things from the database
leyte <- read_db("Leyte") 

fish <- leyte %>%  #pull out APCL clownfish
  tbl('clownfish') %>%
  select(fish_table_id, anem_table_id, fish_spp, size, cap_id, recap, tag_id) %>%
  collect() %>%
  filter(fish_spp == 'APCL')

anems <- leyte %>%  #pull out anemones associated with APCL clownfish entries in the clownfish database
  tbl('anemones') %>%
  select(anem_table_id, dive_table_id, anem_spp, old_anem_id, anem_id, anem_obs) %>%
  collect() %>%
  filter(anem_table_id %in% fish$anem_table_id)
# #check that the warning messages I get when I run this don't really mean anything... Warning messages:
# 1: In .local(conn, statement, ...) :
#   Decimal MySQL column 10 imported as numeric
# 2: In .local(conn, statement, ...) :
#   Decimal MySQL column 5 imported as numeric

dives <- leyte %>%
  tbl('diveinfo') %>%
  select(dive_table_id, dive_type, date, site, gps) %>%
  collect() %>%
  filter(dive_table_id %in% anems$dive_table_id)

#join them all up into one data frame 
fishInfo <- left_join(fish, anems, by="anem_table_id") #join fish + anem info
fishInfo <- left_join(fishInfo, dives, by="dive_table_id") #and add dive info to that...

#make year, month, day columns for easier sorting
fishInfo$year <- as.integer(substring(fishInfo$date, 1, 4)) #make a year column
fishInfo$month <- as.integer(substring(fishInfo$date, 6, 7)) #make a month column
fishInfo$day <- as.integer(substring(fishInfo$date, 9, 10)) #make a day column

#convert size from chr to number
fishInfo$size_num <- as.numeric(fishInfo$size) #got warning message that "NAs introduced by coersion" - should check into why... I think it's a ? in the data and also some existing NAs, plus one number with a space after the decimal point

#find minimum and maximum size of fish in dataset (to use in setting plot limits and such...)
min_size <- min(fishInfo$size_num, na.rm = TRUE)
max_size <- max(fishInfo$size_num, na.rm = TRUE)

#make some vectors of breaks of varying fineness to use for histogram plotting
break_vec_small <- seq(min_size, max_size, by = 0.5)
break_vec_large <- seq(min_size, max_size, by = 1.0)

# #filter in different ways for plotting below (just doing this in the plot instead for now)
# # C dives
# fishInfoC <- fishInfo %>% filter(dive_type == "C") #filter out just C dives (has all years except 2012)
# fishInfoC_Cabatoan <- fishInfo %>% filter(dive_type == "C", site == "Cabatoan")
# fishInfoC_Palanas <- fishInfo %>% filter(dive_type == "C", site == "Palanas")
# 
# # A dives
# fishInfoA <- fishInfo %>% filter(dive_type == "A") #filter out just A dives (only 2015, 2017)

#################### Plots: ####################
##### Size-structures lumping all sites together
# Overall histogram of all fish sizes and sites from C dives (all years except 2012)
#fine-scale breaks
pdf(file = here("Plots", "CfishHist_allsites_smallbreaks.pdf"))
hist(fishInfoC$size_num, break_vec_small, xlab = 'Size (cm)', main = 'All fish from C dives (for all years and sites)')
dev.off()

#larger-scale breaks
pdf(file = here("Plots", "CfishHist_allsites_largebreaks.pdf"))
hist(fishInfoC$size_num, break_vec_large, xlab = 'Size (cm)', main = 'All fish from C dives (for all years and sites)')
dev.off()

# Histogram of C dives across years for all sites together (all years except 2012 which didn't have a C dive)
pdf(file = here("Plots", "CfishHist_allsites_byyear.pdf"))
ggplot(data = fishInfoC, aes(size_num)) +
  geom_histogram(binwidth = 0.5) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Size histograms for fish from C dives (all sites together)") +
  theme_bw()
dev.off()

# Histogram of C dives across years for all sites together, percentage of fish sampled that year rather than raw count

# Histogram from A dives for all years and sites lumped (only 2015 and 2017)
#fine-scale breaks
pdf(file = here("Plots", "AfishHist_allsites_smallbreaks.pdf"))
hist(fishInfoA$size_num, breaks = break_vec_small, xlab = 'Size (cm)', main = 'All fish from A dives (for all years and sites)')
dev.off()

#larger-scale breaks
pdf(file = here("Plots", "AfishHist_allsites_largebreaks.pdf"))
hist(fishInfoA$size_num, breaks = 15, xlab = 'Size (cm)', main = 'All fish from A dives (for all years and sites)')
dev.off()

# Comparing C and A dives, plotted by year (all sites combined)
pdf(file = here("Plots", "CandAfishHist_allsites_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("A","C"))), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A")), alpha = 0.6, binwidth = 1) + #if don't do the lines separately, it does the histogram for all the data but colors sections by dive type (stacks them) rather than plotting two histograms on top of each other
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C")), alpha = 0.6, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Size histograms for fish from A and C dives (all sites combined)") +
  theme_bw()
dev.off()

# Comparing C and A dives (plotted by year), adding in D,E,M for 2012
pdf(file = here("Plots", "ACDEMfishHist_allsites_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("A","C","D","E","M"))), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M")), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Size histograms for fish from A,C,D,E,M dives (all sites combined)") +
  theme_bw()
dev.off()

# Comparing C and R dives (plotted by year)
pdf(file = here("Plots", "CRfishHist_allsites_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("C","R"))), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R")), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Size histograms for fish from C and R dives (all sites combined)") +
  theme_bw()
dev.off()

##### Comparing sites
pdf(file = here("Plots/Size_structure", "SitesCDE_allyears.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("C","D","E"))), aes(size_num, fill = site)) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[1], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[2], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[3], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[4], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[5], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[6], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[7], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[8], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[9], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[10], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[11], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[12], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[13], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[14], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[15], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[16], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[17], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[18], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  #facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("C,D,E dives by site, all years combined") +
  theme_bw()
dev.off()

# All sites on top of each other, by year
pdf(file = here("Plots/Size_structure", "SitesCDE_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("C","D","E"))), aes(size_num, fill = site)) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[1], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[2], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[3], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[4], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[5], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[6], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[7], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[8], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[9], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[10], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[11], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[12], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[13], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[14], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[15], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[16], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[17], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[18], dive_type %in% c("C","D","E"))), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("C,D,E dives by site and year") +
  theme_bw()
dev.off()

##### Look just at small fish (below tagging size)
# All sites on top of each other, by year, just below-clip sized fish
pdf(file = here("Plots/Size_structure", "SitesADE_belowclipsize_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("A","D","E"))), aes(size_num, fill = site)) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[1], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[2], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[3], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[4], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[5], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[6], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[7], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[8], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[9], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[10], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[11], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[12], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[13], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[14], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[15], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[16], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[17], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[18], dive_type %in% c("A","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("A,D,E dives by site and year, <= min clip size") +
  theme_bw()
dev.off()

# All sites on top of each other, by year, just below-clip sized fish, just C (and D+E)
pdf(file = here("Plots/Size_structure", "SitesCDE_belowclipsize_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("C","D","E"))), aes(size_num, fill = site)) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[1], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[2], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[3], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[4], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[5], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[6], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[7], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[8], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[9], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[10], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[11], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[12], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[13], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[14], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[15], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[16], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[17], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(site == site_vec[18], dive_type %in% c("C","D","E"), size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("C,D,E dives by site and year, <= min clip size") +
  theme_bw()
dev.off()

# All sites combined, by year, <= min clip size, separated by survey
pdf(file = here("Plots/Size_structure", "ACDE_belowclipsize_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(dive_type %in% c("A","C","D","E"))), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", size_num <= clip_min)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("A,C,D,E dives by type and year, <= min clip size, all sites comb") +
  theme_bw()
dev.off()

##### Looking at sites individually
# Cabatoan
pdf(file = here("Plots", "Cabatoan_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == "Cabatoan")), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == "Cabatoan")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == "Cabatoan")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == "Cabatoan")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == "Cabatoan")), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Cabatoan") +
  theme_bw()
dev.off()

# Caridad Cemetery
pdf(file = here("Plots", "CaridadCemetary_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == "Caridad Cemetery")), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == "Caridad Cemetery")), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Caridad Cemetery") +
  theme_bw()
dev.off()

# Caridad Proper
site_name <- site_vec[CaridadProper]
pdf(file = here("Plots", "CaridadProper_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == "Caridad Proper")), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle("Caridad Proper") +
  theme_bw()
dev.off()

# Elementary School
site_name <- site_vec[ElementarySchool]
pdf(file = here("Plots", "ElementarySchool_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_vec[ElementarySchool]) +
  theme_bw()
dev.off()

# Gabas
site_name <- site_vec[Gabas]
pdf(file = here("Plots", "Gabas_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_vec[Gabas]) +
  theme_bw()
dev.off()

# Haina
site_name <- site_vec[Haina]
pdf(file = here("Plots", "Haina_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Hicgop South
site_name <- site_vec[HicgopSouth]
pdf(file = here("Plots", "HicgopSouth_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Magbangon
site_name <- site_vec[Magbangon]
pdf(file = here("Plots", "Magbangon_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Palanas
site_name <- site_vec[Palanas]
pdf(file = here("Plots", "Palanas_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Poroc Rose
site_name <- site_vec[PorocRose]
pdf(file = here("Plots", "PorocRose_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Poroc San Flower
site_name <- site_vec[PorocSanFlower]
pdf(file = here("Plots", "PorocSanFlower_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# San Agustin
site_name <- site_vec[SanAgustin]
pdf(file = here("Plots", "SanAgustin_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Sitio Baybayon 
site_name <- site_vec[SitioBaybayon]
pdf(file = here("Plots", "SitioBaybayon_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Sitio Lonas
site_name <- site_vec[SitioLonas]
pdf(file = here("Plots", "SitioLonas_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Sitio Tugas
site_name <- site_vec[SitioTugas]
pdf(file = here("Plots", "SitioTugas_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Tamakin Dacot
site_name <- site_vec[TamakinDacot]
pdf(file = here("Plots", "TamakinDacot_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Visca 
site_name <- site_vec[Visca]
pdf(file = here("Plots", "Visca_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

# Wangag
site_name <- site_vec[Wangag]
pdf(file = here("Plots", "Wangag_byyear.pdf"))
ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
  geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
  facet_grid(.~ year) +
  xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
  theme_bw()
dev.off()

#Was hoping this code would work for cycling through all the sites and making the plots but creates plots that don't open... Maybe b/c spaces in the site names are in the title? Will try to monkey with later...
#paste(paste(site_name, "byyear", sep = "_"), "pdf", sep = ".") - makes labels like "Wangag_byyear.pdf"
# for(i in 1:length(site_vec)) {
#   site_name <- site_vec[i]
#   pdf(file = here("Plots", paste(paste(site_name, "byyear", sep = "_"), "pdf", sep = ".")))
#   ggplot(data = (fishInfo %>% filter(site == site_name)), aes(size_num, fill = dive_type)) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "A", site == site_name)), alpha = 0.5, binwidth = 1) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "C", site == site_name)), alpha = 0.5, binwidth = 1) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "D", site == site_name)), alpha = 0.5, binwidth = 1) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "E", site == site_name)), alpha = 0.5, binwidth = 1) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "M", site == site_name)), alpha = 0.5, binwidth = 1) +
#     geom_histogram(data = (fishInfo %>% filter(dive_type == "R", site == site_name)), alpha = 0.5, binwidth = 1) +
#     facet_grid(.~ year) +
#     xlab("size (cm)") + ylab("# fish") + ggtitle(site_name) +
#     theme_bw()
#   dev.off()
# }



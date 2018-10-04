#Useful functions and constants for clownfish work
#Inspired by Michelle's helper functions script, some functions and constants I expect to use multiple times

#################### Constants, indices, etc. ####################
##### Vector and indices to reference sites easily in plots and filtering and such (can call things like site_vec[Cabatoan] or site_vec[1]))
site_vec <- c("Cabatoan", "Caridad Cemetery", "Caridad Proper", "Elementary School", "Gabas", "Haina", "Hicgop South",
              "N. Magbangon", "S. Magbangon", "Palanas", "Poroc Rose", "Poroc San Flower", "San Agustin", "Sitio Baybayon", "Sitio Lonas", 
              "Sitio Tugas", "Tamakin Dacot", "Visca", "Wangag")
Cabatoan <- 1
CaridadCemetery <- 2
CaridadProper <- 3
ElementarySchool <- 4
Gabas <- 5
Haina <- 6
HicgopSouth <- 7
N_Magbangon <- 8
S_Magbangon <- 9
Palanas <- 10
PorocRose <- 11
PorocSanFlower <- 12
SanAgustin <- 13
SitioBaybayon <- 14
SitioLonas <- 15
SitioTugas <- 16
TamakinDacot <- 17
Visca <- 18
Wangag <- 19

#tagging and fin-clipping thresholds
min_tag_size <- 6.0 #minimum size for tagging   
min_clip_size <- 3.5 #minimum size for fin-clip

#first anemone tag in 2018
tag1_2018 <- 2938

#first metal anemone tag number (started using in May 2015)
first_metal_tag <- 2001 

#number of years sampled
years_sampled <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)

#winter months
winter_months <- c(1,2) #to pull out winter 2015 surveys - check that they didn't go into March too
spring_months <- c(3,4,5,6,7) #to pull out non-winter 2015 surveys

#recaptured fish known to be caught at two or more sites (been checked for typos)
multiple_site_recaps <- data.frame(tag_id = c('982000411818588', '982000411818610', '985153000401241'),
                                   gen_id = c(NA, NA, NA))

#################### Functions: ####################
#function to make vector of strings for column names for something done each year (like columns for sampling each year or minimum distance sampled to each anem each year, etc.)
makeYearlyColNames <- function(start.Year, end.Year, descriptor) { #start.Year is first year of sampling, end.Year is final year of sampling, descriptor is string to go before year in column name (like "min_dist_" if want columns like "min_dist_2012")
  
  if (start.Year > end.Year) {
    out <- NULL
  } 
  else {
    out <- as.vector(NA) #initalize output vector of column names
    year <- start.Year 
    
    for (i in 1:(end.Year - start.Year + 1)) {
      out[i] <- paste(descriptor, year, sep="") #create string like "min_dist_2012", where descriptor is something like "min_dist_" and year is the year sampled
      year <- year + 1
    }
  }
  return(out)
}

# Finds the real parameter estimate from the logit estimate
logit_recip <- function(logitval) {
  recip = (exp(logitval))/(1 + exp(logitval))
  return(recip)
}

# Function to attach 2018 anems to anems previously seen (since right now, anem_obs hasn't been extended to all of them), from AnemLocations.R script originally, this version from TotalAnemsAtSite.R script
attach2018anems <- function(anemdf) {
  
  #make anem_id numeric, create anem_id_unq
  anemdf <- anemdf %>%
    mutate(anem_id = as.numeric(anem_id)) %>% #make anem_id numeric to make future joins/merges easier
    mutate(anem_id_unq = ifelse(is.na(anem_obs), paste("id", anem_id, sep=""), paste("obs", anem_obs, sep="")))  #add unique anem id so can track anems easier (has obs or id in front of whichever number it is reporting: eg. obs192)
  
  #filter out anems that might be repeat visits (2018, tag_id less than the new tags for 2018 or new tag and old tag present) and don't already have an anem_obs
  anems2018 <- anemdf %>% #646
    filter(year == 2018) %>%
    filter(anem_id < tag1_2018 | (anem_id >= tag1_2018 & !is.na(old_anem_id))) %>% #filter out the ones that could could have other anem_ids (sighting of existing anem_id tag or new tag with old_anem_id filled in), checked this (below) and think it is filtering out correctly...
    filter(is.na(anem_obs)) #some anem_obs already filled in so take those out... 
    
  #other 2018 anems that aren't candidates for repeat obs (so can add back in later)
  anems2018_2 <- anemdf %>% #270
    filter(year == 2018) %>%
    filter(anem_id >= tag1_2018 & is.na(old_anem_id))
  
  #other 2018 anems that already have an anem_obs (so can add back in later) - checked (once...) that anems2018, anems2018_2, and anems2018_3 covers all 2018 anems
  anems2018_3 <- anemdf %>%
    filter(year == 2018) %>%
    filter(!is.na(anem_obs))
  
  #filter out anems that are candidates for revisits (just anything from before 2018...), and that will get added back into final df later
  otheranems <- anemdf %>% #4068
    filter(year != 2018)
  
  #the filtering above covers all anems except anem_table_id 10473 with anem_id 2535, which has no year associated with it
  #test <- anemdf %>% filter(!year %in% c(2012,2013,2014,2015,2016,2017,2018)) #this is how I found that anem
  
  #go through anems2018 that might be revisits and see if there are anems from previous years that match
  for (i in 1:length(anems2018$anem_id)) {
    
    testid <- anems2018$anem_id[i] #pull out anem_id to compare
    testoldid <- anems2018$old_anem_id[i] #pull out old_anem_id
    
    matchanem <- filter(otheranems, anem_id == testid)  #does the anem_id match an anem_id record from the past?
    matcholdanem <- filter(otheranems, anem_id == testoldid) #does the old_anem_id match an anem_id from the past?
    
    # does the anem_id match an anem_id from the past? 
    if (length(matchanem$anem_id) > 0) { #if the anem_id matches an old one
      # if so, does the site match?
      if (matchanem$site[1] == anems2018$site[i]) { #make sure the site matches
        anems2018$anem_id_unq[i] = matchanem$anem_id_unq[1] #if there are multiple records from the past that match, take the first one
      } else {
        print(paste("Site does not match for anem_id", testid)) #print message if site doesn't match
      }
      # if not, does an old_anem_id match an anem_id from the past?
    } else if (length(matcholdanem$anem_id) > 0) { #if the old_anem_id matches one from the past
      # if so, does the site match?
      if (matcholdanem$site[1] == anems2018$site[i]) { #check site
        anems2018$anem_id_unq[i] = matcholdanem$anem_id_unq[1]
      } else {
        print(paste("Site does not match for old_anem_id", testoldid, "and anem_id", testid))
      }
    } else {
      anems2018$anem_id_unq[i] = anems2018$anem_id_unq[i]
      print(paste("No past anem matches found for testid", testid, "and testoldid", testoldid))
    }
  }
  out <- rbind(anems2018, anems2018_2, anems2018_3, otheranems)
  
  if(length(out$anem_table_id) == (length(anemdf$anem_table_id)-1)) { #1 anem (anem_table_id 10473, anem_id 2535 has no year listed - investigate further later) (maybe one of those 3 anems I sighted that used to get lost in the filtering b/c not have a year associated with them (NA)?)
    print("All anems accounted for.")
  } else {
    print("Some anems missing or double-counted.")
    print(length(out$anem_table_id))
    print(length(anemdf$anem_table_id))
  }
  return(out)
}


# #################### Pull out database info and save: ####################
# leyte <- read_db("Leyte") 
# 
# anem_db <- leyte %>% tbl("anemones") %>% collect()
# fish_db <- leyte %>% tbl("clownfish") %>% collect()
# dives_db <- leyte %>% tbl("diveinfo") %>% collect()
# gps_db <- leyte %>% tbl("GPX") %>% collect()
# 
# save(leyte, file = here("Data", "leyte.RData"))
# save(anem_db, file = here("Data", "anem_db.RData"))
# save(fish_db, file = here("Data", "fish_db.RData"))
# save(dives_db, file = here("Data", "dives_db.RData"))
# save(gps_db, file = here("Data", "gps_db.RData"))
# 
# load(file = here("Data", "anem_db.RData"))
# load(file = here("Data", "fish_db.RData"))
# load(file = here("Data", "dives_db.RData"))
# load(file = here("Data", "gps_db.RData"))

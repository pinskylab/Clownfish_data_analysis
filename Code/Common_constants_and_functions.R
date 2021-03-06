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

#vector of sites from north to south
# this one has Sitio Hicgop in in, which as far as I can tell doesn't have any fish associated to it in the db
# site_vec_NS <- c('Palanas', 'Wangag', 'N. Magbangon', 'S. Magbangon' , 'Cabatoan',
#                  'Caridad Cemetery', 'Caridad Proper', 'Sitio Hicgop', 'Hicgop South',
#                  'Sitio Tugas', 'Elementary School', 'Sitio Lonas', 'San Agustin',
#                  'Poroc San Flower', 'Poroc Rose', 'Visca', 'Gabas', 'Tamakin Dacot',
#                  'Haina', 'Sitio Baybayon')

site_vec_NS <- c('Palanas', 'Wangag', 'N. Magbangon', 'S. Magbangon' , 'Cabatoan',
                 'Caridad Cemetery', 'Caridad Proper', 'Hicgop South',
                 'Sitio Tugas', 'Elementary School', 'Sitio Lonas', 'San Agustin',
                 'Poroc San Flower', 'Poroc Rose', 'Visca', 'Gabas', 'Tamakin Dacot',
                 'Haina', 'Sitio Baybayon')

# data frame with site names and order alphabetically and geographically (N-S)
site_vec_order <- data.frame(site_name = site_vec)
site_vec_order$alpha_order <- seq(1:length(site_vec))
site_vec_order$geo_order <- c(5, 6, 7, 10, 16, 18, 8, 3, 4, 1, 14, 13, 12, 19, 11, 9, 17, 15, 2)
  
#tagging and fin-clipping thresholds
min_tag_size <- 6.0 #minimum size for tagging   
min_clip_size <- 3.5 #minimum size for fin-clip

#size thresholds for determining stage (just made up based on gut for now) - update based on data - Michelle has a boxplot somewhere?
min_breeding_F_size <- 6
min_breeding_M_size <- 6
breeding_F_YR_cutoff <- 9 #for now, saying if a fish is greater than 9cm but marked YR, probably a female
female_size_cutoff <- 10 #for now, saying if a fish is >10cm but we don't know anything about the color, probably a female

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

# Function to find the lat and long for an anem_id (based off of Michelle's sample_latlon function) - copied from AnemLocations.R
anemid_latlong <- function(anem.table.id, anem.df, latlondata) { #anem.table.id is one anem_table_id value, anem.df is the anem.Processed data frame (so don't have to pull from db again here), latlondata is table of GPX data from database (rather than making the function call it each time); will need to think a bit more clearly about how to handle different locations read for different visits to the same anem_id (or different with same anem_obs); for now, just letting every row in anem.Info get a lat-long
  
  #this is what causes the multiple entries - pulls multiple rows for a few anems (81) that have multiple entries for the same anem_table_id in the database
  anem <- anem.df %>% 
    filter(anem_table_id == anem.table.id) %>% #get the relevant dive, time, site, etc. info for this anem_table_id
    distinct(anem_table_id, .keep_all = TRUE) #added this in to get remove multiple entries that exist for some 2018 anem_table_ids
  
  # find the lat long for this anem observation
  latloninfo <- latlondata %>%
    filter(date %in% anem$date & unit == anem$gps) %>% #filter out just the GPS unit associated with this anem observation (added since previous time)
    filter(hour == anem$hour & min == anem$min) %>%
    mutate(lat = as.numeric(lat)) %>%
    mutate(lon = as.numeric(lon))
  
  #pull duplicates (so if sat in one place for enough time that multiple readings recorded there)
  #(there are more digits in the lats and lons than show up on the screen so sometimes things look like duplicates but aren't)
  dups_lat <- which(duplicated(latloninfo$lat)) #vector of positions of duplicate values 
  dups_lon <- which(duplicated(latloninfo$lon))
  
  #either take the mean of the lat/lon readings or the duplicated values, depending if there are duplicate points
  if(length(dups_lat) == 0) { #if all latitude points are different
    anem$lat <- round(mean(latloninfo$lat), digits = 5) #take the mean of the latitude values (digits = 5 b/c that is what Michelle had)
    anem$lon <- round(mean(latloninfo$lon), digits = 5) #take the mean of the longitude values
  }else{
    anem$lat <- latloninfo$lat[dups_lat[1]] #if are duplicates, take the value of the first duplicated point
    anem$lon <- latloninfo$lon[dups_lon[1]]
    print(paste("Dups in lat lons at anem_table_id", anem$anem_table_id, "on", anem$date, "with lat", anem$lat, sep = " ")) #just have this while trouble-shooting repeat entries in the database
  }
  
  return(anem)
  
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

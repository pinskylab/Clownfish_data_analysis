#Useful functions and constants for clownfish work
#Inspired by Michelle's helper functions script, some functions and constants I expect to use multiple times

#################### Constants, indices, etc. ####################
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

#tagging and fin-clipping thresholds
min_tag_size <- 6.0 #minimum size for tagging   
min_clip_size <- 3.5 #minimum size for fin-clip

#first anemone tag in 2018
tag1_2018 <- 2938

#first metal anemone tag number (from May 2015)
first_metal_tag <- 2001 #based on AD guess from looking @ database, emailed Michelle and Malin to confirm

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

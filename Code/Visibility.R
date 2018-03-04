# Assess visibility across sites and across time for Albuera managers

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

# Add year and month as a category to the dives 
dives$year <- as.integer(substring(dives$date,1,4)) #add year to dives
dives$month <- as.integer(substring(dives$date,6,7)) #add month to dives

# Visibility ranges used and the numbers to convert them to
vis_rec <- c("<1", "2 to 3", "2 to 4", "2 to 5", "2 to 6", "3 to 4", "3 to 8", "4 to 1", "4 to 2", "4 to 3", "4 to 5") #recorded visibility on datasheet
vis_max <- c(1, 3, 4, 5, 6, 4, 8, 4, 4, 4, 5) #maximum visibility in range recorded
vis_min <- c(0, 2, 2, 2, 2, 3, 3, 1, 2, 3, 4) #minimum visibility in range recorded
vis_avg <- c(0.5, 2.5, 3, 3.5, 4, 3.5, 5.5, 2.5, 3, 3.5, 4.5) #average visibility in range recorded
vis_convert <- data.frame(vis_rec, vis_max, vis_min, vis_avg)

# Notes for testing: 139 is first row in "dives" with a range recorded for visibility_m ("4 to 1")

#################### Functions: ####################
convertVisRangeToNum <- function(df, convertList, vis_choice) { #vis_choice = maxV, minV, avgV
  df$visibility_m_clean <- rep(NA, length(df$dive_table_id), 1) #add a column for cleaned-up visibility
  
  for(i in 1:length(df$dive_table_id)){
    vis <- df$visibility_m[i] #pull out the recorded visibility 
    if(vis %in% convertList$vis_rec == FALSE) { #if recorded vis isn't one of the ranges, just use the recorded vis
      df$visibility_m_clean[i] <- df$visibility_m[i]
    } else { #if it is, convert as chosed (to max, min, or avg of range)
      vis_id <- which(convertList$vis_rec %in% vis) #find the position of the range recorded in the convert df
      if(vis_choice == 'maxV') { 
        df$visibility_m_clean[i] <- convertList$vis_max[vis_id] # if want to convert to max of range
      } else if (vis_choice == 'minV') {
        df$visibility_m_clean[i] <- convertList$vis_min[vis_id] # if want to convert to min of range
      } else if (vis_choice == 'avgV') {
        df$visibility_m_clean[i] <- convertList$vis_avg[vis_id] # if want to convert to avg of range
      }
    }
  }
  df$visibility_m_clean <- as.numeric(df$visibility_m_clean) #convert to numeric
  return(df)
}

#################### Running things! ####################
##### Find average vis per year by site
#convert ranges (like "2 to 3" entered into the database to numbers (either max, min, or average of range))
dives_visClean <- convertVisRangeToNum(dives, vis_convert, 'avgV')

#find average vis by year and site
avg_vis_2012 <- dives_visClean %>% filter(year == 2012) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2012$year <- rep(2012, length(avg_vis_2012$site), 1)
avg_vis_2013 <- dives_visClean %>% filter(year == 2013) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2013$year <- rep(2013, length(avg_vis_2012$site), 1)
avg_vis_2014 <- dives_visClean %>% filter(year == 2014) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2014$year <- rep(2014, length(avg_vis_2012$site), 1)
avg_vis_2015 <- dives_visClean %>% filter(year == 2015) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2015$year <- rep(2015, length(avg_vis_2015$site), 1)
avg_vis_2016 <- dives_visClean %>% filter(year == 2016) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2016$year <- rep(2016, length(avg_vis_2016$site), 1)
avg_vis_2017 <- dives_visClean %>% filter(year == 2017) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
avg_vis_2017$year <- rep(2017, length(avg_vis_2017$site), 1)

avg_vis <- rbind(avg_vis_2012, avg_vis_2013, avg_vis_2014, avg_vis_2015, avg_vis_2016, avg_vis_2017)

#trying to figure out how to do the above in fewer lines...
# site_group <- group_by(dives_visClean, site)
# avg_vis_site <- site_group %>% group_by(year) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
# avg_vis_site_year <- dives_visClean %>% group_by(site) %>% summarize()
# avg_vis_per_year <- dives %>% filter(visi)
# avg_vis_per_year <- dives_visClean %>% group_by(site, year) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE))

#################### Plots! ####################
pdf(file = here("Plots/Occupancy", "OccupancyBreakdownBySpp2015_Freq.pdf")) # raw counts
ggplot(data=anemOcc_2015, aes(x=Var1, y=Freq)) +
  geom_col() +
  labs(x='species',y='# anemones') +
  ggtitle('2015 anemones') +
  theme_bw() 
dev.off()
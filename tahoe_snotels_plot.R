# jack tarricone
# december 26, 2021
# shelburne, VT

# creating homemade tahoe snotel plot!

library(snotelr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(plyr)
library(RColorBrewer)

# read in station meta data from Truckee, Carson, Lower Sac (32 stations)
tahoe_meta <-read.csv("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/tahoe_station_meta.csv")

# format date columns to actual dates
tahoe_meta$start_date <-mdy(tahoe_meta$start_date)
tahoe_meta$end_date <-mdy(tahoe_meta$end_date)

# create list to loop the dataframes into
df_list <-as.list(rep(NA, 32))

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(tahoe_meta$name)){
  df_list[[i]] <-as.data.frame(snotel_download(tahoe_meta$station_id[i], path = tempdir(), internal = TRUE))
}

# assign correct names to elements of list
names(df_list) <-tahoe_meta$name

# filter to just wy2021
wy2021 <-df_list

# loooooop
for (i in seq_along(df_list)){
  single_df <-df_list[[i]] # pull out df from list
  single_df$date <-ymd(single_df$date) # convert data to R date
  
  # filter to start at 10/1/2021 or beginging of water year 2021
  wy2021[[i]] <-filter(single_df, date >= "2021-10-01")
}

# convert list to df, yup
wy2021_df <-bind_rows(wy2021) 
head(wy2021_df)

mean_test <-wy2021_df %>%
  group_by(date) %>%
  dplyr::summarize(swe_mean = mean(snow_water_equivalent, na.rm=TRUE))

theme_set(theme_light(base_size = 12)) 
ggplot(wy2021_df)+
  geom_line(aes(date, snow_water_equivalent, color = site_name), alpha = .5)+ 
  geom_line(data = wy2021_df %>%
              group_by(date) %>%
              dplyr::summarize(swe_mean = mean(snow_water_equivalent, na.rm=TRUE)), 
              mapping = aes(y = swe_mean, x = date), color = "black", size = .9) +
  scale_fill_brewer()

  
 

# jack tarricone
# december 26, 2021
# shelburne, VT

# creating homemade tahoe snotel plot!

library(snotelr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

# read in station meta data from Truckee, Carson, Lower Sac (32 stations)
tahoe_meta <-read.csv("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/tahoe_station_meta.csv")

# format date columns to actual dates using lubridate
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

# loop to filter each df to start at 10/1/2021
for (i in seq_along(df_list)){
  single_df <-df_list[[i]] # pull out df from list
  single_df$date <-ymd(single_df$date) # convert data to R date
  
  # filter to start at 10/1/2021 or beginging of water year 2021
  wy2021[[i]] <-filter(single_df, date >= "2021-10-01")
}

# convert list to df, yup
wy2021_df <-bind_rows(wy2021) 
head(wy2021_df)


theme_set(theme_light(base_size = 11)) 
plot <-ggplot(wy2021_df)+
  geom_line(aes(date, snow_water_equivalent, group = site_name, color = elev), alpha = .8)+ # plot by site
  scale_colour_gradientn(colours = terrain.colors(20)) +
  labs(title = "12/27 Lake Tahoe SNOTEL SWE 32 Stations ", 
    y = "SWE (mm)", x = "Date", color = "Station Elevation (m)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.20,.6),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot2 <-plot + 
  ggnewscale::new_scale_colour() + 
  geom_line(data = wy2021_df %>% # plot mean within imbedded dplyr functions
                group_by(date) %>%
                dplyr::summarize(swe_mean = mean(snow_water_equivalent, na.rm=TRUE)), 
              mapping = aes(y = swe_mean, x = date, color = ""), size = .9)+
  scale_colour_manual("32 Station Average", values = "black")

print(plot2)

setwd("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/")
ggsave(plot2,
       file = "tahoe_swe_12-27v2.png",
       width = 7, 
       height = 5,
       dpi = 400)


  


  
 

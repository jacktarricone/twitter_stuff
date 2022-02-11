# jack tarricone
# december 26, 2021
# shelburne, VT

# creating homemade tahoe snotel plot!

library(snotelr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)

{
# read in station meta data from Truckee, Carson, Lower Sac (32 stations)
tahoe_meta <-read.csv("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/tahoe_station_meta.csv")
tahoe_meta <-tahoe_meta[-c(2,18)]

# format date columns to actual dates using lubridate
tahoe_meta$start_date <-mdy(tahoe_meta$start_date)
tahoe_meta$end_date <-mdy(tahoe_meta$end_date)

# format normals data
truckee_normals <-read.csv("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/truckee_91-20_normals.csv")
truckee_normals$Date <-as.Date(truckee_normals$Date)
plot(truckee_normals$dowy, truckee_normals$min_mm) # test

# define date
todays_date <-as.character(Sys.Date())

# filter normals to today
normals_today <-filter(truckee_normals, Date <= Sys.Date())

# create list to loop the dataframes into
df_list <-as.list(rep(NA, 32))

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(tahoe_meta$name)){
  df_list[[i]] <-as.data.frame(snotel_download(tahoe_meta$station_id[i], path = tempdir(), internal = TRUE))
}


# loop to add inches
for (i in seq_along(df_list)){
  df <-df_list[[i]] # pull out df from list
  df_list[[i]] <-df %>% dplyr::mutate(swe_in = snow_water_equivalent / 25.4)
}

head(df_list[[1]])

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

# create plot with grey transarent stations, and max/min/med info to todays date
theme_set(theme_classic(base_size = 11)) 
plot <-ggplot(wy2021_df)+
  geom_line(aes(date, swe_in, group = site_name, color = "2021-22 SNOTEL Stations"), alpha = .7,  size = .3)+ # plot by site
  labs(title =  paste0(todays_date," Lake Tahoe SNOTEL 32 Station Composite"), 
       y = "SWE (in)", x = "Date", color = "Station Elevation (m)") +
  geom_line(normals_today, mapping = aes(y = min_in, x = Date, color = "'91-'20 Minimum"), size = .9) +
  geom_line(normals_today, mapping = aes(y = max_in, x = Date, color = "'91-'20 Maximum"), size = .9) +
  geom_line(normals_today, mapping = aes(y = med_in, x = Date, color = "'91-'20 Median"), size = .9, linetype = "dashed") +
  geom_line(data = wy2021_df %>% # plot mean within imbedded dplyr functions
              group_by(date) %>%
              dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE)), 
            mapping = aes(y = swe_mean_in, x = date, color = "2021-22 SNOTEL Mean"), size = .9)+
  scale_colour_manual("", values = c("blue","orange","red","black","grey"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.25,.7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))

# test plot
plot(plot)

# setwd("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/")
# ggsave(plot,
#        file = "tahoe_swe_12-27_normals_inv3.png",
#        width = 7, 
#        height = 5,
#        dpi = 400)


# create plot with grey transparent stations, and max/min/med info to full year

fy_plot <-ggplot(wy2021_df)+
  geom_line(aes(date, swe_in, group = site_name, color = "2021-22 SNOTEL Stations"), alpha = .7,  size = .3)+ # plot by site
  labs(title = paste0(todays_date," Lake Tahoe SNOTEL 32 Station Composite Full Water Year"), 
       y = "SWE (in)", x = "Date", color = "Station Elevation (m)") +
  geom_line(truckee_normals, mapping = aes(y = min_in, x = Date, color = "'91-'20 Minimum"), size = .9) +
  geom_line(truckee_normals, mapping = aes(y = max_in, x = Date, color = "'91-'20 Maximum"), size = .9) +
  geom_line(truckee_normals, mapping = aes(y = med_in, x = Date, color = "'91-'20 Median"), size = .9, linetype = "dashed") +
  geom_line(data = wy2021_df %>% # plot mean within imbedded dplyr functions
              group_by(date) %>%
              dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE)), 
            mapping = aes(y = swe_mean_in, x = date, color = "2021-22 SNOTEL Mean"), size = .9)+
  scale_colour_manual("", values = c("blue","orange","red","black","grey"))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.85,.7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))

# test plot
plot(fy_plot)

# setwd("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/")
# ggsave(fy_plot,
#        file = "tahoe_swe_12-27_normals_wy_inv3.png",
#        width = 7, 
#        height = 5,
#        dpi = 400)



##################################################################
######### create plot with mean and stations by elevation #######
#################################################################
?hcl.colors
RColorBrewer::display.brewer.all()

ele_plot <-ggplot(wy2021_df)+
  geom_line(aes(date, swe_in, group = site_name, color = elev), alpha = .7)+ # plot by site
  scale_colour_gradientn(colors = brewer.pal(9,"RdYlBu"))+
  labs(title = paste0(todays_date," Lake Tahoe SNOTEL 32 Station Composite by Elevation"), 
       y = "SWE (in)", x = "Date", color = "Station Elevation (m)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.2,.7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
ele_plot2 <-ele_plot + 
  ggnewscale::new_scale_colour() + 
  geom_line(data = wy2021_df %>% # plot mean within imbedded dplyr functions
              group_by(date) %>%
              dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE)), 
            mapping = aes(y = swe_mean_in, x = date, color = "WY2021 Mean"), size = .9)+
  scale_colour_manual("", values = c("black")) 

print(ele_plot2)

# setwd("/Users/jacktarricone/Desktop/twitter/tahoe_snotels/")
# ggsave(ele_plot2,
#        file = "tahoe_swe_12-27_ele_in.png",
#        width = 7, 
#        height = 5,
#        dpi = 400)
# 
# data <- wy2021_df %>% # plot mean within imbedded dplyr functions
#   group_by(date) %>%
#   dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE))
}


ele_plot <-ggplot(wy2021_df)+
  geom_line(aes(date, swe_in, group = site_name, color = elev), alpha = .7)+ # plot by site
  scale_colour_gradientn(colors = brewer.pal(9,"RdYlBu"))+
  labs(title = paste0(todays_date," Lake Tahoe SNOTEL 32 Station Composite by Elevation"), 
       y = "SWE (in)", x = "Date", color = "Station Elevation (m)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.2,.7),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
ele_plot2 <-ele_plot + 
  ggnewscale::new_scale_colour() + 
  geom_line(data = wy2021_df %>% # plot mean within imbedded dplyr functions
              group_by(date) %>%
              dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE)), 
            mapping = aes(y = swe_mean_in, x = date, color = "WY2021 Mean"), size = .9)+
  scale_colour_manual("", values = c("black")) 

print(ele_plot2)


############################
#### plot since dec storm ##

# filter to just from dec 26 to doay
dec_storm_2021 <-df_list

# loop to filter each df to start at 10/1/2021
for (i in seq_along(df_list)){
  single_df <-df_list[[i]] # pull out df from list
  single_df$date <-ymd(single_df$date) # convert data to R date
  
  # filter to start at 10/1/2021 or beginging of water year 2021
  dec_storm_2021[[i]] <-filter(single_df, date >= "2021-12-25")

# convert list to df, yup
dec_storm_2021_df <-bind_rows(dec_storm_2021) 
head(dec_storm_2021_df)


dry_plot <-ggplot(dec_storm_2021_df)+
  geom_line(aes(date, swe_in, group = site_name, color = elev), alpha = 1)+ # plot by site
  #scale_colour_gradientn(colors = brewer.pal(9,"RdYlBu"))+
  viridis::scale_color_viridis(option = "cividis") +
  labs(title = paste0("32 Tahoe SNOTELs since Dec. storm ",todays_date), 
       y = "SWE (in)", x = "Date", color = "Station Elevation (m)") 
  # theme(axis.line = element_line(colour = "black"),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       legend.position = c(.2,.7),
  #       legend.text = element_text(size = 8),
  #       legend.title = element_text(size = 9, face = "bold"),
  #       legend.margin = margin(t=0, unit='cm'),
  #       legend.key = element_rect(size = .2))
dry_plot2 <-dry_plot + 
  ggnewscale::new_scale_colour() + 
  geom_line(data = dec_storm_2021_df %>% # plot mean within imbedded dplyr functions
              group_by(date) %>%
              dplyr::summarize(swe_mean_in = mean(swe_in, na.rm=TRUE)), 
            mapping = aes(y = swe_mean_in, x = date, color = "WY2021 Mean"), size = 1.3)+
  scale_colour_manual("", values = c("red")) 

print(dry_plot2)
}
  
 

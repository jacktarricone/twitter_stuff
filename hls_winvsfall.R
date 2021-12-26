library(terra)

# tutorial for creating harmonized Landsat/Sen-2 (HLS) figures for twitter
# jack tarricone
# december 21, 2021

## add section on how to download

# list list rgb bands from both days
setwd("/Users/jacktarricone/Desktop/twitter/hls_win_vs_fall")
dl_fall <-list.files(pattern = "241", full.names = TRUE) # list all files for Aug-29 2021 (Julian day 241)
dl_winter <-list.files(pattern = "351", full.names = TRUE) # for Dec-17 2021 (Julian day 351)

# full 12 band winter
hls_winter_raw <-rast(dl_winter) # create "SpatRaster" for winter image
hls_winter_raw # inspect
plot(hls_winter_raw[[2]], col = gray(0:100 / 100)) # grayscale test plot

# full 12 band fall
hls_fall_raw <-rast(dl_fall)  # create "SpatRaster" for fall image
hls_fall_raw

# set crop extent
box <-c(720000,795000,4290240,4400040)
box_ext <-ext(box)

# crop images down slightly for better visualization
hls_winter <-crop(hls_winter_raw, box_ext)
hls_fall <-crop(hls_fall_raw, box_ext)

# test plot using terra's plotRGB function
plotRGB(hls_winter, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(hls_fall, r = 4, g = 3, b = 2, stretch = "lin")

# create RGB 3 band rasters to save
winter_rgb <-c(hls_winter[[4]], hls_winter[[3]], hls_winter[[2]])
fall_rgb <-c(hls_fall[[4]], hls_fall[[3]], hls_fall[[2]])

# test plot these
plotRGB(fall_rgb, stretch = "lin")
plotRGB(winter_rgb, stretch = "hist", bgalpha = 0, add = TRUE)

##### stretch rbg values and convert to 1-255
## not sure i need this but good practice

####################
##### red band #####
####################

# find max and max reflectance
r_min <-as.numeric(global(winter_rgb[[1]], fun= "min", na.rm = TRUE))
r_max <-as.numeric(global(winter_rgb[[1]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[1]] <-((winter_rgb[[1]]+abs(r_min))/(r_max+abs(r_min))*255)
plot(winter_rgb[[1]], col = gray(0:100 / 100)) # test plot

######################
##### green band #####
######################

# find max and max reflectance
g_min <-as.numeric(global(winter_rgb[[2]], fun= "min", na.rm = TRUE))
g_max <-as.numeric(global(winter_rgb[[2]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[2]] <-((winter_rgb[[2]]+abs(g_min))/(g_max+abs(g_min))*255)
plot(winter_rgb[[2]], col = gray(0:100 / 100)) # test plot

######################
##### blue band ######
######################

# find max and max reflectance
b_min <-as.numeric(global(winter_rgb[[3]], fun= "min", na.rm = TRUE))
b_max <-as.numeric(global(winter_rgb[[3]], fun= "max", na.rm = TRUE))

# convert to 0-255
winter_rgb[[3]] <-((winter_rgb[[3]]+abs(b_min))/(b_max+abs(b_min))*255)
plot(winter_rgb[[3]], col = gray(0:100 / 100)) # test plot

##### test plot with converted values
plotRGB(winter_rgb, stretch = "hist") # no difference 

# save rasters
writeRaster(winter_rgb, "winter_rgb.tif")
writeRaster(fall_rgb, "fall_rgb.tif")

### save image at the same resolution as the raster
# https://stackoverflow.com/questions/50953192/r-how-is-it-possible-to-export-from-r-an-image-from-a-raster-layer-while-mainta

png("tahoe_caldorv2.png", height=nrow(fall_rgb), width=ncol(winter_rgb)) # same dim as raster
plotRGB(fall_rgb, stretch = "lin", maxcell=ncell(fall_rgb)) # maxcell key
dev.off()
  
png("tahoe_winterv2.png", height=nrow(winter_rgb), width=ncol(winter_rgb))
plotRGB(fall_rgb, stretch = "lin", maxcell=ncell(winter_rgb)) # plot this first to fill NaN cells over lake
plotRGB(winter_rgb, stretch = "hist", maxcell=ncell(winter_rgb), bgalpha = 0, add = TRUE)
dev.off()

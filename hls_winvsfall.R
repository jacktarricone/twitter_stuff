library(terra)
library(EBImage)
install.packages("imagerExtra")
library(imagerExtra)

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
box <-c(710000,802000,4290240,4400040)
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

layout(matrix(1:2, 1, 2))
plotRGB(winter_rgb, stretch = "lin")

min <-as.numeric(global(winter_rgb[[1]], fun= "min", na.rm = TRUE))
max <-as.numeric(global(winter_rgb[[1]], fun= "max", na.rm = TRUE))
winter_rgb[[1]] <-(winter_rgb[[1]]+abs(min))/(max+abs(min))
plot(winter_rgb[[1]])


# save rasters
writeRaster(winter_rgb, "winter_rgb.tif")
writeRaster(fall_rgb, "fall_rgb.tif")

png("tahoe_fall.png", width = 3, height = 3, units = 'in', res = 2000)
plotRGB(fall_rgb, stretch = "lin")
dev.off()

png("tahoe_winter.png", width = 4, height = 4, units = 'in', res = 1200)
plotRGB(fall_rgb, stretch = "lin")
plotRGB(winter_rgb, stretch = "hist", bgalpha = 0, add = TRUE)
dev.off()



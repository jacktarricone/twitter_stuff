library(terra)

# list list rbg bands from both days
setwd("/Users/jacktarricone/Desktop/twitter/")
full_fall <-list.files(pattern = "244", full.names = TRUE)
full_winter <-list.files(pattern = "351", full.names = TRUE)

hls_winter <-rast(full_winter[1:12])
hls_winter
full_winter
blue <-list.files(pattern = "B02", full.names = TRUE)
green <-list.files(pattern = "B03", full.names = TRUE)
red <-list.files(pattern = "B04", full.names = TRUE)


# create 3 band rasters
winter <-c(red[1], green[1], blue[1])
winter <-rast(winter)

fall <-c(red[2], green[2], blue[2])
fall <-rast(fall)

# test plot
plotRGB(winter, stretch = "hist")
plotRGB(fall, stretch = "lin")

writeRaster(winter, "winter.tif")
writeRaster(fall, "fall.tif")

?plotRGB
?png



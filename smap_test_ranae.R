library(smapr)
library(terra)
library(rhdf5)
library(XML)
library(ncdf4)

# find
available_data <-find_smap(id = "SPL2SMAP_S", date = "2020-04-10", version = 3)
str(available_data)

# download
downloads <-download_smap(available_data)
str(downloads)

# set dir
list_smap(downloads, all = FALSE)

# rasterize
sm_raster <-extract_smap(downloads, "Soil_Moisture_Retrieval_Data_1km/soil_moisture_1km")
plot(sm_raster, main = "Level 3 soil moisture: 04 April 2020")
# writeRaster(sm_raster, "sm_raster.tif")






path <- "~/Downloads/SMAP_L2_SM_A_01097_D_20150416T140148_R13080_001.h5"

nc <- nc_open("~/Downloads/SMAP_L2_SM_A_01097_D_20150416T140148_R13080_001.h5")
v1 <- nc$var[["Soil_Moisture_Retrieval_Data/soil_moisture"]]

z_all <- ncvar_get(nc, v1)

test <-rast(path)

y <-4872 * 11568 
l <-872175/65535

h5ls(path)
dims <-path
nday <-as.integer(sub("6601 x 5701 x ","",dims))
??terra::sprc

r <- extract_smap(path,
                  name = "Soil_Moisture_Retrieval_Data/latitude")
expect_that(r, is_a("SpatRaster"))

3856 * 1614



# load in borth half of the data cube for RAM purposes
c1 <-as.matrix(h5read(path, "/Soil_Moisture_Retrieval_Data/soil_moisture"), nrow = 2175)
lat <-as.matrix(h5read(path, "/Soil_Moisture_Retrieval_Data/latitude"))
lon <-as.matrix(h5read(path, "/Soil_Moisture_Retrieval_Data/longitude"))
lon_df <-as.data.frame(lon)
hist(lon_df$V1)

mat <-as.matrix(c1, nrow = 2175)

872175 / 401
                 
list(3301:6601,1:5701,1:nday)

hmmt <-h5read(path, "/Metadata")
872175^.5
c1
print("c1 read into memory")
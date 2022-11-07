library(ggplot2)
library(terra)
library(scales)

w# set home folder
setwd("/Users/jacktarricone/ch1_jemez_data/gpr_rasters_ryan/new_swe_change")
list.files() #pwd

# import corrected unwrapped phase data
swe_raw <-rast("dswe_feb19-26_sp.tif")
plot(swe_raw)

# set crop extent and crop for better visualization
crop_ext <-ext(-106.57, -106.38, 35.81, 35.96) 
swe <-crop(swe_raw, crop_ext)

swe_df <-as.data.frame(swe, xy = TRUE)
colnames(swe_df)[3] <- "dswe"
head(swe_df)

?diverge_hcl
?scale_fill_gradientn


qn <- quantile(swe_df$dswe, c(0.01, 0.99), na.rm = TRUE)
head(qn)
qn01 <- scales::rescale(c(qn, range(swe_df$dswe))) 


# mapa
ggplot(swe_df) +
  geom_raster(aes(x,y, fill = dswe)) +
  labs(x="Latitude (deg)",
       y="Longitude (deg)",
       title = "phase")+
  scale_fill_gradientn(colours = colorRampPalette(c("darkred", "white", "darkblue"))(20),
                       limits = c(-5,5))+
  #guides(fill = guide_legend(keywidth = 2, keyheight = .3, label.position = "bottom",
  #                           title.position = "top")) +
  #labs(fill = "DROUGHT") +
  theme_void() +
  theme(legend.position = "top",
        legend.justification = 0.2,
        plot.background = element_rect(fill = "black", colour = NA),
        legend.title = element_text(colour = "white", size = 20, hjust = .5),
        legend.text = element_text(colour = "white"),
        plot.margin = margin(t = 10))

ggsave()

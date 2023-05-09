library(ggplot2)
library(snotelr)

indy_lake <-as.data.frame(snotel_download(541, path = tempdir(), internal = TRUE))
filt <-dplyr::filter(indy_lake, date >= "2022-10-01")

ggplot(filt)+
  geom_line(aes(x = as.Date(date), y = temperature_mean), linewidth = .15)+
  geom_line(aes(x = as.Date(date), y = temperature_max), color = 'darkred', linewidth = .15)+
  geom_line(aes(x = as.Date(date), y = temperature_min), color = 'darkblue', linewidth = .15)+
  theme_light(12)

# save
ggsave(
       file = "~/Downloads/indy_tmp_v1.png",
       width = 7, 
       height = 3,
       dpi = 600)

system("open ~/Downloads/indy_tmp_v1.png")

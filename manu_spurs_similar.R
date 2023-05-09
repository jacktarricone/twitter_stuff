# start man_u spurs similar analysis

library(dplyr)

# ## man u
# manu_manu_df <-read.csv("~/Desktop/twitter/manu_stats_v1.csv")
# 
# # make gf and ga
# manu_df$gf <-substr(manu_df$goals, 1, 2)
# manu_df$ga <-substr(manu_df$goals, 4, 5)
# 
# # format
# manu_df <-manu_df %>% relocate(c(gf,ga), .after=goals) %>%
#   select(-goals)
#   
# write.csv(manu_df,"~/Desktop/twitter/manu_stats_v2.csv")
# 
# ## man u
# spurs_df <-read.csv("~/Desktop/twitter/spurs_stats_v1.csv")
# head(spurs_df)
# 
# # make gf and ga
# spurs_df$gf <-substr(spurs_df$goals, 1, 2)
# spurs_df$ga <-substr(spurs_df$goals, 4, 5)
# 
# # format
# spurs_df <-spurs_df %>% relocate(c(gf,ga), .after=goals) %>%
#   select(-goals)
# 
# head(spurs_df)
# 
# write.csv(spurs_df,"~/Desktop/twitter/spurs_stats_v2.csv")


# read in dfs
spurs_df <-read.csv("~/Desktop/twitter/spurs_stats_v2.csv")
manu_df <-read.csv("~/Desktop/twitter/manu_stats_v2.csv")
manu_df$points <-as.integer(manu_df$points)
spurs_df$points <-as.integer(spurs_df$points)
spurs_df$ga <-as.integer(spurs_df$ga)

# filter for after sir alex
psa_manu <-filter(manu_df, season_start >= 2013)
psa_spurs <-filter(spurs_df, season_start >= 2013)

# calc stats
manu_mean <-psa_manu %>%
  summarise(across(where(is.integer), ~ mean(.x, na.rm = TRUE)))
manu_mean


spurs_mean <-psa_spurs %>%
  summarise(across(where(is.integer), ~ mean(.x, na.rm = TRUE)))
spurs_mean

# define names
club <-c("Man U", "Spurs")

# bind
both <-rbind(manu_mean, spurs_mean)
both <-both %>% select(-c(X, season_start, season_end))
both <-cbind(club,both)
both
write.csv(both, "~/Desktop/twitter/manu_spurs_comp.csv")

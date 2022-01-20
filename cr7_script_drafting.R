remotes::install_github('ewenme/understatr')
library(understatr)
library(worldfootballR)
library(tidyverse)

# load in MU 2021 team data
mu_2021 <-get_team_players_stats(team_name = "Manchester United", year = 2021)

# load in MU 2020 team data
mu_2020 <-get_team_players_stats(team_name = "Manchester United", year = 2020)

# bind dfs
mu_stats_raw <-rbind(mu_2021, mu_2020)

# filter out cr7
mu_stats_raw_nocr7 <-filter(mu_stats_raw, player_id != "2371")

# add new metrics
mu_stats <-mutate(mu_stats_raw,
  shots_per_90min = (shots / time) * 90,
  goals_per_90min = (goals / time) * 90 
)

# filter for fowards
mu_forwards <-filter(mu_stats, str_detect(position, 'F'))
mu_mids <-filter(mu_stats, str_detect(position, 'M'))

# bind good data
mu_good_cr <-rbind(mu_forwards, mu_mids)

# test plot
ggplot(mu_good, aes(file = year, x=year, y=goals_per_90min)) + 
  geom_boxplot()

ggplot(mu_good, aes(group = year, x=year, y=shots_per_90min)) + 
  geom_boxplot()

theme_set(theme_light(11))
ggplot(mu_good, aes(group = year,  x=year, y=xG)) + 
  geom_boxplot() +
  scale_x_continuous(breaks=c(2020,2021))+
  labs(title = "Manchester United Forward and Midfielder Expected Goals without CR7")


ggplot(mu_good_cr, aes(group = year,  x=year, y=xG)) + 
  geom_boxplot() +
  scale_x_continuous(breaks=c(2020,2021))+
  labs(title = "Manchester United Forward and Midfielder Expected Goals with CR7")
  
write.csv(mu_good, "/Users/jacktarricone/Desktop/twitter/cr7/mu_player_stats.csv")


bruno <-get_player_seasons_stats(1228)

bruno <-get_player_seasons_stats(1228)
# define epl url for the 2020-2021 campaign
pl_url <-fb_league_urls(country = "ENG", gender = "M", season_end_year =  "2021", tier = "1st")

# list of all pl teams urls
pl_teams_url <-fb_teams_urls(pl_url)
head(pl_teams_url)

# select MU from url list
mu_url <-pl_teams_url[2]

# define list of MU player urls
mu_players <-fb_player_urls(mu_url)

# identify individual attacking players
bruno_url <-mu_players[1] 

# get season stats
bruno_shooting <-fb_player_season_stats(bruno_url, stat_type = "shooting")
bruno_gca <-fb_player_season_stats(bruno_url, stat_type = "gca")



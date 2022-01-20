remotes::install_github('ewenme/understatr')
library(understatr)
library(worldfootballR)
library(ggplot2)
library(dplyr)


# load in MU 2021 team data
mu_2021 <-get_team_players_stats(team_name = "Manchester United", year = 2021)

# load in MU 2020 team data
mu_2020 <-get_team_players_stats(team_name = "Manchester United", year = 2020)

# bind dfs
mu_stats_raw <-rbind(mu_2021, mu_2020)

# filter for players only playing over 150 mintues
mu_stats <-filter(mu_stats_raw, time > 150)

ggplot(mu_stats, aes(group = year, x=year, y=xG)) + 
  geom_boxplot()



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



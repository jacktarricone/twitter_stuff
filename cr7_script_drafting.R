library(worldfootballR)

?fb_player_urls
?fb_teams_urls

# define epl url for the 2020-2021 campaign
pl_url <-fb_league_urls(country = "ENG", gender = "M", season_end_year =  "2021", tier = "1st")

# list of all pl teams urls
pl_teams_url <-fb_teams_urls(pl_url)
head(pl_teams_url)

# select MU from url list
mu_url <-pl_teams_url[2]

# define list of MU player urls
mu_players <-fb_player_urls(mu_url)

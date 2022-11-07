# november 6th, 2022
# comparing CR7 to other strikers in the EPL to firgure out which teams he would start on
# jack tarricone

library(understatr)
library(worldfootballR)
library(tidyverse)

# set wd
setwd("/Users/jacktarricone/Desktop/twitter/cr7/")
list.files()

# read in teams
epl_teams <-read.csv("epl_teams.csv")

# load in all player stats
all_players  <-vector(mode='list', length=20)

# loop to read in all players from the EPL
for (i in 1:length(epl_teams$team)){
  all_players[[i]] <-get_team_players_stats(team_name = epl_teams$team[i], year = 2022)
}

# turn list of dfs into one big df
players_df <-bind_rows(all_players, .id = "team")
head(players_df)


mu_2022 <-get_team_players_stats(team_name = "Manchester United", year = 2022)


# add new metrics
mu_stats <-mutate(mu_2022,
  shots_per_90min = (shots / time) * 90,
  goals_per_90min = (goals / time) * 90 
)

# filter for fowards
mu_forwards <-filter(mu_stats, str_detect(position, 'F'))
mu_mids <-filter(mu_stats, str_detect(position, 'M'))

# bind good data
mu_good <-rbind(mu_forwards, mu_mids)

# filter out cr7
mu_good_nocr7 <-filter(mu_good, player_id != "2371")

# test plot
theme_set(theme_light(11))

# goals per 90
ggplot(mu_good_nocr7, aes(group = year, x=year, y=goals_per_90min)) + 
  geom_boxplot()

# shots per 90
ggplot(mu_good_nocr7, aes(group = year, x=year, y=shots_per_90min)) + 
  geom_boxplot()

# xG without CR
ggplot(mu_good_nocr7, aes(group = year,  x=year, y=xG)) + 
  geom_boxplot() +
  scale_x_continuous(breaks=c(2020,2021))+
  labs(title = "Manchester United Forward and Midfielder Expected Goals without CR7")

# xG with CR
ggplot(mu_good, aes(group = year,  x=year, y=xG)) + 
  geom_boxplot() +
  scale_x_continuous(breaks=c(2020,2021))+
  labs(title = "Manchester United Forward and Midfielder Expected Goals with CR7")
  
# write.csv(mu_good, "/Users/jacktarricone/Desktop/twitter/cr7/mu_player_stats.csv")

############################
#### matchday standings ####
############################

# get epl table statistics for the 2020-2021 season
epl_table20 <-tm_matchday_table(country_name="England", start_year="2020", matchday=c(1:38))
head(epl_table20)

# filter for MU 2020
mu_table20 <-filter(epl_table20, squad == "Man Utd")
mu_table20 <-mutate(mu_table20,  pts_per_game = (pts / matchday))

# get epl table statistics for the 2021-2022 season
epl_table21 <-tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:22))

# filter for MU2022
mu_table21 <-filter(epl_table21, squad == "Man Utd")
mu_table21 <-mutate(mu_table21,  pts_per_game = (pts / matchday))

p1 <-ggplot() + 
  geom_line(mu_table20, mapping = aes(x=matchday, y=rk, color = "20-21")) +
  geom_line(mu_table21, mapping = aes(x=matchday, y=rk, color = "21-22")) +
  scale_colour_manual(values=c('20-21' = "red",'21-22' = "goldenrod")) +
  scale_x_continuous(breaks = seq(0,38,2))+
  scale_y_continuous(breaks = seq(0,18,1))+
  labs(title = "Manchester United EPL Standings by Matchday",
       y = "Position",
       x = "Match Day",
       color = "Season") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.75,.7),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot(p1)

# ggsave(p1,
#        file = "mu_standings_md21.png",
#        width = 7,
#        height = 5,
#        dpi = 400)





############################
####   goals scored     ####
############################


p2 <-ggplot() + 
  geom_line(mu_table20, mapping = aes(x=matchday, y=gf, color = "20-21")) +
  geom_line(mu_table21, mapping = aes(x=matchday, y=gf, color = "21-22")) +
  scale_colour_manual(values=c('20-21' = "blue",'21-22' = "firebrick")) +
  scale_x_continuous(breaks = seq(1,38,2))+
  scale_y_continuous(breaks = seq(0,80,5))+
  labs(title = "Manchester United EPL Goals for (Matchday 21)",
       y = "Goals For",
       x = "Match Day",
       color = "Season") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.20,.8),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot(p2)

ggsave(p2,
       file = "mu_gf_md21.png",
       width = 7,
       height = 5,
       dpi = 400)


############################
####   goals against    ####
############################


p3 <-ggplot() + 
  geom_line(mu_table20, mapping = aes(x=matchday, y=ga, color = "20-21")) +
  geom_line(mu_table21, mapping = aes(x=matchday, y=ga, color = "21-22")) +
  scale_colour_manual(values=c('20-21' = "orange",'21-22' = "darkgreen")) +
  scale_x_continuous(breaks = seq(1,38,2))+
  scale_y_continuous(breaks = seq(0,80,5))+
  labs(title = "Manchester United EPL Goals Against (Matchday 21)",
       y = "Goals Against",
       x = "Match Day",
       color = "Season") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.20,.8),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot(p3)

# ggsave(p3,
#        file = "mu_ga_md21.png",
#        width = 7,
#        height = 5,
#        dpi = 400)


############################
####   Points    ####
############################


p4 <-ggplot() + 
  geom_line(mu_table20, mapping = aes(x=matchday, y=pts, color = "20-21")) +
  geom_line(mu_table21, mapping = aes(x=matchday, y=pts, color = "21-22")) +
  scale_colour_manual(values=c('20-21' = "purple",'21-22' = "green")) +
  scale_x_continuous(breaks = seq(1,38,2))+
  scale_y_continuous(breaks = seq(0,80,5))+
  labs(title = "Manchester United EPL Points (Matchday 21)",
       y = "Points",
       x = "Match Day",
       color = "Season") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.20,.8),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot(p4)

ggsave(p4,
       file = "mu_pts_md21.png",
       width = 7,
       height = 5,
       dpi = 400)




############################
####   Points per game   ####
############################


p5 <-ggplot() + 
  geom_line(mu_table20, mapping = aes(x=matchday, y=pts_per_game, color = "20-21")) +
  geom_line(mu_table21, mapping = aes(x=matchday, y=pts_per_game, color = "21-22")) +
  scale_colour_manual(values=c('20-21' = "cyan",'21-22' = "firebrick")) +
  scale_x_continuous(breaks = seq(1,38,2))+
  scale_y_continuous(breaks = seq(0,3,.2))+
  labs(title = "Manchester United EPL Average Points Per Game (Matchday 21)",
       y = "Points Per Game",
       x = "Match Day",
       color = "Season") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = c(.80,.8),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 9, face = "bold"),
        legend.margin = margin(t=0, unit='cm'),
        legend.key = element_rect(size = .2))
plot(p5)

ggsave(p5,
       file = "mu_pts_per_game_md21.png",
       width = 7,
       height = 5,
       dpi = 400)



head(mu_table20)









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



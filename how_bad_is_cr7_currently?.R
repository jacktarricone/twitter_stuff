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

# add new metrics
players_df <-mutate(players_df,
  shots_per_90min = (shots / time) * 90,
  goals_per_90min = (goals / time) * 90 
)

# add shots per goal
players_df <-mutate(players_df,
       shots_per_goal = (shots_per_90min / goals_per_90min)
)

# filter for fowards and strikers
forwards <-filter(players_df, str_detect(position, 'F'))
#strikers <-filter(players_df, str_detect(position, 'S'))

# filter out cr7
cr7 <-filter(forwards, player_id == "2371")
cr7_gp90 <-cr7$goals_per_90min

# quick plot
hist(forwards$shots_per_90min, breaks = 40)

# test plot
theme_set(theme_classic(12))

# goals per 90
ggplot(forwards) + 
  geom_histogram(aes(goals_per_90min), fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=cr7_gp90, linetype="dashed", color = "red") + 
  annotate("text",x = .26, y = 20, label = "CR7", color = "red") +
  labs(title = "EPL 2022 Fowards/Strikers Goals per 90 Minutes",
       x = "Goals per 90 mintues",
       y = "Count")

# ggsave(file = "forwards_goals_per_90_2020.png",
#        width = 6,
#        height = 4,
#        dpi = 400)


# shots per 90
cr7_sp90 <-cr7$shots_per_90min

ggplot(forwards) + 
  geom_histogram(aes(shots_per_90min), fill="darkviolet", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=cr7_sp90, linetype="dashed", color = "red") + 
  annotate("text",x = 4, y = 18, label = "CR7", color = "red") +
  labs(title = "EPL 2022 Fowards/Strikers Shots per 90 Minutes",
       x = "Shots per 90 mintues",
       y = "Count")

# ggsave(file = "forwards_shots_per_90_2020.png",
#        width = 6,
#        height = 4,
#        dpi = 400)

# shots per goal
cr7_spg <-cr7$shots_per_goal

ggplot(forwards) + 
  geom_histogram(aes(shots_per_goal), fill="firebrick", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=cr7_spg, linetype="dashed", color = "black") + 
  annotate("text", x = 23.6, y = 18, label = "CR7", color = "black") +
  labs(title = "EPL 2022 Fowards/Strikers Shots per goal",
       x = "Shots per goal",
       y = "Count")

ggsave(file = "forwards_shots_per_goal_2020.png",
       width = 6,
       height = 4,
       dpi = 400)



# above 0 gp90
forwards_0 <-filter(forwards, goals_per_90min > 0)
hist(forwards_0$goals_per_90min)

# goals per 90
ggplot(forwards_0) + 
  geom_histogram(aes(goals_per_90min), fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  geom_vline(xintercept=cr7_gp90, linetype="dashed", color = "red") + 
  annotate("text",x = .26, y = 20, label = "CR7", color = "red") +
  labs(title = "EPL 2022 Fowards/Strikers Goals per 90 Minutes",
       x = "Goals per 90 mintues",
       y = "Count")

ggsave(file = "forwards_goals_per_90_2020.png",
       width = 6,
       height = 4,
       dpi = 400)

# relocate cols
forwards <-forwards %>% relocate(shots_per_90min)
forwards <-forwards %>% relocate(goals_per_90min)
forwards$team <-as.numeric(forwards$team)

# max goals per team
max_gp90 <-forwards %>% 
  group_by(team) %>%
  filter(goals_per_90min == max(goals_per_90min))

# print
max_gp90$goals_per_90min <-round(max_gp90$goals_per_90min, digits = 2)
max_gp90$shots_per_90min <-round(max_gp90$shots_per_90min, digits = 2)
print(max_gp90)

 
max_gp90 <-mutate(max_gp90,
                    shots_per_goal = (shots_per_90min / goals_per_90min)
)




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



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
forwards <-forwards %>% relocate(shots_per_goal)
forwards$team <-as.numeric(forwards$team)

# max goals per team
max_gp90 <-forwards %>% 
  group_by(team) %>%
  filter(goals_per_90min == max(goals_per_90min))

# min
# min_spg <-forwards %>% 
#   group_by(team) %>%
#   filter(shots_per_goal == min(shots_per_goal))

# round
max_gp90$goals_per_90min <-round(max_gp90$goals_per_90min, digits = 2)
max_gp90$shots_per_90min <-round(max_gp90$shots_per_90min, digits = 2)
max_gp90$shots_per_goal <-round(max_gp90$shots_per_goal, digits = 2)

#
print(max_gp90)

write.csv(max_gp90, "max_gp90_per_team.csv") 
mean(max_gp90$shots_per_goal)

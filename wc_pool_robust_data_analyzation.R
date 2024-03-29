library(googlesheets4)
library(tibble)
library(tidyverse)
library(data.table)


## group sheet
group_sheet <-read.csv("/Users/jacktarricone/Desktop/twitter/2022 Qatar Bracket Challenge - Group Stage.csv")

# remove undeeded data
group_v2 <-group_sheet[-c(1,2:35,37,38),-c(1:2)]
head(group_v2)
group_points <-transpose(group_v2)

# define url to sheed and read in jsut top section
url <-"https://docs.google.com/spreadsheets/d/1IQZRgsarCoNi18ZZwgC9SczFyPZZmD0kgNvJZZS7q08/edit#gid=1704876733"
names_raw <-range_read(url, range = "A1:AB1", sheet = "Knockout Rounds")
names_v1 <-colnames(names_raw)
names <-names_v1[-1]

# knockout points
points_raw <-range_read(url, range = "A24:AB24", sheet = "Knockout Rounds")
pr_v2 <-colnames(points_raw) # extract col names
pr_v3 <-substr(pr_v2,1,nchar(pr_v2)-4) # remove name change 
pr_v4 <-pr_v3[-1]
knockout_points <-as.numeric(pr_v4)

# make table
standings <-as.data.frame(cbind(names,group_points$V1,knockout_points))

# rename cols
colnames(standings)[1] <-"Participant"
colnames(standings)[2] <-"Group Points"
colnames(standings)[3] <-"Knockout Points"

# make int
standings$`Group Points` <-as.integer(standings$`Group Points`)
standings$`Knockout Points` <-as.integer(standings$`Knockout Points`)

# add total
standings$Total <-standings$`Group Points`+standings$`Knockout Points`

# order by total
ordered <-standings[order(-standings$Total),]
ordered



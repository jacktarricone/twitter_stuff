library(googlesheets4)
library(tibble)

# define url to sheed and read in jsut top section
url <-"https://docs.google.com/spreadsheets/d/1IQZRgsarCoNi18ZZwgC9SczFyPZZmD0kgNvJZZS7q08/edit#gid=1525044235"
picks <-as.data.frame(range_read(url, range = "A1:AC17"), row.names = FALSE)
head(picks)

### format for analysis
group <-c("A","A","B","B","C","C","D","D","E","E","F","F","G","G","H","H")
place <-c("1st","2nd","1st","2nd","1st","2nd","1st","2nd",
          "1st","2nd","1st","2nd","1st","2nd","1st","2nd")

# group
picks$Picks <-group
names(picks)[1] <-"group"

# place
picks$'...2' <-place
names(picks)[2] <-"place"

# current results
results <-c("Netherlands", "Senegal", # A
            "England", "USA",         # B
            "NA", "NA",               # C
            "NA", "NA",               # D
            "NA", "NA",               # E
            "NA", "NA",               # F
            "NA", "NA",               # G
            "NA", "NA")               # H

# add results
picks <-add_column(picks, results, .after = "place")

# make standings table
Participant <-colnames(picks[4:30])
Participant 

Points <-rep(NA,27)

# cbind
standings <-as.data.frame(cbind(Participant, Points), row.names = FALSE)
standings


picks$Reany




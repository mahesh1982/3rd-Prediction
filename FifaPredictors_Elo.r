#setwd("~/Documents/My-Courses/IIITB/FIFA Challenge/Elo")
setwd("C:/Mahesh/Personal/Projects/R/Upgrad/Prediction Model/Before Finalizing")
library(dplyr)
library(readr)
library(elo)

matches <- read_csv('results.csv')
teams <- data.frame(team = unique(c(matches$home_team, matches$away_team)))
teams <- teams %>% mutate(elo = 1500)
matches <- matches %>% 
  mutate(result = ifelse(home_score > away_score, 1, 
                         ifelse(home_score == away_score, 0.5, 0)))
head(matches)
head(teams)

for (i in seq_len(nrow(matches))) {
  match <- matches[i, ]
  
  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$home_team)$elo
  teamB_elo <- subset(teams, team == match$away_team)$elo
  
  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)
  
  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]
  
  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  teams <- teams %>%
    mutate(elo = if_else(team == match$home_team, teamA_new_elo,
                         if_else(team == match$away_team, teamB_new_elo, elo)))
}

teams %>%
  arrange(-elo) %>%
  head

WC_teams <- teams %>%
  filter(team %in% c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                     "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                     "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                     "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                     "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                     "Korea Republic", "Saudi Arabia")) %>%
  arrange(-elo)

print.data.frame(WC_teams)

print("Mexico Vs Sweden")
Mexico <- subset(WC_teams, team == "Mexico")$elo
Sweden <- subset(WC_teams, team == "Sweden")$elo
elo.prob(Mexico, Sweden)

print("Germany Vs Korea Republic")
Germany <- subset(WC_teams, team == "Germany")$elo
Korea_Republic <- subset(WC_teams, team == "Korea Republic")$elo
elo.prob(Germany, Korea_Republic)

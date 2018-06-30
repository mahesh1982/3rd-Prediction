setwd("~/Documents/My-Courses/IIITB/FIFA Challenge/Elo/3rd Prediction")
#setwd("C:/Mahesh/Personal/Projects/R/Upgrad/Prediction Model/Before Finalizing")
library(dplyr)
library(elo)
library(readr)
library(crayon)

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

dendogram_fifa <- c()

won_battle <- function(team1, team2) {
  print(paste(team1, team2, ""))
  team_1 <- subset(WC_teams, team == team1)$elo
  team_2 <- subset(WC_teams, team == team2)$elo
  res_1 <- elo.prob(team_1, team_2)
  if (res_1 > 0.5) {
    return(team1)
    dendogram_fifa <- c(dendogram_fifa, team1)
    dendogram_fifa <- c(dendogram_fifa, res_1)
    dendogram_fifa <- c(dendogram_fifa, (1-res_1))
  } else {
    return(team2)
    dendogram_fifa <- c(dendogram_fifa, team2)
    dendogram_fifa <- c(dendogram_fifa, res_1)
    dendogram_fifa <- c(dendogram_fifa, (1-res_1))
  }
}

print("France Vs Argentina")

#Round 16 matches
round16_teams <-   matrix(c("France", "Argentina",
                     "Uruguay", "Portugal",
                     "Brazil", "Mexico",
                     "Belgium", "Japan",
                     "Spain", "Russia",
                     "Croatia", "Denmark",
                     "Sweden", "Switzerland",
                     "Colombia", "England"),ncol = 2, byrow = TRUE)
res_R16 <- c()

for (i in seq(1,8, by=1)){
  t1 <- round16_teams[i, 1]
  t2 <- round16_teams[i, 2]
  res <- won_battle(t1, t2)
  #print("Round 16 Winner is:")
  #print(res)
  res_R16 <- c(res_R16, res)
}
print(res_R16)

#Quter Finals
res_Qua <- c()
Qua <-   matrix(res_R16,ncol = 2, byrow = TRUE)
for (j in seq(1,4, by=1)){
  q1 <- Qua[j, 1]
  q2 <- Qua[j, 2]
  res_q <- won_battle(q1, q2)
  #print("Quatters Winner is:")
  #print(res_q)
  res_Qua <- c(res_Qua, res_q)
}
#print(res_Qua)

#Semis
sem_1 <- won_battle(res_Qua[1], res_Qua[2])
#print("Semmis 1 Winner is:")
#print(sem_1)

sem_2 <- won_battle(res_Qua[3], res_Qua[4])
#print("Semmis 2 Winner is:")
#print(sem_2)

#Final
final <- won_battle(sem_1, sem_2)
#print("Grand Final Winner is:")
#print(final)

cat(bold(silver("\nRound 16 Winners Are:\n")))
cat(blue(res_R16))
cat(bold(silver("\nQuarter Final Winners are:\n")))
cat(magenta(res_Qua))
cat(bold(silver("\nSemi Final Winners are:\n")))
cat(yellow(c(sem_1, sem_2)))
cat(bold(silver("\nGrand Final Winner is:\n")))
cat(bold(green(final)))

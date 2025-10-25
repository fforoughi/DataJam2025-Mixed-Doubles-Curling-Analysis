#Compling code and libraries
library(readr)
library(dplyr)
library(ggplot2)
games <- read_csv("..DSSSHackathon 2025/Games.csv")
StonesMain <- read_csv("../DSSSHackathon 2025/Stones.csv")

#Finding average points for each team and each player
stones <- StonesMain |> group_by(TeamID) |> select("TeamID","PlayerID","Task","Points") |> filter(Points < 4) |> filter(Points > -1)
pointsAVG <- stones |> group_by(TeamID,PlayerID) |> summarize(avg = mean(Points))
model = lm(avg ~ TeamID, data = pointsAVG)

#Model plots for average teams + outliers of players
ggplot(pointsAVG, aes(x = TeamID, y = avg)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Linear Regression: Average Points vs Team ID",
    x = "Team ID",
    y = "Average Points"
  ) +
  theme_minimal()
#Model for boxplot that showcases distribution of average points by player
ggplot(pointsAVG, aes(x = as.factor(PlayerID), y = avg)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Distribution of Average Points by Player",
    x = "Player ID",
    y = "Average Points"
  ) +
  theme_minimal()


 

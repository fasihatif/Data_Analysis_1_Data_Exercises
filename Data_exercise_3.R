library(tidyverse)


#Filter dataset for 2018 season
epl_2017 <- epl_games %>% filter(season == 2017)

glimpse(epl_2017)


# Rankings takble for 2018 based on previous years performance

rankings_home <- epl_2017 %>%
  group_by("team" = team_home) %>%
  summarise("total_home_points" = sum(points_home))
  

rankings_away <- epl_2017 %>%
  group_by("team" = team_away) %>%
  summarise("total_away_points" = sum(points_away))

rankings <- merge(rankings_home,rankings_away, by = "team")


rankings$total_points <- rankings$total_home_points + rankings$total_away_points
rankings <- rankings %>% arrange(-total_points)

------------------
epl_2018 <- epl_games %>% filter(season == 2018)  

top_teams <- epl_2018 %>%
  filter(team_home %in% c("Man City","Man United","Tottenham","Liverpool","Chelsea","Arsenal"))
  
middle_teams <- epl_2018 %>%
  filter(team_home %in% c("Burnley", "Everton", "Leicester", "Bournemouth", "Crystal Palace", "Newcastle", "West Ham"))

bottom_teams <- epl_2018 %>%
  filter(team_home %in% c("Watford","Brighton","Huddersfield","Southampton","Wolves","Fulham","Cardiff"))
         


-------------------------------------
  
top_teams <- mutate(top_teams, "home_advantage" = goals_home - goals_away)
middle_teams <- mutate(middle_teams, "home_advantage" = goals_home - goals_away)
bottom_teams <- mutate(bottom_teams, "home_advantage" = goals_home - goals_away)

-------------------------------------

library(lattice)
histogram(top_teams$home_advantage,
          main = "Distribution of goal diff on home pitch (Top Teams)",
          xlab = "Goal Difference",
          ylab = "Share of games (percent)",
          col = "red",
          breaks = 8)

histogram(middle_teams$home_advantage,
          main = "Distribution of goal diff on home pitch (Middle Teams)",
          xlab = "Goal Difference",
          ylab = "Share of games (percent)",
          col = "steelblue",
          breaks = 8)

histogram(bottom_teams$home_advantage,
          main = "Distribution of goal diff on home pitch (Bottom Teams)",
          xlab = "Goal Difference",
          ylab = "Share of games (percent)",
          col = "grey",
          breaks = 8)



top <- c("Man City","Man United","Tottenham","Liverpool","Chelsea","Arsenal")
middle <- c("Burnley", "Everton", "Leicester", "Bournemouth", "Crystal Palace", "Newcastle", "West Ham")
bottom <- c("Watford","Brighton","Huddersfield","Southampton","Wolves","Fulham","Cardiff")

--------------------------------------
  
  



 
   ?mutate                          
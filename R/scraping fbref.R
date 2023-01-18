library(worldfootballR)
big5_player_standard <- fb_big5_advanced_season_stats(season_end_year= 2023, stat_type= "standard", team_or_player= "player")
dplyr::glimpse(big5_player_standard)
LaLiga_fbref_standard_stats <- big5_player_standard %>% 
  filter(Comp=="La Liga")
library(readr)
write_csv(LaLiga_fbref_standard_stats, "LaLiga_fbref_standard_stats.csv")

big5_player_shooting <- fb_big5_advanced_season_stats(season_end_year= 2023, stat_type= "shooting", team_or_player= "player")
dplyr::glimpse(big5_player_shooting)
LaLiga_fbref_shooting_stats <- big5_player_shooting %>% 
  filter(Comp=="La Liga")
write_csv(LaLiga_fbref_shooting_stats, "LaLiga_fbref_shooting_stats.csv")

big5_player_passing <- fb_big5_advanced_season_stats(season_end_year= 2023, stat_type= "passing", team_or_player= "player")
dplyr::glimpse(big5_player_passing)
LaLiga_fbref_passing_stats <- big5_player_passing %>% 
  filter(Comp=="La Liga")
write_csv(LaLiga_fbref_passing_stats, "LaLiga_fbref_passing_stats.csv")

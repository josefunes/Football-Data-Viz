install.packages("installr")
library(installr)
updateR()

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(tidyverse, ## mainly dplyr, purrr, and tidyr
               StatsBombR, SBpitch, soccermatics,
               extrafont, ggupset, tibbletime,
               ggtext, ggrepel, glue,
               patchwork, cowplot, gtable, grid,
               magick)

## loading fonts
loadfonts(device = "win", quiet = TRUE)

install.packages("worldfootballR")
install.packages("ggsoccer")
install.packages("tidyverse")
install.packages("here")
remotes::install_github('ewenme/understatr', force = TRUE)
install.packages("ggrepel")
install.packages("openxlsx")
library(openxlsx)
library(worldfootballR)
library(tidyverse)
library(here) ##for saving output - more stable than set_wd()
library(ggrepel)
library(ggsoccer)
library(glue)
library(understatr)

laliga_shot_location <- understat_league_season_shots(league = "La liga", season_start_year = 2022)

####################################################################
# Añadir nueva columna para poder filtrar por los tiros de un equipo
####################################################################
# number of rows in data frame
num_rows = nrow(laliga_shot_location)

# creating ID column vector
ID <- c(1:nrow(laliga_shot_location))

listaEquiposLaLiga <- c()
for (i in c(1:nrow(laliga_shot_location))) {
  if (laliga_shot_location$h_a[i] == "h") {
    equipo <- laliga_shot_location$home_team[i]
  } 
  else {
    equipo <- laliga_shot_location$away_team[i]
  }
  listaEquiposLaLiga  <- c(listaEquiposLaLiga, equipo)
}
laliga_shot_location$team <- listaEquiposLaLiga 



listaPartidosLaLiga <- c()
for (i in c(1:nrow(laliga_shot_location))) {
  local <- paste(laliga_shot_location$home_team[i], laliga_shot_location$home_goals[i], sep = " ")
  visitante <- paste(laliga_shot_location$away_team[i], laliga_shot_location$away_goals[i], sep = " ")
  partido <- paste(local, visitante, sep = " - ")
  listaPartidosLaLiga <- c(listaPartidosLaLiga, partido)
}
laliga_shot_location$partido <- listaPartidosLaLiga

laliga_shot_location$minute <- as.integer(laliga_shot_location$minute) 

listaIntervalosTiempo <- c()
for (i in c(1:nrow(laliga_shot_location))) {
  if (laliga_shot_location$minute[i] >= 0 &
      laliga_shot_location$minute[i] < 16) {
    tiempo <- print("0-15")
  } 
  else if (laliga_shot_location$minute[i] >= 16 &
           laliga_shot_location$minute[i] < 31) {
    tiempo <- print("16-30")
  }
  else if (laliga_shot_location$minute[i] >= 31 &
           laliga_shot_location$minute[i] < 46) {
    tiempo <- print("31-45")
  }
  else if (laliga_shot_location$minute[i] >= 46 &
           laliga_shot_location$minute[i] < 61) {
    tiempo <- print("46-60")
  }
  else if (laliga_shot_location$minute[i] >= 61 &
           laliga_shot_location$minute[i] < 76) {
    tiempo <- print("61-75")
  }
  else {
    tiempo <- print("76+")
  }
  listaIntervalosTiempo <- c(listaIntervalosTiempo, tiempo)
}
laliga_shot_location$intervalo <- listaIntervalosTiempo

laliga_shot_location$minute <- as.integer(laliga_shot_location$minute)

# write.xlsx(laliga_shot_location, "laliga_shots.xlsx")

data <- laliga_shot_location

data[1, 9]

# Cambiamos para poner los visitantes en la otra parte del campo (izquierda)
for (i in 1:nrow(data)) {
  if (data[i, 9] == 'a'){
    data[i, 5] = 1 - data[i, 5]
    data[i, 6] = 1 - data[i, 6]
  }
}


data$X <- round(data$X,3)
write.xlsx(data, "laliga_shots.xlsx")

data_alm_atm <- subset(data, home_team == 'Almeria' & away_team == 'Atletico Madrid')
matches <- unique(data_alm$partido)
write.xlsx(data_alm_atm, "data_alm_atm_shots.xlsx")

data_alm_atm$xG <- as.double(data_alm_atm$xG)
data_alm_atm$xG <- round(data_alm_atm$xG,3)

for (i in c()) {
  data_alm_atm[nrow(data_alm_atm)+1,] = c("", "", i, 'MissedShots', "", "", 0, "", "", "", "", "", "", "", "", "", "", "", "", "", "", 'Almeria', "", "")
}

data_alm_atm$minute <- as.integer(data_alm_atm$minute)


alm_atm_rollsum <- data_alm_atm %>% 
  group_by(minute, team) %>% 
  summarize(sumxg = sum(xG)) %>% 
  ungroup() %>% 
  group_by(team) %>% 
  mutate(rollsum = lag(cumsum(sumxg)),
         rollsum = if_else(is.na(rollsum), 0, rollsum)) %>% 
  select(team, minute, rollsum, sumxg) %>%
  mutate(rollsum = case_when(
    row_number() == n() & sumxg != 0 ~ rollsum + sumxg,
    TRUE ~ rollsum
  ))

alm_atm_rollsum <- alm_atm_rollsum %>% 
  left_join(data_alm_atm %>% filter(result == "Goal") %>% select(minute, result, team, player), 
            by = c("minute", "team")) %>% 
  mutate(rollsum_goal = rollsum + sumxg,
         minute_goal = minute + 1,
         player_label = case_when(
           result == "Goal" ~ glue::glue("{player}: {sumxg %>% signif(digits = 2)} xG"),
           TRUE ~ ""))

glimpse(alm_atm_rollsum)

alm_atm_xg <- data_alm_atm %>% 
  group_by(team) %>% 
  summarize(tot_xg = sum(xG) %>% signif(digits = 2)) %>% 
  mutate(team_label = glue::glue("{team}: {tot_xg} xG"))

tot_alm_atm_df <- alm_atm_xg %>% 
  pull(tot_xg)

unique(alm_atm_rollsum$team)


alm_atm_rollsumxg_plot <- alm_atm_rollsum %>% 
  ggplot(aes(x = minute, y = rollsum, 
             group = team, color = team)) +
  geom_line(size = 2.5) +
  geom_label_repel(data = alm_atm_rollsum %>% filter(result == "Goal"),
                   aes(x = minute_goal, y = rollsum_goal, 
                       color = team, label = player_label), 
                   nudge_x = 6, nudge_y = 0.15, family = "Roboto Condensed",
                   show.legend = FALSE) +
  geom_point(data = alm_atm_rollsum %>% filter(result == "Goal"),
             aes(x = minute_goal, y = rollsum_goal, color = team), show.legend = TRUE,
             size = 5, shape = 21, fill = "white", stroke = 1.25) +
  scale_color_manual(values = c("Almeria" = "#a50044",
                                "Atletico Madrid" = "#000000"),
                     labels = c("Almeria" = "Almeria",
                                "Atletico Madrid" = "Atlético de Madrid")) +
  scale_fill_manual(values = c("Almeria" = "#a50044",
                               "Atletico Madrid" = "#000000")) +
  scale_x_continuous(breaks = c(seq(0, 90, by = 5), 94),
                     labels = c(seq(0, 40, by = 5), "D", 
                                seq(50, 90, by = 5), "FT"),
                     expand = c(0.01, 0),
                     limits = c(0, 94)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = tot_alm_atm_df)) +
  labs(title = "<b style='color: black; font-size: 35px'>UD Almería: 1 </b><b style='color: black; font-size: 20px'>(13º, 18 pts.)</b><br> <b style='color:#a50044; font-size: 35px'>Atlético de Madrid: 1 </b><b style ='color:#a50044; font-size: 20px'>(4º, 28 pts.)</b>",
       subtitle = "15 Enero, 2023 (Jornada 17)",
       x = NULL,
       y = "Goles Esperados (xG)") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 40, family = "Roboto Condensed"),
        plot.subtitle = element_text(size = 18, family = "Roboto Condensed",
                                     color = "grey20"),
        axis.title = element_text(size = 18, color = "grey20"),
        axis.text = element_text(size = 16, face = "bold"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = c(0.25, 0.85),
        legend.direction = "horizontal",
        legend.title = element_blank())

alm_atm_rollsumxg_plot

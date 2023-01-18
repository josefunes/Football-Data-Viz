install.packages("worldfootballR")
install.packages("ggsoccer")
install.packages("tidyverse")
install.packages("here")
remotes::install_github('ewenme/understatr')
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


# write.xlsx(laliga_shot_location, "laliga_shots.xlsx")

data <- laliga_shot_location
lista <- 1-laliga_shot_location$X

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



data_alm <- subset(data, home_team == 'Almeria' | away_team == 'Almeria')
matches <- unique(data_alm$partido)

rivales <- c()
partidosAlm <- c()
puntosPerdidos <- c()
for (i in 1:length(matches)) {
  puntos <- 0
  resultMin75 <- getResultadoPrevio(matches[i])
  resultFinal <- getResultadoFinal(matches[i])
  if (resultMin75 > 0 & resultFinal == 0) {
    puntos <- -2
  }
  if (resultMin75 > 0 & resultFinal < 0) {
    puntos <- -3
  }
  if (resultMin75 == 0 & resultFinal < 0) {
    puntos <- -1
  }
  puntosPerdidos <- c(puntosPerdidos, puntos)
  partidosAlm <- c(partidosAlm, matches[i])
}

getResultadoPrevio <- function(partidoAlm) {
  golesFavor <- 0
  golesContra <- 0
  dif <- 0
  filtrado <- subset(data_alm, data_alm$partido == partidoAlm & data_alm$intervalo != '76+' & data_alm$result == 'Goal')
  if (nrow(filtrado) > 0) {
    for (i in c(1:nrow(filtrado))) {
      if (filtrado$team[i] == 'Almeria') {
        golesFavor <- golesFavor + 1
      }
      else {
        golesContra <- golesContra + 1
      }
    }
  }
  return(golesFavor - golesContra)
}

getResultadoFinal <- function(partidoAlm) {
  golesFavor <- 0
  golesContra <- 0
  dif <- 0
  filtrado <- subset(data_alm, data_alm$partido == partidoAlm)
  if (nrow(filtrado) > 0) {
    if (filtrado$home_team[1] == 'Almeria') {
      golesFavor <- filtrado$home_goals[1]
      golesContra <- filtrado$away_goals[1]
    }
    else {
      golesFavor <- filtrado$away_goals[1]
      golesContra <- filtrado$home_goals[1]
    }
  }
  return(golesFavor - golesContra)
}


ptsPerd <- data.frame(Partido = partidosAlm, Puntos_Perdidos = puntosPerdidos)
write.xlsx(ptsPerd, "puntosPerdidos.xlsx")


####################################################################
# Añadir nueva columna para poder filtrar por los tiros de un equipo
####################################################################
# number of rows in data frame
num_rows = nrow(uda_shots)

# creating ID column vector
ID <- c(1:num_rows)

listaEquipos <- c()
for (i in ID) {
  if (uda_shots$home_away[i] == "h") {
    equipo <- uda_shots$home_team[i]
  } 
  else {
    equipo <- uda_shots$away_team[i]
  }
  listaEquipos <- c(listaEquipos, equipo)
}
uda_shots$team <- listaEquipos



listaPartidos <- c()
for (i in ID) {
  local <- paste(uda_shots$home_team[i], uda_shots$home_goals[i], sep = " ")
  visitante <- paste(uda_shots$away_team[i],uda_shots$away_goals[i], sep = " ")
  partido <- paste(local, visitante, sep = " - ")
  listaPartidos <- c(listaPartidos, partido)
}

uda_shots$partido <- listaPartidos


uda_shots <- understat_team_season_shots(team_url = "https://understat.com/team/Almeria/2022")
dplyr::glimpse(uda_shots)


uda_shots2 <- uda_shots %>% filter(team=="Almeria")

uda_shots3 <- uda_shots2[which(uda_shots2$player %in% c("Largie Ramazani","Rodrigo Ely","Srdjan Babic","Lucas Robertone","Leo Baptistao","Adrián Embarba","El Bilal Touré","Gonzalo Melero","Sergio Akieme")),]
##plot half pitch
#ggplot()+
#  annotate_pitch(dimensions = pitch_statsbomb)+
#  theme_pitch()+
#  coord_flip(xlim = c(60,120),
#             ylim = c(0,80))

titulo_grafico<-"Representación tiros de los Jugadores del Almería"

##plot half pitch
uda_shots3 %>% 
  mutate(X = X*100,
         Y = Y*100) %>% 
  ggplot(aes(x = X, y = 100-Y))+
  annotate_pitch()+
  geom_point(aes(colour = result, size = xG)) +
  coord_flip(xlim = c(50,100),
             ylim = c(0,100)) +
  theme_pitch() +
  labs(title = glue({titulo_grafico}),
       subtitle = "Shot Locations 2022/2023 - Penalties Removed") +
  geom_point(data = uda_shots3 %>% 
               filter(result=='Goal'),
             aes(x = X*100,
                 y = Y*100,
                 size = xG),
             shape = 21,
             fill = "#40d71e",
             stroke = 0.6,
             colour = "black")+
  scale_colour_manual(values = c("#dc6d03", "#40d71e", "#d6f910", "#3c7adf", "#663535"), 
                      name = "Resultado", 
                      labels = c("Bloqueado", "Gol", "Fuera", "Parado", "Al palo"))+
  # Esta instrucción divide el gráfico en 9 campos iguales, con los datos de los 9 jugadores que hemos filtrado
  facet_wrap(~player)





##pull player meta data
teams <- unique(laliga_shot_location$home_team)
teams_url <-understat_team_meta(unique(laliga_shot_location$home_team))
urls <- teams_url %>% filter(teams_url$year == 2022)

url_team_list <- urls$url

team_players <- understat_team_players_stats(team_url = url_team_list)
dplyr::glimpse(team_players)
team_players

##create some summary stats
summary<-team_players %>% 
  mutate(nineties = time/90,
         npxg_p90 = npxG/nineties,
         xa_p90 = xA/nineties)

##save player meta data
#write_csv(summary, here("player_meta_2020.csv"))
summary2 <- summary %>% filter(nineties>6) %>% arrange(-npxg_p90)
##quick plot npxg_p90 & xa_p90
summary2  %>% 
  ggplot(aes(x=npxg_p90, y=xa_p90))+
  geom_point() +
  geom_text_repel(data = summary2 %>% 
                    filter(npxg_p90>0.4 | xa_p90>0.4),
                  aes(label = player_name))+
  labs(title = "Contribución ofensiva",
       subtitle = "LaLiga Santander // > 6 partidos",
       x = "NPxG P90",
       y = "xA P90")

##top 9 players by NPxG P90
summary3<-summary2 %>% 
  slice(1:10)

install.packages("remotes")
remotes::install_github("clauswilke/ggtextures", force = TRUE)
install.packages(ggimage)
library(ggimage)
library(tidyverse)
library(magick)
library(ggtextures)
img <- list.files(system.file("extdata", package="ggimage"),
                  pattern="png", full.names=TRUE)

Almeria <- image_read("ud almeria.png")
# Mostrar la imagen
#print(Almeria, info = FALSE)
Barcelona <- image_read("barcelona.png")
RealMadrid <- image_read("real madrid.png")
Villarreal <- image_read("villarreal.png")
RealSociedad <- image_read("real sociedad.png")
Sevilla <- image_read("sevilla.png")

escudos <- c()

for (i in c(1:nrow(summary3))) {
  if (summary3$team_name[i] == 'Almeria') {
    escudo <- image_read("ud almeria.png")
  }
  else if (summary3$team_name[i] == 'Barcelona') {
    escudo <- image_read("barcelona.png")
  }
  else if (summary3$team_name[i] == 'Real Madrid') {
    escudo <- image_read("real madrid.png")
  }
  else if (summary3$team_name[i] == 'Villarreal') {
    escudo <- image_read("villarreal.png")
  }
  else if (summary3$team_name[i] == 'Real Sociedad') {
    escudo <- image_read("real sociedad.png")
  }
  else {
    escudo <- image_read("sevilla.png")
  }
  escudos <- c(escudos, escudo)
}

summary3$logos <- escudos

getwd()
setwd("C:/Users/Usuario/OneDrive/Escritorio/LaLiga Santander")
##bar plot top 9 players
ac_top_graph <- ggplot(data = summary3,
       aes(x = npxg_p90, 
           y = reorder(player_name, npxg_p90), image = logos))+
  
  geom_col(fill = "midnightblue") +
  
  geom_text(aes(label = round(npxg_p90, digits = 2)), 
            colour = "white", 
            hjust = 1.2, 
            fontface = "bold", 
            size = 5) +
  
  labs(title = "LaLiga Santander Non Penalty xG Per 90 min (NPxG P90)",
       subtitle = "2022/2023 Hasta Jornada 16",
       x = "NPxG P90",
       y = "",
       caption = "Data: Understats | @UdalmeriaD") +
  
  theme_minimal()+
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 15),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) 

pacman::p_load(tidyverse, scales, lubridate, ggrepel, stringi, magick, 
               glue, extrafont, rvest, ggtextures, cowplot, ggimage, polite)

axis_image <- axis_canvas(ac_top_graph, axis = 'y') + 
  draw_image("https://upload.wikimedia.org/wikipedia/commons/c/ca/Flag_of_Iran.svg", 
             y = 13, scale = 1.5) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/0/09/Flag_of_South_Korea.svg", 
             y = 10, scale = 1.7) +
  draw_image("https://upload.wikimedia.org/wikipedia/en/9/9e/Flag_of_Japan.svg", 
             y = 7, scale = 1.7) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/f/f6/Flag_of_Iraq.svg", 
             y = 4, scale = 1.6) +
  draw_image("https://upload.wikimedia.org/wikipedia/commons/a/aa/Flag_of_Kuwait.svg", 
             y = 1, scale = 1.2)

ggdraw(insert_yaxis_grob(ac_top_graph, axis_image, position = "left"))
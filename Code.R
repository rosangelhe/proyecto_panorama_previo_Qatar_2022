#instalar en caso de no poseer

#install.packages("readr")
#install.packages("ggthemes")
#install.packages("gganimate")
#install.packages("reactablefmtr")
#install.packages("reactable")
#install.packages("ggsoccer")

library(tidyverse)#manejo de datos
library(dplyr)
library(readr)    #lectura de cvs
library(readxl)   #lectura de xlsx
library(ggplot2)  #Manejo de Grafico
library(ggthemes) #Manejo de temas en graficos
library(gganimate)#creacion de animaciones en graficos
library(reactablefmtr) #mejora el estilo y el formato de las tablas 
library(reactable) #mejora el estilo y el formato de las tablas 
library(ggsoccer) #creacion de graficos de juego



#Carga de la base de datos en formato .CSV
world_cup_matches <- read_csv("World+Cup/world_cup_matches.csv")
X2022_world_cup_groups <- read_csv("World+Cup/2022_world_cup_groups.csv")
X2022_world_cup_squads <- read_csv("World+Cup/2022_world_cup_squads.csv")
attendance_Sheet <- read_csv("World+Cup/Attendance Sheet.csv")
FIFA_WORLD_CUP_2022 <- read_excel("World+Cup/FIFA_WORLD_CUP_2022.xlsx")
the_final <- read_csv("World+Cup//the-final.csv")
player_Goals <- read_csv("World+Cup//Player Goals.csv")
player_Assists <- read_csv("World+Cup//Player Assists.csv")
team_Assists <- read_csv("World+Cup/Team Assists.csv")
the_final <- read_csv("World+Cup/the-final.csv")
team_Penalties_Scored <- read_csv("World+Cup/Team Penalties Scored.csv")

head(X2022_world_cup_groups)
head(world_cup_matches)
head(X2022_world_cup_squads)
#----------- Parte I Limpieza y procesamiento de los datos -------------#

# 1 Cantidad de anotaciones por selección en el torneo FIFA World Cup desde el primer torneo oficial en Uruguay 1930

# Separacion de la tabla por equipo segun los goles obtenidos en el torneo
wcup_goal <- world_cup_matches[c("Home Team", "Home Goals","Away Goals", "Away Team")]

#Creacion de una nueva columna en el data frame
team_column <- c("Team", "Goals")

#filtro y asignacion de goles obtenidos en condicion de Home dentro de team_column
h_cup <- wcup_goal %>% 
  group_by(`Home Team`) %>%
  summarise(`Home Goals` = sum(`Home Goals`),
            )
colnames(h_cup) <- c("Team", "Home")

#filtro y asignacion de goles obtenidos en condicion de Away dentro de team_column
a_cup <- wcup_goal %>%
  group_by(`Away Team`) %>%
  summarise(`Away Goals` = sum(`Away Goals`),
  )
colnames(a_cup) <- c("Team", "Away")

#Union y reodenamiento de las tablas de goles obtenidos en un orden descendente 
total_gol <- merge(h_cup, a_cup, by = "Team", all = TRUE)
total_gol$total <- total_gol$Home + total_gol$Away
total_gol <- total_gol[with(total_gol, order(-total)), ]

#-----------------------------------------------------------------------------#

# 2 Top 05 selecciones más goleadoras
fav_teams <- c("Brazil", "Argentina", "France", "Spain", "England")
total_top_5 <- total_gol %>%
  filter(`Team` %in% fav_teams) %>%
  select(Team, Home, Away, total)

#-----------------------------------------------------------------------------#

# 3 Filtro de datos "Promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav <- total_gol %>% 
  filter(`Team` %in% fav_teams) %>%
  select(Team, Home, Away, total)

#conteo de todos los partidos jugados de las seleciones favoritas
home_matches <- world_cup_matches %>%
  select(Year, `Home Team`, `Home Goals`) %>%
  rename(Team = `Home Team`, Goals = `Home Goals`)
away_matches <- world_cup_matches %>%
  select(Year, `Away Team`, `Away Goals`) %>%
  rename(Team = `Away Team`, Goals = `Away Goals`)
all_matches <- rbind(home_matches, away_matches) %>% 
  filter(`Team` %in% fav_teams) %>%
  select(Team) %>%
  add_count(`Team`, name = "Matches") %>%
  unique()
top_fav <- left_join(top_fav, all_matches, by = "Team") #union de tablas
top_fav$avg <- top_fav$total/top_fav$Matches #calculo de promedio de goles


#-----------------------------------------------------------------------------#

# 4 ranking FIFA de como inician al mundial las 32 selecciones

r_fifa22 <- X2022_world_cup_groups
r_fifa22 <- r_fifa22[with(r_fifa22, order(r_fifa22$`Group`)), ] 

#-----------------------------------------------------------------------------#
# 5 Resultados de historicos de juegos en la historia del FIFA World Cup
#Funcion comun para limpiar la data
merge_matches <- function(df, f, cols) { # a traves de la funcion se obtiene los partidos ganados, perdidos y empatados
  temp <- df
  temp_cols <- c("Team", "Matches") 
  
  temp1 <- temp %>% 
    group_by(`Home Team`) %>%
    summarise(`Home Goals` = sum(f(`Home Goals`, `Away Goals`)), )
  colnames(temp1) <- temp_cols
  
  temp2 <- temp %>% 
    group_by(`Away Team`) %>%
    summarise(`Away Goals` = sum(f(`Away Goals`, `Home Goals`)), )
  colnames(temp2) <- temp_cols
  
  total <- merge(temp1, temp2, by = "Team", all = TRUE)
  total$Matches <- total$Matches.x + total$Matches.y
  total <- total[with(total, order(-Matches)), ]
  total <- select(total, Team, Matches)
  colnames(total) <- cols
  total
}

#  filtro cantidad de partidos ganados por seleccion e instancias que llegaron
total_wins <- merge_matches(world_cup_matches, `>`, c("Team", "Wins"))

#filtro cantidad de partidos empatados por seleccion e instancias que llegaron
total_ties <- merge_matches(world_cup_matches, `==`, c("Team", "Ties"))

#filtro cantidad de partidos perdidos por seleccion e instancias que llegaron
total_loss <- merge_matches(world_cup_matches, `==`, c("Team", "Loss"))

#Combinacion de tablas
all_matchs <- left_join(total_wins, total_ties, by = "Team") %>%
  left_join(total_loss, by = "Team")

#Resultados de historicos de juegos en la historia del FIFA World Cup
all_WC <- select(all_matchs, Team, Wins, Ties, Loss)

#----------------------------------------------------------------------------------------

# 6 Resultados de historicos de juegos en la historia de los 32 equipos participantes 
#en Qatar 2022

q_2022 <- all_WC %>% #filtro de las selecciones
  filter(`Team` %in% c("Qatar", "Germany", "Denmark", "Brazil", "France",
                       "Belgium", "Croatia", "Spain", "Serbia", "England", "Switzerland", "Netherlands", 
                       "Argentina", "Iran", "South Korea", "Japan", "Saudi Arabia", "Ecuador",
                       "Uruguay", "Canada","Ghana", "Senegal", "Portugal", "Poland", "Tunisia", 
                       "Morocco", "Cameroon","Mexico", "United States", "Wales", "Australia", 
                       "Costa Rica")) %>%
  select(Team, Wins, Ties, Loss)

#Anexion de la seleccion de Qatar dado a que es su primera participacion en este torneo

q_2022 <- q_2022 %>%
  rows_upsert(data.frame(Team = "Qatar", Wins = 0, Ties = 0, Loss = 0))

#----------------------------------------------------------------------------------------

# 7 comparacion entre los candidatos fuertes dado a la cantidad de goles y de partidos ganados

top_4 <- total_wins$Team[1:4] #en este caso se obtienen a los 4 candidatos fuertes
home_matches <- world_cup_matches %>%
  select(Year, `Home Team`, `Home Goals`) %>% #luego se obtienen los goles obtenidos segun su condicion
  rename(Team = `Home Team`, Goals = `Home Goals`)
away_matches <- world_cup_matches %>%
  select(Year, `Away Team`, `Away Goals`) %>%
  rename(Team = `Away Team`, Goals = `Away Goals`)
all_matches <- rbind(home_matches, away_matches)

#----------------------------------------------------------------------------------------
# 8 ultimas posiciones logradas de esos candidatos en mundiales anteriores
top_4 <- total_wins$Team[1:4]
home_matches_by_stage <- world_cup_matches %>%
  select(Year, `Home Team`, `Stage`) %>%
  rename(Team = `Home Team`)
away_matches_by_stage <- world_cup_matches %>%
  select(Year, `Away Team`, `Stage`) %>%
  rename(Team = `Away Team`)
all_matches_by_stage <- rbind(home_matches_by_stage, away_matches_by_stage)

stages <- c("First group stage",
            "Second group stage",
            "First round",
            "Final round",
            "Group stage",
            "Round of 16",
            "Quarter-finals",
            "Semi-finals",
            "Third place",
            "Final"
            )
#----------------------------------------------------------------------------------------
# 9 ganadores de la copa
#lista de paises que participaron en las finales
final_rounds <- world_cup_matches %>% 
  filter(`Stage` == "Final round") %>%
  select(`Year`, `Home Team`, `Away Team`) %>%
  rename(Team = `Home Team`, Against = `Away Team`)
final_round <- tail(final_rounds, n = 1) 

finals <- world_cup_matches %>%
  filter(`Stage` == "Final") 
finals_home <- finals %>%
  filter(`Home Goals` > `Away Goals` | `Home Goals` == `Away Goals`) %>%
  select(`Year`, `Home Team`, `Away Team`) %>%
  rename(Team = `Home Team`, Against = `Away Team`)
finals_away <- finals %>%
  filter(`Away Goals` > `Home Goals`) %>%
  select(`Year`, `Home Team`, `Away Team`) %>%
  rename(Team = `Away Team`, Against = `Home Team`)
all_finals <- rbind(finals_home, finals_away, final_round) %>%
  add_count(`Team`, name = "Times")
#----------------------------------------------------------------------------------------
# 10 promedio de edad de las selecciones
# Extraccion de plantillas
wcup_age <- X2022_world_cup_squads[c("Team", "Position", "Player", "Age")]

age_cup <- wcup_age %>%
  group_by(`Team`) %>%
  summarize(mean(Age))

colnames(age_cup) <- c("Team", "Age")

#----------------------------------------------------------------------------------------
# 11 Ligas y Clubs con mayor influencia

fc_league <- X2022_world_cup_squads
fc_league <- fc_league %>% # creacion de tabla de frecuencia variable cualitativa
  group_by(League, Club) %>% 
  summarise(frequency = n())

fc_league <- fc_league[with(fc_league, order(-frequency)), ] #ajuste de orden descendiente

# ------------- RESULTADOS POST MUNDIAL ---------------------------------

# 13 TABLA DE JUEGOS E INSTANCIAS QUE LLEGARON # ALFRED

#----------------------------------------------------------------------------------------
# 14 ¿Quienes fueron los Goleadores del torneo? - ¿Quien gano el botin de oro?
player_Goals <- na.omit(player_Goals)

head(player_Goals)

#----------------------------------------------------------------------------------------
# 15 Promedio de goles por partido
mean_goals <- rowMeans(player_Goals[, c(2,3)], na.rm = TRUE )
head(mean_goals) # revisar

#----------------------------------------------------------------------------------------
# 16 ¿Quienes dieron mas asistencia a gol en el torneo?
head(player_Assists)

#----------------------------------------------------------------------------------------
# 17 Equipos con mayor numero de asistencias
head(team_Assists)

#----------------------------------------------------------------------------------------
# 18 ¿Cuantos penales fueron concebidos a lo largo de todo el torneo?
team_Penalties_Scored #hacer la suma de todos los penales cobrados y marcados

#----------------------------------------------------------------------------------------
# 19 ¿Cuales fueron las selecciones con mayor cantidad de penales a favor durante todo el torneo?
wcup_penaltis_s <- team_Penalties_Scored %>%
  select(`Country`, `Rank`, `Games Played`, `Penalties Scored`)

#----------------------------------------------------------------------------------------
# 20 ¿Cuales fueron las selecciones con mayor cantidad de penales en contra durante todo el torneo?
wcup_penaltis_a <- team_Penalties_Scored %>%
  select(`Country`, `Rank`, `Games Played`, `Goal Against`)

# ------------- ¿Que paso en la Final? ---------------------------------

# 21 Mapa de tiros y pases
map_arg <- the_final %>%
  filter(Team %in% c("ARG"))

map_fra <- the_final %>%
  filter(Team %in% c("FRA"))

#----------------------------------------------------------------------------------------
# 22 ¿Cuantos penales fueron concebidos a los finalistas en todo el torneo?
penaltis_final_s <- team_Penalties_Scored %>%
  filter(Country %in% c("Argentina", "France")) %>%
  select(`Country`, `Rank`, `Games Played`, `Penalties Scored`)

#----------------------------------------------------------------------------------------
# 23 ¿Cuantos penales fueron en contra a los finalistas en todo el torneo?
penaltis_final_a <- team_Penalties_Scored %>%
  filter(Country %in% c("Argentina", "France")) %>%
  select(`Country`, `Rank`, `Games Played`, `Goal Against`)

#----------------------------------------------------------------------------------------
# 24 ¿Cuantas tarjetas amarillas y rojas hubo a lo largo del partido para Argentina?
wcup_cards_home <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Yellow_cards, Home_team_red_cards)

# 25 ¿Cuantas tarjetas amarillas y rojas hubo a lo largo del partido para Francia?
wcup_cards_away <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Away_team_Yellow_cards, Away_team_red_cards)

# 26 Faltas cometidas
fouls <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Fouls, Away_team_Fouls)
#----------------------------------------------------------------------------------------
# 27 Efectividad de los pases
pass <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_shots, Home_team_pass_accuracy, Away_team_shots, Away_team_pass_accuracy)

# 28 Posesion de la pelota
possession <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Possession, Away_team_Possession)

# 29 Tiros al arco / tiros de esquina
shots <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_shots, Away_team_shots, `Home_team_(Shots_on_target)`, `Away_team_(Shots_on_target)`)

corners <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Corners, Away_team_Corners)

# 30 Cantidad de fuera de juegos (Offside)
offside <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_offsite, Away_team_offsite)
#----------------------------------------------------------------------------------------
# 31 Resultado final

# Resultado ronda de penales
p_final_wcup <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Penalties, Away_team_Penalties)

# Resultado entre los 90 min y la prorroga
g_final_wcup <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Home_team_Goals, Away_team_Goals)

#----------------------------------------------------------------------------------------
#32 Caracteristicas del Estadio

stadium_final_wcup <- FIFA_WORLD_CUP_2022 %>%
  filter(Mach_type %in% c("Final")) %>%
  select(Stadium, winner_Team)
# agregar cantidad de aficionados

#comparacion con otros partidos y sacar:
#cual fue el partido con mas aficionados



# 21 Cuales fueron los estadios con mas asistencia?


#----------- Parte II Visualizacion de los datos "Data Cleaning" -------------#

#----------------------------------------------------------------------------------------
# Tabla 1 Tabla "Total de goles en todas las selecciones en el torneo FIFA World Cup 
# desde el primer torneo oficial en Uruguay 1930"

reactable(
  total_gol,
  defaultSorted = "total",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(total_gol, text_position = "outside-base")
  )
)

# GRAFICA 2 Top 5 selecciones más goleadoras

ggplot(total_top_5, aes(x = reorder(Team, -total), y = total)) +
  geom_segment(aes(x = reorder(Team, -total),
                   xend = reorder(Team, -total),
                   y = 0, yend = total),
               color = "gray", lwd = 1) +
  geom_point(size = 7.5, pch = 21, bg = 4, col = 1) +
  geom_text(aes(label = total), color = "white", size = 3) +
  xlab("Team") +
  ylab("") +
  coord_flip() +
  theme_minimal() + #tema del grafico
  shadow_mark() + #sombra de la animacion
  enter_grow() + #animacion de crecimiento
  transition_states(total, wrap = FALSE) + #transicion de la animacion
  labs(title = "Top 05 de selecciones más goleadoras", #titulo del grafico
       subtitle = "Qatar 2022", #subtitulo del grafico
       caption = "Fuente: Archivo de la Copa Mundial de la FIFA y RSSSF",
       tag = "Figura 1",
       x = "Selecciones", #etiqueta del eje x
       y = "Cantidad de Goles", #etiqueta del eje y
  )

#----------------------------------------------------------------------------------------
# Tabla 3 Promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

reactable(
  top_fav,
  defaultSorted = "avg",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(top_fav, text_position = "outside-base")
  )
)

# GRAFICA 4 promedio de goles anotados en todas sus participaciones en
#el torneo dentro de los cinco (5) equipos favoritos para participar a 
#la final de Qatar 2022 segun la casa de apuesta betfair"

top_fav %>% 
  ggplot(aes(x = `Team`, y = `avg`, fill = `Team`)) +
  geom_col() +
  scale_fill_manual(values = c("cadetblue1", "seagreen3", "seashell1", "steelblue", "tomato2"), name = "Top 5") +
  geom_text(aes(label = round(avg, digits = 2)), vjust = -1) +
  xlab("Team") +
  ylab("") +
  theme_minimal() +
  labs(title = "Promedio de goles anotado en todas sus participaciones en el",
       subtitle = "torneo dentro de los 05 equipos favoritos a participar a la final de Qatar 2022",
       caption = "Fuente: Archivo de la Copa Mundial de la FIFA y RSSSF",
       tag = "Figura 2",
       x = "Selecciones",
       y = "Promedio de Goles",
  )

#----------------------------------------------------------------------------------------
# Tabla 5 ranking FIFA de como inician al mundial las 32 selecciones

reactable(
  r_fifa22,
  defaultSorted = "FIFA Ranking",
  defaultSortOrder = "asc",
  defaultColDef = colDef(
    cell = data_bars(r_fifa22, text_position = "outside-base")
  )
)


# Grafica 6 ranking FIFA de como inician al mundial las 32 selecciones
ggplot(r_fifa22, aes(y = Team, x = `FIFA Ranking`, color = `Group`, fill = `Group`)) +
  geom_bar(stat = 'identity', width = 0.5, alpha = 0.25) +
  geom_text(aes(label = `FIFA Ranking`), hjust = -1) +
  theme_minimal() +
  facet_wrap(~Group, scales = "free_y") +
  labs(title = "Ranking FIFA de las selecciones que disputaran en",
       subtitle = "Qatar 2022",
       caption = "Fuente: Archivo de la Copa Mundial de la FIFA y RSSSF",
       tag = "Figura 3",
       y = "Selecciones",
       x = "Posicion FIFA Ranking"
  )

#-----------------------------------------------------------------------------#
# Tabla 7 Tabla de Resultados de historicos de juegos en la historia del FIFA World Cup

reactable(
  all_WC,
  defaultSorted = "Wins",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(all_WC, text_position = "outside-base")
  )
)

#-----------------------------------------------------------------------------#
# Tabla 8 Resultados de historicos de juegos en la historia de los 32 equipos participantes 
#en Qatar 2022
reactable(
  q_2022,
  defaultSorted = "Wins",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(q_2022, text_position = "outside-base")
  )
)

# Grafica 9 comparacion entre los candidatos fuertes dado a la cantidad de goles y de partidos ganados

all_matches %>%
  filter(`Team` %in% top_4) %>%
  group_by(`Team`, `Year`) %>%
  summarise(`Goals` = sum(Goals)) %>%
  ggplot(aes(x = factor(Year), y = Goals, color = Team, fill = Team)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.15) +
  geom_text(aes(label = Goals), vjust = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Comparacion entre los 4 principales participantes", "Por pais") +
  facet_wrap(~ Team)

#----------------------------------------------------------------------------------------
# Grafica 10 posiciones logradas de esos candidatos en mundiales anteriores

all_matches_by_stage %>%
  filter(`Team` %in% top_4) %>%
  group_by(`Team`, `Year`, `Stage`) %>%
  distinct() %>%
  ggplot(aes(x = factor(Stage, levels = stages), color = Team, fill = Team)) +
  geom_bar(width = 0.5, alpha = 0.15) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Comparacion entre 4 principales participantes", "Por pais") +
  facet_wrap(~ Team)

#----------------------------------------------------------------------------------------
# Grafica 11 ganadores de la copa
all_finals %>%
  select(`Team`, `Times`) %>%
  unique() %>%
  arrange(Times) %>%
  ggplot(aes(x = reorder(Team, -Times), y = Times, color = Team, fill = Team, reorder(Team, Times))) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.15) +
  geom_text(aes(label = Times), vjust = -1) +
  theme_minimal() +
  ggtitle("Copas ganadas", "Por pais")

#----------------------------------------------------------------------------------------
# Tabla 12 Edad de los jugadores en sus selecciones


# Grafica 13 Promedio de edad de las selecciones

ggplot(age_cup, aes(x = `Team`, group = 1)) + 
  geom_line(aes(y = Age)) + 
  labs(title = "Promedio de edad de las selecciones", 
       caption = "Fuente: Economics", 
       y = "Edad Promedio",
       x = "Selecciones") +  # title and caption
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid

# Tabla 14 Ligas y Clubs con mayor influencia



# ------------- RESULTADOS POST MUNDIAL ---------------------------------

# Tabla 15 ¿Quienes fueron los Goleadores del torneo?
reactable(
  player_Goals,
  defaultSorted = "Goals",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(player_Goals, text_position = "outside-base")
  )
)
#----------------------------------------------------------------------------------------
# Tabla 16 ¿Quienes dieron mas asistencia a gol en el torneo?
reactable(
  player_Assists,
  defaultSorted = "Games Played",
  defaultSortOrder = "desc",
  defaultColDef = colDef(
    cell = data_bars(player_Assists, text_position = "outside-base")
  )
) #Revisar posiciones



# 13 TABLA DE JUEGOS E INSTANCIAS QUE LLEGARON # ALFRED

#----------------------------------------------------------------------------------------
# 15 Promedio de goles por partido
#----------------------------------------------------------------------------------------
# 17 Equipos con mayor numero de asistencias

#----------------------------------------------------------------------------------------
# 18 ¿Cuantos penales fueron concebidos a lo largo de todo el torneo?
#----------------------------------------------------------------------------------------
# 19 ¿Cuales fueron las selecciones con mayor cantidad de penales a favor durante todo el torneo?

#----------------------------------------------------------------------------------------
# 20 ¿Cuales fueron las selecciones con mayor cantidad de penales en contra durante todo el torneo?

# ------------- ¿Que paso en la Final? ---------------------------------

# Grafica de tiros y pases de la seleccion Argentina
ggplot(map_arg) +
  annotate_pitch(fill = "#1b893e", colour = "white") +
  geom_segment(aes(x = X, y = Y, xend = X2, yend = Y2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#186d33"))

# Grafica de tiros y pases de la seleccion Francesa
ggplot(map_fra) +
  annotate_pitch(fill = "#1b893e", colour = "white") +
  geom_segment(aes(x = X, y = Y, xend = X2, yend = Y2),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#186d33"))
#----------------------------------------------------------------------------------------
# 22 ¿Cuantos penales fueron concebidos a los finalistas en todo el torneo?
#----------------------------------------------------------------------------------------
# 23 ¿Cuantos penales fueron en contra a los finalistas en todo el torneo?
#----------------------------------------------------------------------------------------
# 24 ¿Cuantas tarjetas amarillas y rojas hubo a lo largo del partido para Argentina?

# 25 ¿Cuantas tarjetas amarillas y rojas hubo a lo largo del partido para Francia?

# 26 Faltas cometidas
#----------------------------------------------------------------------------------------
# 27 Efectividad de los pases

# 28 Posesion de la pelota

# 29 Tiros al arco / tiros de esquina

# 30 Cantidad de fuera de juegos (Offside)

#----------------------------------------------------------------------------------------
# 31 Resultado final

# Resultado ronda de penales

# Resultado entre los 90 min y la prorroga
#----------------------------------------------------------------------------------------
#32 Caracteristicas del Estadio

#comparacion con otros partidos y sacar:
#cual fue el partido con mas aficionados



# 21 Cuales fueron los estadios con mas asistencia?

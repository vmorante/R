#Con el data frame mtcars (viene cargado en R).

library(ggplot2)
library(dplyr)
#Previsualizar el contenido con la funcion head()
head(mtcars)
#Mirar el numero de las y columnas con nrow() y ncol().
nrow(mtcars)
ncol(mtcars)
#3 Crear un nuevo data frame con los modelos de coche que
#consumen menos de 15 millas/galon.
bajo_consumo<-filter(mtcars,mpg<15)
#4 Ordenar el data frame anterior por disp.
arrange(bajo_consumo,desc(disp))
#5 Calcular la media de las marchas (gear) de los modelos del
#data frame anterior.
summarise(bajo_consumo,media=mean(gear,na.rm=TRUE))

#si lo agrupamos
mtcars %>% 
filter(mpg<15)%>%
arrange(desc(disp)) %>%
summarize(avg_gear = mean(gear))
#6 Hacer una graca del peso (wt) con respecto al consumo (mpg).
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()


#2 parte Ejercicio Lahman
#ver descripcion del dataframe
?Lahman::Batting
#media partidos
batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
    group_by(playerID) %>% 
    summarise(
       media =mean(G, na.rm = TRUE),
      
   )

#total_partidos_ordenados
 batters <- batting %>% 
   group_by(playerID) %>% 
    
        summarize(total_partidos = sum(G)) %>%
        arrange(desc(total_partidos))

#mostrar el top 5    
 batters <- batting %>% 
   group_by(playerID) %>% 
    
        summarize(total_partidos = sum(G)) %>%
        arrange(desc(total_partidos)) %>%
        slice(0:5)
    

#3 parte Ejercicio nycflights13
library(nycflights13)
select(flights,hour,minute)

#Crear una nueva variable time a partir de las variables
#hour y minute
transmute(flights,
    time = hour  + 60/minute
)
flights_time<-mutate(flights,
   time = hour  + 60/minute
)

#Calcular el retraso medio a la llegada (arr delay) y el
#numero de vuelos para cada uno de los valores de la
#variable time.
summarise(flights, delay = mean(arr_delay, na.rm = TRUE))
f <- flights_time %>% 
    group_by(time) %>% 
    summarize(media =mean(arr_delay))
 
#time
f <- flights_time %>% 
    group_by(time) %>% 
    summarize(media =n())
#Simplificado
delay.per.time <-flights %>%
       mutate(time = hour  + 60/minute) %>%
       group_by(time) %>% 
       mutate(total =n()) %>%
       mutate(media =mean(arr_delay)) 
        
#Representar el retraso medio con respecto a la variable
#time, escalando ademas el tama~no de los puntos de
#acuerdo con el numero de vuelos
ggplot(data = delay.per.time, mapping = aes(x = time, y = media)) +
  geom_point(aes(size = total), alpha = 1/3) +
  geom_smooth(se = FALSE)


##Resumen
library(nycflights13)
library(dplyr)
library(ggplot2)


delay.per.time<-
  flights %>%
    mutate(time=hour + round(minute/60,1)) %>%
    group_by(time)%>%
    summarise(delay = mean(arr_delay,na.rm=T),
      n_vuelos = n())


ggplot(delay.per.time,aes(x=time,y=delay,size=n_vuelos))+
  geom_point()

##nflights2 data frame flights
library(nycflights13)
library(dplyr)
library(ggplot2)

#Calcular el retraso medio a la llegada (arr delay) y el
#numero de vuelos para cada uno de los destinos (variable
#dest).
flights_1<-
  flights %>%
  group_by(dest)%>%
  summarise(delay = mean(arr_delay,na.rm=T),
            n_vuelos = n())

#Hacer un merge del data.frame anterior con airports
flights_u<-left_join(flights_1, airports,by=c("dest"="faa"))
#Representar la latitud con respecto a la longitud, escalando
#ademas el tama~no de los puntos de acuerdo con el numero
#de vuelos.
ggplot(flights_u,aes(x=lat,y=lon,size=n_vuelos))+
  geom_point()

##nba
library(readr)
#Cargar los datos
nba.shots <- read_csv("shot_logs(1).csv")

#comparar las estadsticas de tiro de dos jugadores,
#por ejemplo Kobe Bryant y James Harden
nba.shots %>%  
  filter(player_name == "kobe bryant" | player_name == "james harden" )

#acumula el tiempo total que cada jugador ha tocado el balon antes de cada tiro
(#TOUCH TIME) en cada partido
nba_2<-
  nba.shots %>% 
  group_by(GAME_ID,player_id)%>%
  summarise(total_touch_time = last(cumsum(TOUCH_TIME)))
  nrow(nba_2)
#calculando
#una nueva variable, points per tt dividiendo la suma
#total de puntos de cada jugador/partido (variable PTS) por
#el tiempo total que ha tocado la pelota (maximo de la
#variable anterior, total touch time
nba_2<-
  nba.shots %>%
  group_by(GAME_ID,player_id)%>%
  summarise(total_touch_time = last(cumsum(TOUCH_TIME)),PTS=sum(PTS_TYPE),points_per_tt=PTS/total_touch_time)
#calculando la media de
#la variable points per tt para todos los partido
nba_2<-
  nba.shots %>%
  group_by(player_id)%>%
  summarise(total_touch_time = last(cumsum(TOUCH_TIME)),PTS=sum(PTS_TYPE),points_per_tt=mean(PTS/total_touch_time))
#histograma de la distancia de tiro (SHOT DIST)
library(ggplot2)

ggplot(data = nba.shots) + 
  geom_bar(mapping = aes(x = SHOT_DIST, fill = PTS_TYPE), position = "dodge")
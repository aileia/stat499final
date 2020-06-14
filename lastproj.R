library(readr)
library(magrittr)
library(dplyr)
library(tidyr) 
library(lubridate)
library(ggplot2)
met000 <- read_csv("meteoroloji_000.csv")
library(leaflet)
leaflet()
summary(met000) 
met000%>%
  mutate(dayobs=day(OBSERVING_DATE))%>%
  group_by(dayobs)%>% 
  summarize(n_obs = n(),
            maxtemp = max(TEMPERATURE),
            mintemp = min(TEMPERATURE),
            avetemp=mean(TEMPERATURE))%>%
gather(key = key, value = temp,-n_obs,-dayobs)%>%
  ggplot(aes(x = dayobs, y = temp,color=key )) + 
  geom_point() + geom_line()+geom_smooth()

met000%>%
  group_by(OBSERVING_DATE)%>%
  summarize(n_obs = n(),
            avetemp = mean(TEMPERATURE),
            ave_humidity = mean(HUMIDITY))%>%
  ggplot(aes(x = OBSERVING_DATE, y = avetemp )) + 
  geom_point() + geom_line()+geom_smooth()
met000%>%
  group_by(OBSERVING_DATE)%>%
  summarize(n_obs = n(),
            avetemp = mean(TEMPERATURE),
            ave_humidity = mean(HUMIDITY))%>%
  ggplot(aes(x = OBSERVING_DATE, y = ave_humidity )) + 
  geom_point() + geom_line()+geom_smooth()
metsum<-met000%>%
  group_by(WEATHER_STATION_ID)%>%
  summarize(n_obs = n(),
            windspeed = max(WIND_SPEED),
            longt= mean(LONGITUDE),
            latt=mean(LATITUDE))%>%
  mutate(popup_info = paste("<b>Windspeed:</b>", windspeed, "<br />"))
pal <- colorNumeric(palette = "Blues", domain = metsum$windspeed, reverse = F)



leaflet(data = metsum) %>%
  addTiles() %>%
  addCircleMarkers(
    lng =  ~ longt,
    lat =  ~ latt,
    radius = ~ windspeed ,
    popup =  ~ popup_info,
    color =  ~ pal(windspeed)
  )%>%
  addLegend("bottomright", pal = pal, values = ~windspeed,
            title = "Windspeed",
            opacity = 1
  )
leaflet() %>%
  addTiles() %>%
  addMarkers(data = metsum, 
             lng = ~ longt, lat = ~ latt,popup = ~ popup_info)
    
dolmer <- read_csv("istanbulkart-dolum-merkezi-bilgileri.csv")
leaflet() %>%
  addTiles() %>%
  addMarkers(data = dolmer, 
             lng = ~ LONGITUDE, lat = ~ LATITUDE,
             clusterOptions = markerClusterOptions())

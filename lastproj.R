#libraries required
library(readr)
library(magrittr)
library(dplyr)
library(tidyr) 
library(lubridate)
library(ggplot2)
library(leaflet)
library(plotly)
library(readxl)
#read meteorology data
met000_file <- paste0("https://data.ibb.gov.tr/dataset/824c942e-4e1b-41b6-8e33-394aae4a55e8/resource/8b8889bc-7d81-4708-9e4d-74e1342c7f8c/download/meteoroloji_000.csv")
met000 <- read_csv(met000_file)
met000 <- read_csv('met000.csv')
# summary info on meteorology data
summary(met000) 
# graph of maximum average and min temperatures for given day in march 2019
met000%>%
  mutate(dayobs=day(OBSERVING_DATE))%>%
  group_by(dayobs)%>%
  summarize(n_obs = n(),
            maxtemp = max(TEMPERATURE,na.rm=TRUE),
            mintemp = min(TEMPERATURE,na.rm=TRUE),
            avetemp=mean(TEMPERATURE,na.rm=TRUE))%>%
gather(key = key, value = temp,-n_obs,-dayobs)%>%
  ggplot(aes(x = dayobs, y = temp,color=key )) + 
  geom_point() + geom_line()+geom_smooth()
# graph of of average humidity in İstanbul, according to time
met000%>%
  group_by(OBSERVING_DATE)%>%
  summarize(n_obs = n(),
            avetemp = mean(TEMPERATURE),
            ave_humidity = mean(HUMIDITY))%>%
  ggplot(aes(x = OBSERVING_DATE, y = ave_humidity )) + 
  geom_point() + geom_line()+geom_smooth()
# map of max windspeed in March 2019
metsum<-met000%>%
  group_by(WEATHER_STATION_ID)%>%
  summarize(n_obs = n(),
            windspeed = max(WIND_SPEED),
            longt= mean(LONGITUDE),
            latt=mean(LATITUDE))%>%
  mutate(popup_info = paste("<b>Windspeed:</b>", windspeed, "<br />"))
pal <- colorNumeric(palette = "Blues", domain = metsum$windspeed, reverse = F)
metsum %>%
leaflet() %>%
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
head(dolmer)
  doalgaztuk <- read_excel('doalgaztuk.xlsx')
  f1 <- list(size = 8)
  doalgaztuk%>%
  gather(key = Yıl, value = Tüketim,-İlçe)%>%
  plot_ly(
    x = ~reorder(İlçe,Tüketim), 
    y = ~Tüketim,  
    frame = ~Yıl, 
    hoverinfo = "text",
    type = 'bar'
  )%>%
  layout(
    title = "İlçelerin yıllara göre doğalgaz tüketimleri",
    xaxis = list(title = "İlçeler",
                 tickfont = f1)
  )

 aksaraymet <- read_excel('otomatik-meteorolojik-gozlem-istasyon-verileri-akom (1).xlsx',3)
 aksaraymet$Ort_Hava_S<-as.double(aksaraymet$Ort_Hava_S)
 
translatemonth<- data.frame("Ay" = c("Ocak","Şubat","Mart","Nisan","Mayıs",
                                     "Haziran","Temmuz","Ağustos","Eylül",
                                     "Ekim","Kasım","Aralık"), 
                            "Month" = c("January","February","March","April",
                                        "May","June","July","August","September",
                                        "October","November","December"),
                            "Mevsim"=c("Kış","Kış","İlkbahar","İlkbahar","İlkbahar","Yaz","Yaz","Yaz","Sonbahar","Sonbahar","Sonbahar","Kış"))
aksaraymet
aksaraymet %>% left_join(translatemonth)%>%
  unite(datetime, `Verinin Yılı`, Month, Gün)%>%
  mutate(datetime = ymd(datetime))%>%
  plot_ly(
    x = ~datetime, 
    y = ~Ort_Hava_S,  
    hoverinfo = "text",
    type = 'scatter', mode = 'lines'
  )
aksaraymet %>% left_join(translatemonth)%>%
  rename(Yıl=`Verinin Yılı`)%>%
  group_by(Mevsim,Yıl)%>%
  summarize(OrtSıcaklık=mean(Ort_Hava_S,na.rm=TRUE))%>%
  filter(Mevsim=="Kış")%>%
  plot_ly(
    x = ~Yıl,
    y = ~OrtSıcaklık,  
    hoverinfo = "text",
    type = 'scatter', mode = 'lines'
  )

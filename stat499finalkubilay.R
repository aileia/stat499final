install.packages("readxl")
library("readxl")
library("ggplot2")
library(tidyverse)

library(gridExtra)
library("colorspace")

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1221 <- read_excel("mayis19.xlsx")
names(ff1221) <- c("date","kwh")
ff1221

mayis <- ggplot(ff1221, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
mayis <- mayis + ggtitle("2019 Mayıs Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
mayis
#tema degisikligi kismi
theme_set(theme_minimal())
mayis <- mayis +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

mayis  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1222 <- read_excel("nisan19.xlsx")
names(ff1222) <- c("date","kwh")
ff1222

nisan <- ggplot(ff1222, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
nisan <- nisan + ggtitle("2019 Nisan Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
nisan
#tema degisikligi kismi
theme_set(theme_minimal())
nisan <- nisan +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

nisan  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

ff1223 <- read_excel("mart19.xlsx")
names(ff1223) <- c("date","kwh")
ff1223

mart <- ggplot(ff1223, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
mart <- mart + ggtitle("2019 Mart Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
mart
#tema degisikligi kismi
theme_set(theme_minimal())
mart <- mart +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

mart  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1224 <- read_excel("subat19.xlsx")
names(ff1224) <- c("date","kwh")
ff1224

subat <- ggplot(ff1224, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
subat <- subat + ggtitle("2019 Şubat Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
subat
#tema degisikligi kismi
theme_set(theme_minimal())
subat <- subat +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

subat  


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1225 <- read_excel("aralik18.xlsx")
names(ff1225) <- c("date","kwh")
ff1225

aralik <- ggplot(ff1225, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
aralik <- aralik + ggtitle("2018 Aralık Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
aralik
#tema degisikligi kismi
theme_set(theme_minimal())
aralik <- aralik +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

aralik  


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1226 <- read_excel("kasim18.xlsx")
names(ff1226) <- c("date","kwh")
ff1226

kasim <- ggplot(ff1226, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
kasim <- kasim + ggtitle("2018 Kasım Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
kasim
#tema degisikligi kismi
theme_set(theme_minimal())
kasim <- kasim +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

kasim  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1227 <- read_excel("eylul18.xlsx")
names(ff1227) <- c("date","kwh")
ff1227

eylul <- ggplot(ff1227, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
eylul <- eylul + ggtitle("2018 Eylül Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
eylul
#tema degisikligi kismi
theme_set(theme_minimal())
eylul <- eylul +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

eylul  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1228 <- read_excel("agustos18.xlsx")
names(ff1228) <- c("date","kwh")
ff1228

agustos <- ggplot(ff1228, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
agustos <- agustos + ggtitle("2018 Ağustos Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
agustos
#tema degisikligi kismi
theme_set(theme_minimal())
agustos <- agustos +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

agustos  


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


ff1229 <- read_excel("haziran18.xlsx")
names(ff1229) <- c("date","kwh")
ff1229

haziran <- ggplot(ff1229, aes(x = date, y = kwh, color = kwh)) + geom_line() + geom_col() + scale_color_gradient2()
haziran <- haziran + ggtitle("2018 Haziran Ayı Anlık Enerji Üretimi") + xlab("Tarih") + ylab("Üretilen Enerji(kwh/dk)")
haziran
#tema degisikligi kismi
theme_set(theme_minimal())
haziran <- haziran +
  theme(
    axis.text.x = element_text(color = "black",size=11),
    axis.text.y = element_text(color = "black",size=11),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    axis.line = element_line(color = "black", size = 2),
    plot.title = element_text(lineheight = 8, face = "bold"),
    axis.title.x.bottom = element_text(colour = "black",size = 14),
    axis.title.y.left = element_text(color = "black", size ="14"),
    
  )

haziran  

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


grid.arrange(subat, mart, nisan, mayis, nrow= 4,ncol= 1)

grid.arrange(mayis, nisan, mart, subat, aralik, kasim, eylul, agustos, haziran, nrow= 3,ncol= 3)

summary(ff1221$kwh)
summary(ff1222$kwh)
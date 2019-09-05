library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(ggrepel)
library(lubridate)
library(scales)
Defectuosos<-read_csv2("Datos/Defectos.csv", 
                       col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Defecto", "Causa"), skip = 1, 
                       cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Defecto="f", Causa="f" ))%>%
  filter(Defecto !="NI")%>%
  filter(Causa!="NI")
  

ggplot(Defectuosos, aes(x=Defecto, fill=Causa))+
  geom_bar(position="fill")+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  labs(x=" ",y=" ", title = "Causas de los defectos",
       subtitle="Los problemas con la pantalla y los conos aparecen como las principales \n causas de rechupes y rebabas, respectivamente")



  
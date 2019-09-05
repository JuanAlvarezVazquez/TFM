#=======CARGA DE PAQUETES Y DATOS===============

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(ggrepel)

Inspeccion<-read_csv2("Datos/hist_inspec.csv", 
                      col_names = c("Horno","Fecha","Aleacion","Formato",
                                    "Colada","Lingotera","Id_Especialista","Especialista",
                                    "Id_Mando","Mando"), skip = 1, 
                      cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", 
                                Formato="f", Colada="i", Lingotera="i", Id_Especialista="f",
                                Especialista="f", Id_Mando="f", Mando="f"))%>%
  select(-"Id_Mando", - "Id_Especialista")

lingPorHorno<-inspeccion%>%
  distinct(Horno,Aleacion,Colada)%>%
  group_by(Horno,Aleacion,Colada)%>%
  summarise(coladas=n())

lingPorHorno2<-lingPorHorno%>%
  group_by(Horno)%>%
  summarise(n())


ggplot(lingPorHorno, aes(x=Horno, y =coladas, fill=Horno))+
  geom_bar(stat="identity")+
  theme_minimal()+
  labs(x=" ", y= "Coladas", title="Coladas inspeccionadas durante el periodo de pruebas")


#=============Cargamos librerias y archivo de inspeccion=================

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
registroLingotes<-read_csv2("Datos/hist_inspec.csv", col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Id_Especialista","Especialista","Id_Mando","Mando"), skip = 1, cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Id_Especialista="f", Especialista="f", Id_Mando="f", Mando="f"))

#============Calculo de los lingotes inspeccionados cada dia =============

lingotesInspec<-registroLingotes%>%
  filter(((Mando !="Averia")&(Mando !="No_Reg")&(Mando!="Prueba")))%>%
  filter(((Especialista !="Averia")&(Especialista !="No_Reg")&(Especialista!="Prueba")))

lingotesInspecSem<-lingotesInspec%>%  
  group_by(Fecha)%>%
  mutate(Semana=(week(Fecha)-10))%>%
  group_by(Semana)%>%
  summarise(Lingotes=n())

lingotesCoincSem<-lingotesInspec%>%
  filter(Mando==Especialista)%>%
  group_by(Fecha)%>%
  mutate(Semana=(week(Fecha)-10))%>%
  group_by(Semana)%>%
  summarise(Coincidencia=n())%>%
  mutate(Coincidencia=round(100*Coincidencia/lingotesInspecSem$Lingotes,2))

ggplot(lingotesCoincSem,mapping=aes(x=Semana, y=Coincidencia, label=Coincidencia))+
  geom_path()+
  geom_point()+
  geom_label_repel()+
  coord_cartesian(xlim = NULL, ylim = c(95,100))+
  scale_x_continuous(breaks = seq(1,9,1))+
  labs(title="Evolución semanal del porcentaje de coincidencia de criterio entre especialistas y mandos",
       subtitle = "El grado de coincidencia mantiene por encima del 99.8% desde la segunda semana de proyecto",
       caption="Porcentaje de coincidencia calculado sobre el total de lingotes registrados por semana",
       y="Lingotes(%)", x=" ")+
  theme_minimal()

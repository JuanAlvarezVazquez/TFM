#=============Cargamos librerias y archivo de inspeccion=================

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(scales)
registroLingotes<-read_csv2("Datos/hist_inspec.csv", col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Id_Especialista","Especialista","Id_Mando","Mando"), skip = 1, cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Id_Especialista="f", Especialista="f", Id_Mando="f", Mando="f"))

#============Calculo de los lingotes inspeccionados cada dia =============

lingotesInspec<-registroLingotes%>%
  filter(((Mando !="Averia")&(Mando !="No_Reg")&(Mando!="Prueba")))%>%
  filter(((Especialista !="Averia")&(Especialista !="No_Reg")&(Especialista!="Prueba")))

lingotesInspecDia<-lingotesInspec%>%  
  group_by(Fecha)%>%
  summarise(Lingotes=n())

lingotesCoincDia<-lingotesInspec%>%
  filter(Mando==Especialista)%>%
  group_by(Fecha)%>%
  summarise(Coincidencia=n())%>%
  mutate(Coincidencia=100*Coincidencia/lingotesInspecDia$Lingotes)

ggplot(lingotesCoincDia,aes(x=Fecha,y=Coincidencia))+
  geom_path()+
  theme_minimal()+
  annotate("text", x = as.Date("2019-04-02"), y = 97.5, label = "  El 25/03 Comienzan ha acopiarse los \n lingotes rechazados ", size=3.5)+
  coord_cartesian(xlim = NULL, ylim = c(95,100))+
  labs(title="Evolución del porcentaje de coincidencia de criterio entre especialistas y mandos",
       subtitle = "El grado de coincidencia mantiene por encima del 99.5% desde el 24 de marzo",
       caption="Porcentaje de coincidencia calculado sobre el total de lingotes registrados por día.",
       y="Lingotes(%)", x=" ")+
  scale_x_date(labels = date_format("%d/%m"), date_breaks = "week" )+ 
  geom_vline(aes(xintercept=as.Date("2019-03-25")),color="red",linetype=4)
  
 

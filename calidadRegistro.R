#============Evaluación de la calidad de datos del registro=============

#=============Cargamos librerias y archivo de inspeccion=================

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
registroLingotes<-read_csv2("Datos/hist_inspec.csv", 
col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Id_Especialista","Especialista","Id_Mando","Mando"), skip = 1, 
cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Id_Especialista="f", Especialista="f", Id_Mando="f", Mando="f"))

#============Cálculo de los lingotes inspeccionados cada dia =============

lingotesInspec<-registroLingotes%>%
  filter(((Mando !="Averia")&(Mando !="No_Reg")&(Mando!="Prueba")))%>%
  filter(((Especialista !="Averia")&(Especialista !="No_Reg")&(Especialista!="Prueba")))
  
lingotesInspecDia<-lingotesInspec%>%  
  group_by(Fecha)%>%
  summarise(Lingotes=n())

lingotesInspecSem<-lingotesInspec%>%  
  group_by(Fecha)%>%
  mutate(Semana=(week(Fecha)-10))%>%
  group_by(Semana)%>%
  summarise(Lingotes=n())

lingotesInspecDiaSem<-lingotesInspec%>%
  filter(Fecha>"2019-03-25")%>%
  mutate(diaSem=wday(Fecha))%>%
  group_by(diaSem)%>%
  summarise(Lingotes=n())


#============Cálculo del porcentaje de coincidencia=======================


lingotesCoincDia<-lingotesInspec%>%
  filter(Mando==Especialista)%>%
  group_by(Fecha)%>%
  summarise(Coincidencia=n())%>%
  mutate(Coincidencia=100*Coincidencia/lingotesInspecDia$Lingotes)
 
  
lingotesCoincSem<-lingotesInspec%>%
  filter(Mando==Especialista)%>%
  group_by(Fecha)%>%
  mutate(Semana=(week(Fecha)-10))%>%
  group_by(Semana)%>%
  summarise(Coincidencia=n())%>%
  mutate(Coincidencia=round(100*Coincidencia/lingotesInspecSem$Lingotes,2))



lingotesCoincDiaSem<-lingotesInspec%>%
  filter(Mando==Especialista)%>%
  filter(Fecha>"2019-03-25")%>%
  mutate(diaSem=wday(Fecha, label=TRUE))%>%
  group_by(diaSem)%>%
  summarise(Coincidencia=n())%>%
  mutate(Coincidencia=round(100*Coincidencia/lingotesInspecDiaSem$Lingotes,2))
  


ggplot(lingotesCoincDiaSem, mapping=aes(x=diaSem, y=Coincidencia,label=Coincidencia, group=1))+
  geom_line()+
  geom_point()+
  geom_label_repel()+
  coord_cartesian(xlim = NULL, ylim = c(95,100))+
  labs(title="Evoluci?n del porcentaje de coincidencia de criterio entre especialistas y mandos",
       subtitle = "(A partir de la segunda semana desde la puesta en marcha del registro el grado de coincidencia se ha mantenido por encima del 99.8%)",
       caption="Porcentaje de coincidencia calculado sobre el total de lingotes registrados por semana",
       y="Lingotes(%)")+
  theme_minimal()

ggplot(lingotesCoincSem,mapping=aes(x=Semana, y=Coincidencia, label=Coincidencia))+
  geom_path()+
  geom_point()+
  geom_label_repel()+
  coord_cartesian(xlim = NULL, ylim = c(95,100))+
  scale_x_continuous(breaks = seq(0,8,1))+
  labs(title="Evoluci?n del porcentaje de coincidencia de criterio entre especialistas y mandos",
       subtitle = "(A partir de la segunda semana desde la puesta en marcha del registro el grado de coincidencia se ha mantenido por encima del 99.8%)",
       caption="Porcentaje de coincidencia calculado sobre el total de lingotes registrados por semana",
       y="Lingotes(%)")+
  theme_minimal()
  


ggplot(lingotesCoincDia,aes(x=Fecha,y=Coincidencia))+
  geom_path()+
  ggtitle("Evolución del porcentaje de \n coincidencia diario") +
  xlab("Fecha") + ylab("% Lingotes")+
  theme_minimal()
  ggtitle("Evolución del porcentaje de \n coincidencia diario") +
  xlab("Fecha") + ylab("% Lingotes")+
  theme_minimal()

#================No registrados por mando o especialista========================

lingotesNoReg<-registroLingotes%>%
  filter((Mando=="No_Reg")|(Especialista=="No_Reg"))

calendario<-registroLingotes%>%
  select(Fecha)%>%
  distinct(Fecha)%>%
  mutate(Z=as.integer(0))

#==============Me doy cuenta de que coinciden con multiplos de colada y calculo por colada===========
 
lingotesNoregEspecialista<-lingotesNoReg%>%
  filter(Especialista=="No_Reg")%>%
  group_by(Fecha,Colada)%>%
  distinct(Colada)%>%
  group_by(Fecha)%>%
  summarize(Coladas=n())%>%
  full_join(calendario, lingotesNoregEspecialista, by="Fecha")%>% 
  mutate(Coladas = replace_na(Coladas, 0))%>%
  select(-Z)%>%
  arrange(Fecha)

lingotesNoregMando<-lingotesNoReg%>%
  filter(Mando=="No_Reg")%>%
  group_by(Fecha,Colada)%>%
  distinct(Colada)%>%
  group_by(Fecha)%>%
  summarize(Coladas=n())%>%
  full_join(calendario, lingotesNoregMando, by="Fecha")%>% 
  mutate(Coladas = replace_na(Coladas, 0))%>%
  select(-Z)%>%
  arrange(Fecha) 

ggplot()+
  geom_path(lingotesNoregEspecialista, mapping=aes(x=Fecha, y=Coladas, colour="red"))+
  geom_path(lingotesNoregMando, mapping=aes(x=Fecha, y=Coladas))


#====================Coladas no registradas===========================

coladasPorFormatoyAleacion2<-registroLingotes%>%
  distinct(Aleacion,Formato,Colada,Fecha)%>%
  arrange(Aleacion,Formato,Colada,Fecha)

Completitud<-coladasPorFormatoyAleacion2%>%
  group_by(Aleación,Formato,Fecha)%>%
  summarize(primeraCol=min(Colada),ultimaCol=max(Colada),coladasTotales=(ultimaCol-primeraCol+1), coladasRegistradas=n(), porcentajeDeReg=(coladasRegistradas/coladasTotales*100))

completitudDiaria<-Completitud%>%
  group_by(Fecha)%>%
  summarise(total=sum(coladasTotales), registradas=sum(coladasRegistradas), No_Registradas=(total-registradas))

ggplot()+
  geom_path(lingotesNoregEspecialista, mapping=aes(x=Fecha, y=Coladas, colour="red"))+
  geom_path(lingotesNoregMando, mapping=aes(x=Fecha, y=Coladas, colour="blue"))+
  geom_path(completitudDiaria, mapping=aes(x=Fecha, y=No_Registradas, colour="green"))+
  geom_path(completitudDiaria, mapping=aes(x=Fecha, y=total))


#====================Rechazos registro vs Excel========================



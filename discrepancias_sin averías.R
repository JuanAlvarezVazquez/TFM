#============Script para gráficos discrepancias contando el máximo de lingotes posibles=============

#=============Cargamos librerías y archivos=================

library(tidyverse)
library(dplyr)
todas_coladas<-read.csv2("O:/LABORAT/ENSAYOS/fyc/Datos/1.Inspección de producto terminado/hist_inspec.csv")[,1:10]
todas_coladas$Fecha<-as.Date(todas_coladas$Fecha,"%d-%m-%y")
#todas_coladas<-filter(todas_coladas,Fecha>"2019-03-24")
#============

registros_dia<-todas_coladas%>%
  filter((Mando!="Averia")&(Especialista!="Averia"))%>%
  group_by(Fecha)%>%
  summarise(total_diario=n())
#=============Creamos tibble para detectar discrepancias========

discrepancias<-todas_coladas%>%
  group_by(Fecha,Mando,Especialista)%>%
  summarise(lingotes=n() )%>%
  ungroup() %>%
  complete(Fecha, Mando, Especialista,
          fill = list(lingotes = 0))%>%
  filter(Mando!=Especialista)%>%
  filter(Mando!="No_Reg")%>%
  filter(Especialista!="No_Reg")%>%
  group_by(Fecha)%>%
  summarise(lingotes=sum(lingotes))%>%
  mutate(porcentaje_discrepancia=100*(lingotes)/registros_dia$total_diario)

coordenadas_media<-data.frame(x_coor=(max(discrepancias$Fecha)),y_coor=round(mean(discrepancias$porcentaje_discrepancia),2))

media<-coordenadas_media$y_coor[1]

ggplot(data=discrepancias, mapping=aes(x=Fecha, y=porcentaje_discrepancia), t)+
  geom_path()+
  geom_point()+
  ggtitle("Porcentaje de discrepancias \n (sin contar averías)") +
  xlab("Fecha") + ylab("%lingotes")+
  ylim(0,10)+
  geom_hline(yintercept=media)+
  geom_text(data=coordenadas_media,mapping=aes(x=(x_coor-6), y=(y_coor+0.05), label=paste("media = ",y_coor,"%",sep="")))







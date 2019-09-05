#=============Cargamos librerias y archivo de inspeccion=================

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
registroLingotes<-read_csv2("Datos/hist_inspec.csv", col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Id_Especialista","Especialista","Id_Mando","Mando"), skip = 1, cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Id_Especialista="f", Especialista="f", Id_Mando="f", Mando="f"))


#============Cambio theme_minimal para poner la leyenda debajo

theme_minimal <- function(base_size = 11, base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.background   = element_blank(),
      legend.position = "bottom",
      complete = TRUE
    )
}


#===========================

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
  mutate(No_reg = replace_na(Coladas, 0))%>%
  select(-Z,-Coladas)%>%
  mutate(Especialista="Especialista")%>%
  arrange(Fecha)

lingotesNoregMando<-lingotesNoReg%>%
  filter(Mando=="No_Reg")%>%
  group_by(Fecha,Colada)%>%
  distinct(Colada)%>%
  group_by(Fecha)%>%
  summarize(Coladas=n())%>%
  full_join(calendario, lingotesNoregMando, by="Fecha")%>% 
  mutate(No_Reg = replace_na(Coladas, 0))%>%
  select(-Z,-Coladas)%>%
  mutate(Mando="Mando")%>%
  arrange(Fecha)

coladasPorFormatoyAleacion2<-registroLingotes%>%
  distinct(Aleacion,Formato,Colada,Fecha)%>%
  arrange(Aleacion,Formato,Colada,Fecha)

Completitud<-coladasPorFormatoyAleacion2%>%
  group_by(Aleacion,Formato,Fecha)%>%
  summarize(primeraCol=min(Colada),ultimaCol=max(Colada),coladasTotales=(ultimaCol-primeraCol+1), coladasRegistradas=n(), porcentajeDeReg=(coladasRegistradas/coladasTotales*100))

completitudDiaria<-Completitud%>%
  group_by(Fecha)%>%
  summarise(total=sum(coladasTotales), registradas=sum(coladasRegistradas), No_Reg=(total-registradas),Ambos="Ambos")


CompResumen<-completitudDiaria%>%
  mutate(Mando=lingotesNoregMando$No_Reg, Especialista=lingotesNoregEspecialista$No_reg)%>%
  select(-Ambos)%>%
  mutate(PorcenMando=100*Mando/total, PorcenEsp=100*Especialista/total, 
         PorcenOmit=100*No_Reg/total,PorcenNoReg=100*(Mando+Especialista+No_Reg)/total )

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colSuma<-function(data) sapply(data, sum, na.rm = TRUE)


maximos<-colMax(filter(CompResumen, Fecha>"2019-03-25"))
sumas<-colSuma(select(filter(CompResumen, Fecha>"2019-03-25"),-Fecha))


SinRegistrar<-data.frame(Fecha=Fecha<-c(completitudDiaria$Fecha,completitudDiaria$Fecha,lingotesNoregMando$Fecha,lingotesNoregEspecialista$Fecha),
SinReg=SinReg<-c(completitudDiaria$registradas,completitudDiaria$No_Reg,lingotesNoregMando$No_Reg,lingotesNoregEspecialista$No_reg),
Tipo=Tipo<-c(rep("Registradas",60),completitudDiaria$Ambos,lingotesNoregMando$Mando,lingotesNoregEspecialista$Especialista))




ggplot(data=SinRegistrar, mapping=(aes(x=Fecha,fill=Tipo,y=SinReg )))+
  geom_bar(stat="identity")+
  scale_fill_discrete(name = " ", labels = c("No registrada", "Registrada por Mando(*)", "Registrada por Especialista(**)","Correctamente registrada"))+
  theme_minimal()+
  labs(title="Coladas, ¿se están registrando todas?",
       subtitle = "El porcentaje de coladas registradas desde el 25/03 supera el 98%. 
Los mandos registran más del 97% de las coladas, mientras que para los especialistas el porcentaje supera el 99%. ",
       y="Coladas", x="", caption="(*)Falta el registro del especialista.\n(**)Falta el registro del mando")+
  scale_x_date(labels = date_format("%d/%m"), date_breaks = "week" )
  

 



            
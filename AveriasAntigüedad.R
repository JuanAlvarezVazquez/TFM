#=============Cargamos librerias y archivo de inspeccion=================

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggrepel)
library(forcats)
Lingotes<-read_csv2("Datos/hist_inspec.csv", 
                    col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Id_Especialista","Especialista","Id_Mando","Mando"), skip = 1, 
                    cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", 
                              Id_Especialista="f", Especialista="f", Id_Mando="f", Mando="f"))%>%
  filter(Horno!="TR-15-1")


Lingoteras<-read_csv2("Datos/Lingoteras.csv", 
                      col_names = c("Horno","Fecha","Id_Lingotera","Lingotera","Usos"), skip = 1, 
                      cols_only(Horno="f", Fecha =col_date(format ="%m/%d/%Y"), Id_Lingotera="f", Lingotera="i", Usos="i"))



#======================Ranking Averías Mes=====================

coladas<-Lingotes%>%
  distinct(Horno,Lingotera,Colada,Fecha)%>%
  group_by(Horno,Lingotera)%>%
  summarise(Coladas=n())%>%
  mutate(HornoLingotera=as_factor(paste(Horno, Lingotera, sep=": L")))


Lingoteras2<-Lingoteras%>%
  mutate(HornoLingotera=as_factor(paste(Horno, Lingotera, sep=": L")))
  
  
Averias=Lingotes%>%
  filter(Mando=="Averia"& Especialista=="Averia")%>%
  mutate(Mes=(month(Fecha, label=TRUE)))%>%
  group_by(Horno, Lingotera)%>%
  summarise(Averia=n())%>%
  mutate(HornoLingotera=as_factor(paste(Horno, Lingotera, sep=": L")))%>%
  left_join(coladas,by="HornoLingotera")%>%
  mutate(ratioAveria=(100-(100*(Coladas-Averia)/Coladas)))%>%
  left_join(Lingoteras2,by="HornoLingotera")

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





ggplot(data=Averias, aes(x=Usos, fct_reorder(HornoLingotera,-Usos), color=ratioAveria))+
  geom_point()+
  geom_segment(aes(x = 0, y = HornoLingotera, xend = Usos, yend = HornoLingotera, color = ratioAveria), data = Averias)+
  #geom_segment(aes(xend=ratioAveria, yend=HornoLingotera))+
  scale_color_continuous(low="green", high="red")+
  labs(title="Influencia del ciclo de vida de la lingotera sobre las averías",
       subtitle= "No se aprecia una relación significativa entre el número de coladas producidas y
la frecuencia con la que se encuentra fuera de servicio por avería.",
       x= "Coladas", y="", color="Coladas fuera de servicio (%)")+
  theme_minimal()

ggplot(data=Averias, aes(x=Usos, y=ratioAveria, color=Horno))+
  geom_point()+
  labs(title="Influencia del ciclo de vida de la lingotera sobre las averías",
       subtitle= "No se aprecia una relación significativa entre el número de coladas producidas y
       la frecuencia con la que se encuentra fuera de servicio por avería.",
       x= "Coladas", y="", color="Coladas fuera de servicio (%)")+
  theme_minimal()

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(ggrepel)

Defectuosos<-read_csv2("Datos/Defectos.csv", 
                       col_names = c("Horno","Fecha","Aleacion","Formato","Colada","Lingotera","Defecto", "Causa"), skip = 1, 
                       cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleacion="f", Formato="f", Colada="i", Lingotera="i", Defecto="f", Causa="f" ))%>%
  mutate(contenAl=fct_recode(Aleacion,
                             "[0.3%-0.5%]" = "B1",
                             "[0.3%-0.5%]"  = "0.3-Al",
                             "[0.3%-0.5%]"  = "0.5-Al",
                             "(0.5%-0.8%]" ="C1",
                             "(0.5%-0.8%]" ="D1",
                             "(0.5%-0.8%]" ="0.7-Al",
                             ">0.8%" = "CGG-0.9-Al",
                             ">0.8%" = "ARC-1-Al",
                             ">0.8%" = "CGG-1-Al"
                             ))

LinDefec<-Defectuosos%>%
  filter(Horno!="TR-15-1")%>%
  group_by(contenAl)%>%
  summarize(total=n())%>%
  mutate(perc=100*total/sum(total))%>%
  arrange(perc)



LinDefecForm2<-Defectuosos%>%
  filter(Horno!="TR-15-1")%>%
  group_by(contenAl,Defecto)%>%
  filter(Defecto!="NI")%>%
  summarize(total=n())%>%
  mutate(perc=round(100*total/sum(total),0))%>%
  mutate(Defecto=fct_relevel(Defecto, "Grieta","Rebabas","Acabado","Rechupe",after= Inf ))

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


ggplot(LinDefecForm2, aes(x=contenAl, fill=Defecto, y=perc, label=paste(perc,c("%"),sep="")))+
  geom_bar(stat="identity")+
  geom_text(size = 4, position = position_stack(vjust = 0.5), color = "black")+
  theme_minimal()+
  labs(title="Distribución de los defectos según el Horno",
       subtitle= "Los rechupes son la principal causa de rechazo salvo en el formato Aceralia,
       en el que son superados por otros defectos de acabado y la aparación de grietas.",
       x= "", y="")



library(tidyverse)
library(dplyr)
library(lubridate)
registroLingotes<-read_csv2("Datos/Defectos.csv", col_names = c("Horno","Fecha","Aleación","Formato","Colada","Lingotera","Defecto","Causa"), skip = 1, cols_only(Horno="f", Fecha =col_date(format ="%d-%m-%y"), Aleación="f", Formato="f", Colada="i", Lingotera="i", Defecto="f", Causa="f"))

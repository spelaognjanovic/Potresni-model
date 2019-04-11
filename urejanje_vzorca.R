require(readr)
library(dplyr)
library(tidyr)

#Uvoz vzorca na katerem delamo
vzorec <- read_csv2("vzorec.csv", locale=locale(encoding="cp1250"))

#Uvoz kljucev za kodiranje
kljuci <-  read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))

#Dolocitev ranljivostnih razredov na podlagi materiala,
vzorec$Material <- gsub("opeka", "C",vzorec$Material)
vzorec$Material <- gsub("beton", "D",vzorec$Material)
vzorec$Material <- gsub("kamen", "B",vzorec$Material)
vzorec$Material <- gsub("les", "E",vzorec$Material)
vzorec$Material <- gsub("kombinacija", "C",vzorec$Material)
vzorec$Material <- gsub("kovina", "C",vzorec$Material)
vzorec$Material <- gsub("drugo", "D",vzorec$Material)
vzorec$Material <- gsub("drugo", "D",vzorec$Material)

#torej podatka o letu gradnje v tem primeru ne potrebujemo vec
vzorec <- vzorec[,-c(3)]

names(vzorec) <- c('Identifikator', 'Obcina', 'Ranljivostni_razred', 'Povrsina')

#Pogrupiramo po obèinah in ranlivostnih razreih 
tabela <- vzorec%>% drop_na()%>%group_by(Obcina, Ranljivostni_razred)%>%
  summarise(Povrsina = sum(Povrsina), Stevilo = n()) 

#pri obcinah mi ne dela crka C (v pravi kodi)
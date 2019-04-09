require(readr)
library(dplyr)

#Uvoz vzorca na katerem delamo
vzorec <- read_csv2("vzorec.csv", locale=locale(encoding="cp1250"))

#Uvoz kljucev za kodiranje
ranljivostni_razred <-  read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))

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
  
#Zelimo, da nam obcine pogrupira v regije in tako izvedeti kaksno stevilo katerega ranljivostnega razreda
#vsebuje posamezna. Povrsine naj se sestejejo. Torej bi se vsaka regija ponovila 5 krat!
#pri obcinah mi ne dela crka C (v pravi kodi)


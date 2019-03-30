require(readr)
library(dplyr)

#Uvoz vzorca na katerem delamo
vzorec <- read_csv2("vzorec.csv", locale=locale(encoding="cp1250"))

#Uvoz kljucev za kodiranje
ranljivostni_razred <-  read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))


#Iz stolpca Leto_gradnje in Materiali zelimo narediti nov stolpec Ranljivostni_razred!
#Ce to ne gre, bi potem dolocili ranljivostne razrede le po materialu!




#Zelimo obcine spraviti na regije in izvedeti kaksno stevilo katerega ranljivostnega razreda
#vsebuje posamezna. Kvadrature naj se sestejejo. Torej bi se vsaka regija ponovila 5x!


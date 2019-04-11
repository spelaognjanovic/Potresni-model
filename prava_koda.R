require(readr)
library(dplyr)
library(tidyr)

#Uvoz podatkov, ki vsebujejo stevilko stavbe in njegovo povrsino
kvadratura <- read_csv2("REN_SLO_delistavb_20181216.csv", locale=locale(encoding="cp1250"))
kvadratura <- kvadratura%>%group_by(STA_SID)%>%summarise(M_2 = sum(NETO_TLORIS_POV_DST))

#uvoz sifer katastrskih obcin, za prevedbo na obcine
sifre_ko <- read_csv2("sifre_ko.csv", locale=locale(encoding="cp1250"))

#poskus stetja obcin, jih je le 46 v bazi, ker recimo Drg spada pod SG
STEVILO<- sifre_ko%>%group_by(Obcina)%>%summarise(Stevilo = n())
stevilo2 <- STEVILO%>%summarise(Stevilo= sum(Stevilo))

#uvoz datoteke s podatki o stavbah (leto izgradnje, konstrukcija)
stavbe <- read_csv2("REN_SLO_stavbe_20181216.csv", locale=locale(encoding="cp1250"))
stavbe <- stavbe[,-c(3,4,5,7,8,9,10,11,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28)]
stavbe$ID_KONSTRUKCIJE <- gsub("1216", "C",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1217", "D",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1218", "B",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1219", "E",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1220", "C",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1221", "C",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1222", "D",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1223", "D",stavbe$ID_KONSTRUKCIJE)

#zanima me, ce je isto st. sifer za katasterske obcine kot v sifre:ko->torj 2696 (bi mogo bit rezultat)
#preizkusi
STEVILO_STAVBE <- stavbe%>%group_by(KO_SIFO)%>%summarise(Stevilo= n())
stevilo_stavbe <- STEVILO_STAVBE%>%summarise(Stevilo = sum(Stevilo))

#Prekodiraj sifre katastrskih obcin v obcine s pomocjo tabele sifre_ko
stavbe$Obcina=sifre_ko$Obcina[match(stavbe$KO_SIFKO,sifre_ko$KO_SIFKO)]
stavbe <- stavbe[, -c(2,3,4)]

#uvoz kljucev za dolocanjeranljivostnih razredov
#kljuc <- read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))

#nova tabela, dodamo se stolpec kvadratura, in jo uporabljamo v nadaljnje
stavbe <- merge(stavbe, kvadratura, by = 'STA_SID')

names(stavbe) <- c('Identifikator','Ranljivostni_razredi','Obcina', 'Povrsina')

#poskus tudi dobim 46 obcin 
poskus<- stavbe%>%group_by(Obcina)%>%summarise((Stevilo=n()))

#pogrupiranje po obcinah in ranlj razredih..problem ker ne sesteje povrsine? ali prevelika stevilka?
po_obcinah <- stavbe%>%group_by(Obcina, Ranljivostni_razredi)%>%
  summarise(Povrsina = sum(Povrsina), Stevilo = n())


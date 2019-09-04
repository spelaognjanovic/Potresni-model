require(readr)
library(dplyr)
library(tidyr)
library(knitr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(tidyr)


#Uvoz podatkov, ki vsebujejo stevilko stavbe in njegovo povrsino
kvadratura <- read_csv2("REN_SLO_delistavb_20181216.csv", locale=locale(encoding="cp1250"))
kvadratura <- kvadratura%>%group_by(STA_SID)%>%summarise(M_2 = sum(NETO_TLORIS_POV_DST))
kvadratura <- kvadratura%>%drop_na() #nic se ni zgodilo
#rada bi se prestela st. stavb, ki imajo kvadraturo 0 -->pomanjkljivosti

#uvoz sifer katastrskih obcin, za prevedbo na obcine DIREK IZ LINKA
link <- "http://cen.gov.si/JavniVpogled/help/KO.htm"
stran <- html_session(link) %>% read_html()

katastrske <- stran %>% html_nodes(xpath="//table[@class='LIST']") %>%
  .[[1]] %>% html_table()

katastrske <- katastrske[-c(1),-c(2)]
names(katastrske) <- c('KO_SIFKO', 'Obcina')

#vidimo, da je stevilo obcin 46
#lo <- katastrske%>%group_by(Obcina)%>%summarise(Stevilo = n())

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

#Prekodiraj sifre katastrskih obcin v obcine s pomocjo tabele sifre_ko
stavbe$Obcina=katastrske$Obcina[match(stavbe$KO_SIFKO,katastrske$KO_SIFKO)]
stavbe <- stavbe[, -c(2,3,4)]%>%drop_na()

#uvoz kljucev za dolocanjeranljivostnih razredov
kljuc <- read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))

#nova tabela, dodamo se stolpec kvadratura, in jo uporabljamo v nadaljnje
stavbe <- merge(stavbe, kvadratura, by = 'STA_SID')

names(stavbe) <- c('Identifikator','Ranljivostni_razredi','Obcina', 'Povrsina')

stavbe<- stavbe%>%drop_na() #se enx za ziher XD

#pogrupiranje po obcinah in ranlj razredih in DELUJE!
po_obcinah <- stavbe%>%group_by(Obcina, Ranljivostni_razredi)%>%
  summarise(Povrsina = sum(Povrsina), Stevilo = n())

write_csv(po_obcinah, "obcine.csv", na="")

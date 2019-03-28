require(readr)
library(dplyr)

#Uvoz podatkov, ki vsebujejo stevilko stavbe in njegovo povrsino
kvadratura <- read_csv2("REN_SLO_delistavb_20181216.csv", locale=locale(encoding="cp1250"))
kvadratura <- kvadratura%>%group_by(STA_SID)%>%summarise(M_2 = sum(NETO_TLORIS_POV_DST))

#uvoz sifer katastrskih obcin, za prevedbo na obcine
sifre_ko <- read_csv2("sifre_ko.csv", locale=locale(encoding="cp1250"))

#uvoz datoteke s podatki o stavbah (leto izgradnje, konstrukcija)
stavbe <- read_csv2("REN_SLO_stavbe_20181216.csv", locale=locale(encoding="cp1250"))
stavbe <- stavbe[,-c(3,4,5,7,8,9,10,11,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28)]
stavbe$ID_KONSTRUKCIJE <- gsub("1216", "opeka",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1217", "beton",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1218", "kamen",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1219", "les",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1220", "kombinacija",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1221", "kovina",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1222", "drugo",stavbe$ID_KONSTRUKCIJE)
stavbe$ID_KONSTRUKCIJE <- gsub("1223", "drugo",stavbe$ID_KONSTRUKCIJE)

stavbe[["KO_SIFKO"]] <- sifre_ko[ match(stavbe[['KO_SIFKO']], sifre_ko[['KO_SIFKO']] ), "Obcina" ]

#uvoz kljucev
kljuci <- read_csv2("kljuci.csv", locale=locale(encoding="cp1250"))

#nova tabela s podatki o obcinah, ranljivostnimi razredi (in kvadraturo)

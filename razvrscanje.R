require(readr)
library(dplyr)
library(rvest)
library(gsubfn)
library(knitr)
library(rvest)
library(ggplot2)
library(tidyr)


podatki <- read.csv("obcine.csv", encoding="UTF-8")
podatki[,1]=tolower(podatki[,1])

potresi <- read_csv2("regije.csv", locale=locale(encoding="cp1250"))


# Naslov, od koder pobiramo podatke
link <- "https://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
stran <- html_session(link) %>% read_html()

# Preberemo prvo ustrezno tabelo
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
  .[[1]] %>% html_table()

# Nadomestimo decimalne vejice in locila tisocic ter pretvorimo v stevila
tabela[[2]] <- tabela[[2]] %>% strapplyc("([0-9]+)") %>%
  sapply(paste, collapse = ".") %>% as.numeric()
tabela[[3]] <- tabela[[3]] %>% strapplyc("([0-9]+)") %>%
  sapply(paste, collapse = "") %>% as.numeric()

# Odstranimo nekaj stolpcev
tabela <- tabela[,-c(2,3,4,5,6,7,9)]
tabela[,1]=tolower(tabela[,1])

#lepo rocno preimenujemo sentjur pri celju na -->sentjur
podatki[c(129,130,131,132),c(1)] = tabela[c(172,172,172,172), c(1)]

podatki$Regija=tabela$`Statisticna regija`[match(podatki$Obcina, tabela$`Ime obcine`)]

podatki <- podatki%>%group_by(Regija,Ranljivostni_razredi) %>%
  summarise(Povrsina = sum(Povrsina), Stevilo=sum(Stevilo))

#trik, da lahko matcham na cifre in mi preimenuje regije, da bodo v obeh tabelah enako
podatki$cifre <-c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12)

potresi$cifre <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12)

potresi$Regija=podatki$Regija[match(potresi$cifre,podatki$cifre)]
#se pobrisem cifre
podatki<-podatki[,-c(5)]
potresi <-potresi[,-c(4)]

#SIMULACIJA

#ce zelim obdovje 10 let, potem samo vse parametre pomnozim *10
#ker so sedaj ocenjeni na eno leto

potresi$`Ocenjen parameter` = potresi$`Ocenjen parameter` * 10

potresi$'P(x=0)' = dpois(x=0, lambda= potresi$`Ocenjen parameter`)
potresi$'P(x=1)'=dpois(x=1, lambda= potresi$`Ocenjen parameter`)
potresi$'P(x=2)'=dpois(x=2, lambda= potresi$`Ocenjen parameter`)
potresi$'P(x=3)'=dpois(x=3, lambda= potresi$`Ocenjen parameter`)
potresi$'P(x=4)'=dpois(x=4, lambda= potresi$`Ocenjen parameter`)
potresi$'P(x=5)'=dpois(x=5, lambda= potresi$`Ocenjen parameter`)

#za tem sledi pricakovana skoda

#SKODA PRI STOPNJI 6
podatki$'skoda_6'=podatki$Povrsina * c((0.25*0.04 + 0.05*0.09), 0.1*0.04, 0, 0)
#(B*(0.25*0.04 + 0.05*0.09) + C*0.1*0.04 + D*0 + E*0) *verj.potresa 6

#SKODA PRI STOPNJI 7
podatki$'skoda_7'=podatki$Povrsina * c((0.35*0.04 + 0.30*0.09 + 0.10*0.2), 0.1*0.04, 0, 0)
#(B*(0.35*0.04 + 0.30*0.09 + 0.10*0.2) + C*0.10*0.04 + D*0 + E*0)*verj.potresa 7

#SKODA PRI STOPNJI 8
podatki$'skoda_8'=podatki$Povrsina * c((0.25*0.04 + 0.35*0.09 + 0.30*0.2 + 0.1*0.4), (0.35*0.04 + 0.3*0.09 + 0.1*0.2),
                                       (0.3*0.04 + 0.1*0.09), 0)
#(B*(0.25*0.04 + 0.35*0.09 + 0.30*0.2 + 0.1*0.4) + C*(0.35*0.04 + 0.3*0.09 + 0.1*0.2) +
# + D*(0.3*0.04 + 0.1*0.09) + E*0)*verj.potresa 8

#SKODA PRI STOPNJI 9
podatki$'skoda_9'=podatki$Povrsina * c((0.25*0.09 + 0.35*0.2 + 0.3*0.4 + 0.1*0.8), (0.25*0.04 + 0.35*0.09 + 0.30*0.2+ 0.10*0.4),
                                       (0.35*0.04 + 0.3*0.09 + 0.1*0.2), (0.3*0.04 + 0.10*0.09))
#(B*(0.25*0.09 + 0.35*0.2 + 0.3*0.4 + 0.1*0.8) + c*(0.25*0.04 + 0.35*0.09 + 0.30*0.2+ 0.10*0.4)+
#+ D*(0.35*0.04 + 0.3*0.09 + 0.1*0.2) + E*(0.3*0.04 + 0.10*0.09))*verj.potresa 9

transponiranka <-t(potresi)

skoda_regije <- podatki%>%group_by(Regija)%>%summarise(Skoda6=sum(skoda_6),Skoda7=sum(skoda_7),Skoda8=sum(skoda_8),
                                                       Skoda9=sum(skoda_9))
#pomnozimo se z verjetnostmi da potresi teh stopenj sploh nastopijo
ricakovana_skoda <- potresi$`P(x=1)` * 
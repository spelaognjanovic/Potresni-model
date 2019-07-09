require(readr)
library(dplyr)
library(rvest)
#library(gsubfn)
#library(knitr)
#library(ggplot2)
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

#za tem sledi pricakovana skoda

#SKODA PRI STOPNJI 6
podatki$'skoda_6'=podatki$Povrsina * c((0.25*0.04 + 0.05*0.09), 0.1*0.04, 0, 0)
#SKODA PRI STOPNJI 7
podatki$'skoda_7'=podatki$Povrsina * c((0.35*0.04 + 0.30*0.09 + 0.10*0.2), 0.1*0.04, 0, 0)
#SKODA PRI STOPNJI 8
podatki$'skoda_8'=podatki$Povrsina * c((0.25*0.04 + 0.35*0.09 + 0.30*0.2 + 0.1*0.4), (0.35*0.04 + 0.3*0.09 + 0.1*0.2),
                                       (0.3*0.04 + 0.1*0.09), 0)
#SKODA PRI STOPNJI 9
podatki$'skoda_9'=podatki$Povrsina * c((0.25*0.09 + 0.35*0.2 + 0.3*0.4 + 0.1*0.8), (0.25*0.04 + 0.35*0.09 + 0.30*0.2+ 0.10*0.4),
                                       (0.35*0.04 + 0.3*0.09 + 0.1*0.2), (0.3*0.04 + 0.10*0.09))

#transponiranka <-t(potresi)
#samo seštejem skode za vsako regijo posebej, vendar to zapišem v škoda regije

skoda_regije <- podatki%>%group_by(Regija)%>%summarise(Skoda6=sum(skoda_6) ,Skoda7=sum(skoda_7),Skoda8=sum(skoda_8),
                                                       Skoda9=sum(skoda_9), Stevilo=sum(Stevilo))

#škodo po regijah zmanjšam oz. povečam za ustrezen faktor, odvisen od velikosti potresa, oceim s pomočjo aplikacije
skoda_regije$Skoda6 = skoda_regije$Skoda6 * (1/3)
skoda_regije$Skoda7 = skoda_regije$Skoda7 * (2/3)
#za 8 se lih izide, ne spreminjam
#za 9 ne igra vloge, če preštejem nekaj tisoč k miljonski vrednosti škode

#skoda na enoto:
#skoda_regije$Skoda6= c(skoda_regije$Skoda6)/c(skoda_regije$Stevilo)
#skoda_regije$Skoda7= c(skoda_regije$Skoda7)/c(skoda_regije$Stevilo)
#skoda_regije$Skoda8= c(skoda_regije$Skoda8)/c(skoda_regije$Stevilo)
#skoda_regije$Skoda9= c(skoda_regije$Skoda9)/c(skoda_regije$Stevilo)

#Skodo po regijah pomnozimo s verjetnostjo iz tabele potresi, da se potres te stopnje v tej regiji spoh zgodi 
#ST. POTRESOV = 1
ena <-  podatki%>%group_by(Regija)%>%summarise(Skoda6=sum(skoda_6) ,Skoda7=sum(skoda_7),Skoda8=sum(skoda_8),
                                               Skoda9=sum(skoda_9), Stevilo=sum(Stevilo))
ena$Skoda6 = ena$Skoda6 * (1/3)
ena$Skoda7 = ena$Skoda7 * (2/3)

ena$Skoda6 = ena$Skoda6 * potresi$`P(x=1)`[seq(1, 48, 4)]
ena$Skoda7 = ena$Skoda7 * potresi$`P(x=1)`[seq(2, 48, 4)]
ena$Skoda8 = ena$Skoda8 * potresi$`P(x=1)`[seq(3, 48, 4)]
ena$Skoda9 = ena$Skoda9 * potresi$`P(x=1)`[seq(4, 48, 4)]
#ST. POTRESOV = 2
podatki <-  podatki%>%group_by(Regija)%>%summarise(povrsina=sum(Povrsina),Skoda6=sum(skoda_6) ,Skoda7=sum(skoda_7),Skoda8=sum(skoda_8),
                                                   Skoda9=sum(skoda_9), Stevilo=sum(Stevilo))
podatki$Skoda6 = podatki$Skoda6 * (1/3)
podatki$Skoda7 = podatki$Skoda7 * (2/3)

dva <- podatki[,-c(3,4,5,6,7)]
dva$'S66' = podatki$Skoda6 * potresi$`P(x=2)`[seq(1, 48, 4)]*2
dva$'S67'=c(0,0)
dva$'S68'=c(0,0)
dva$'S69'=c(0,0)
dva$'S77' = podatki$Skoda7 * potresi$`P(x=2)`[seq(2, 48, 4)]*2
dva$'S78'=c(0,0)
dva$'S79'=c(0,0)
dva$'S88' = podatki$Skoda8 * potresi$`P(x=2)`[seq(3, 48, 4)]*2
dva$'S89'=c(0,0)
dva$'S99' = podatki$Skoda9 * potresi$`P(x=2)`[seq(4, 48, 4)]*2

#ST. POTRESOV = 3
tri <-podatki[,-c(3,4,5,6,7)]

tri$'S666' = podatki$Skoda6 * potresi$`P(x=3)`[seq(1, 48, 4)]*3
tri$'S777' = podatki$Skoda7 * potresi$`P(x=3)`[seq(2, 48, 4)]*3
tri$'S888' = podatki$Skoda8 * potresi$`P(x=3)`[seq(3, 48, 4)]*3
tri$'S999' = podatki$Skoda9 * potresi$`P(x=3)`[seq(4, 48, 4)]*3

#skoda_regije$ver6 <- c(potresi$`P(x=1)`[seq(1, 48, 4)])
#skoda_regije$Skoda66 <- skoda_regije$Skoda6 * 2
#skoda_regije$ver66 <- c(potresi$`P(x=2)`[seq(1, 48, 4)])


#sestevanje vrstic
#pricakovana_skoda <- skoda_regije[,-c(2,3,4,5)]

#skoda_regije <- skoda_regije[,-c(1)]

#pricakovana_skoda$pricak_skoda <- rowSums(skoda_regije)

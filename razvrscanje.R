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

podatki$cifre <-c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12)


#potresi$cifre <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,10,10,10,10,11,11,11,11,12,12,12,12)
#SIMULACIJA


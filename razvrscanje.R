require(readr)
library(dplyr)
library(rvest)
library(gsubfn)

razvrstitev <- read_csv2("razvrstitev.csv", locale=locale(encoding="cp1250"))

regije <- read_csv2("regije.csv", locale=locale(encoding="cp1250"))

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

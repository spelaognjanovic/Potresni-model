require(readr)
library(dplyr)
library(rvest)
library(tidyr)

skode <- read.csv("skoda_regije.csv", encoding="UTF-8")

verjetnosti <- read.csv("potresi.csv", encoding="UTF-8")
names(verjetnosti) <- c('regija','stopnja','parameter','P(x=0)','P(x=1)','P(x=2)','P(x=3)','P(x=4)')

gorenjska <- 

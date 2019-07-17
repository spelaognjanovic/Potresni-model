require(readr)
library(dplyr)
library(rvest)
library(tidyr)
library(ggplot2)

skode <- read.csv("skoda_regije.csv", encoding="UTF-8")

v <- read.csv("potresi.csv", encoding="UTF-8")
names(v) <- c('regija','stopnja','parameter','N','E','D','T','S')

#nizi <- c("(0,0,0,6)","(0,0,0,7)","(0,0,0,8)","(0,0,0,9)","(0,0,6,6)")
nizi <- c(6,7,8,9,66,67,68,69,77,78,79,88,89,99,666,667,668,669,677,678,679,688,689,699,777,778,779,788,789,799,888,889,899,999,
          6666,6667,6668,6669,6677,6678,6679,6688,6689,6699,6777,6778,6779,6788,6789,6799,6888,6889,6899,6999,
          7777,7778,7779,7788,7789,7799,7888,7889,7899,7999,8888,8889,8899,8999,9999)
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
        )

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
          )

pricakovana<- c(skoda)*c(verj)

gorenjska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)

gorenjska$verj=round(gorenjska$verj,digits = 7)
gorenjska$pricakovana=round(gorenjska$pricakovana,digits = 1)

skoda2<-skoda**2

#skoda3 <- log(skoda)
#ggplot gorenjska
#Gorenjska1 <- ggplot(gorenjska, aes(x=skoda3, y=verj))+ geom_point(size = 1.3, color="red")
#print(Gorenjska1)


#Najdi krivuljo, polinom, ki se točkam najbilj prilega
#fit<-lm(verj~poly(skoda3,2))
#plot(skoda3,verj,pch=20,col="blue")

#x0 <- seq(min(skoda3), max(skoda3), length = 15)  ## prediction grid
#y0 <- predict.lm(fit, newdata = list(skoda3 = x0))  ## predicted values
#lines(x0, y0, col = 2)  ## add regression curve (colour: red)

#fit$coef

#C=0.008051705
#B=-0.163425465
#A=0.168264225

#x = seq(10,16,0.1)
#y <- A*(x**2) + B*x + C
#plot(x,y,type="l")

#vsak krog ko grem na novo regijo pobrišem prve 4 vrstice, da je koda za verjetnosti ista 
#GORIŠKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
goriska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
goriska$verj=round(goriska$verj,digits = 7)
goriska$pricakovana=round(goriska$pricakovana,digits = 1)

#skoda3 <- log(skoda)
#ggplot goriska, isto ko gorensjka, iste verjetnosti
#Goriska1 <- ggplot(goriska, aes(x=skoda3, y=verj))+ geom_point(size = 1.3, color="red")
#print(Goriska1)



#JUGOVZHODNA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
jugovzhodna <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
jugovzhodna$verj=round(jugovzhodna$verj,digits = 7)
jugovzhodna$pricakovana=round(jugovzhodna$pricakovana,digits = 1)

#KOROSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
koroska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
koroska$verj=round(koroska$verj,digits = 7)
koroska$pricakovana=round(koroska$pricakovana,digits = 1)

#NOTRANJSKO KRASKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
notranjsko.k<- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
notranjsko.k$verj=round(notranjsko.k$verj,digits = 7)
notranjsko.k$pricakovana=round(notranjsko.k$pricakovana,digits = 1)

#OBALNO KRAŠKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
obalno.k <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
obalno.k$verj=round(obalno.k$verj,digits = 7)
obalno.k$pricakovana=round(obalno.k$pricakovana,digits = 1)

#OSREDNJESLOVENSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
osrednje <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
osrednje$verj=round(osrednje$verj,digits = 7)
osrednje$pricakovana=round(osrednje$pricakovana,digits = 1)

#PODRAVSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
podravska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
podravska$verj=round(podravska$verj,digits = 7)
podravska$pricakovana=round(podravska$pricakovana,digits = 1)

#POMURSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
pomurska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
pomurska$verj=round(pomurska$verj,digits = 7)
pomurska$pricakovana=round(pomurska$pricakovana,digits = 1)

#SAVINJSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
savinjska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
savinjska$verj=round(savinjska$verj,digits = 7)
savinjska$pricakovana=round(savinjska$pricakovana,digits = 1)

#SPODNJEPOSAVSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
spodnjeposavska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
spodnjeposavska$verj=round(spodnjeposavska$verj,digits = 7)
spodnjeposavska$pricakovana=round(spodnjeposavska$pricakovana,digits = 1)

#ZASAVSKA
v <-v[-c(1,2,3,4),]
skode <- skode[-c(1),]
verj<-c(v$E[1]*v$N[2]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$N[4],v$E[3]*v$N[1]*v$N[2]*v$N[4],v$E[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$D[1]*v$N[2]*v$N[3]*v$N[4],v$E[1]*v$E[2]*v$N[3]*v$N[4],v$E[1]*v$N[2]*v$E[3]*v$N[4],v$E[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$N[1]*v$N[3]*v$N[4],v$E[2]*v$N[1]*v$E[3]*v$N[4],v$E[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$N[2]*v$N[4],
        v$E[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$N[3],
        
        v$T[1]*v$N[2]*v$N[3]*v$N[4],v$D[1]*v$E[2]*v$N[3]*v$N[4],v$D[1]*v$N[2]*v$E[3]*v$N[4],v$D[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$E[1]*v$N[3]*v$N[4],v$E[2]*v$E[1]*v$E[3]*v$N[4],v$E[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$N[2]*v$N[4],
        v$E[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$N[1]*v$N[3]*v$N[4],v$D[2]*v$N[1]*v$E[3]*v$N[4],v$D[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$E[2]*v$N[4],
        v$E[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$N[2]*v$N[4],v$D[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$E[3],v$T[4]*v$N[1]*v$N[2]*v$N[3],
        
        
        v$S[1]*v$N[2]*v$N[3]*v$N[4],v$T[1]*v$E[2]*v$N[3]*v$N[4],v$T[1]*v$N[2]*v$E[3]*v$N[4],v$T[1]*v$N[2]*v$N[3]*v$E[4],
        v$D[2]*v$D[1]*v$N[3]*v$N[4],v$E[2]*v$D[1]*v$E[3]*v$N[4],v$E[2]*v$D[1]*v$N[3]*v$E[4],v$D[3]*v$D[1]*v$N[2]*v$N[4],
        v$E[3]*v$D[1]*v$N[2]*v$E[4],v$D[4]*v$D[1]*v$N[2]*v$N[3],
        
        v$T[2]*v$E[1]*v$N[3]*v$N[4],v$D[2]*v$E[1]*v$E[3]*v$N[4],v$D[2]*v$E[1]*v$N[3]*v$E[4],v$D[3]*v$E[1]*v$E[2]*v$N[4],
        v$E[3]*v$E[1]*v$E[2]*v$E[4],v$D[4]*v$E[1]*v$E[2]*v$N[3],
        
        v$T[3]*v$E[1]*v$N[2]*v$N[4],v$D[3]*v$E[1]*v$N[2]*v$E[4],v$D[4]*v$E[1]*v$N[2]*v$E[3],v$T[4]*v$E[1]*v$N[2]*v$N[3],
        
        
        v$S[2]*v$N[1]*v$N[3]*v$N[4],v$T[2]*v$N[1]*v$E[3]*v$N[4],v$T[2]*v$N[1]*v$N[3]*v$E[4],v$D[3]*v$N[1]*v$T[2]*v$N[4],
        v$E[3]*v$N[1]*v$D[2]*v$E[4],v$D[4]*v$N[1]*v$D[2]*v$N[3],
        
        v$T[3]*v$N[1]*v$E[2]*v$N[4],v$D[3]*v$N[1]*v$E[2]*v$E[4],v$D[4]*v$N[1]*v$E[2]*v$E[3],v$T[4]*v$N[1]*v$E[2]*v$N[3],
        
        
        v$S[3]*v$N[1]*v$N[2]*v$N[4],v$T[3]*v$N[1]*v$N[2]*v$E[4],v$D[4]*v$N[1]*v$N[2]*v$D[3],v$T[4]*v$N[1]*v$N[2]*v$E[3],
        v$S[4]*v$N[1]*v$N[2]*v$N[3] 
)

skoda = c(skode$Skoda6[1],skode$Skoda7[1],skode$Skoda8[1],skode$Skoda9[1],skode$Skoda6[1]*2,skode$Skoda6[1]+skode$Skoda7[1],
          skode$Skoda6[1]+skode$Skoda8[1],skode$Skoda6[1]+skode$Skoda9[1],skode$Skoda7[1]*2,skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda8[1]*2,skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda9[1]*2,
          
          skode$Skoda6[1]*3,skode$Skoda6[1]*2+skode$Skoda7[1],skode$Skoda6[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]*2,skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda8[1]*2,skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]*3,skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda7[1]+skode$Skoda8[1]*2,skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda8[1]*3,skode$Skoda8[1]*2+skode$Skoda9[1],skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda9[1]*3,
          
          
          skode$Skoda6[1]*4,skode$Skoda6[1]*3+skode$Skoda7[1],skode$Skoda6[1]*3+skode$Skoda8[1],
          skode$Skoda6[1]*3+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda7[1]*2,skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda8[1],
          skode$Skoda6[1]*2+skode$Skoda7[1]+skode$Skoda9[1],skode$Skoda6[1]*2+skode$Skoda8[1]*2,skode$Skoda6[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda6[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda7[1]*3,skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda8[1],
          skode$Skoda6[1]+skode$Skoda7[1]*2+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]*2,
          skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1],skode$Skoda6[1]+skode$Skoda7[1]+skode$Skoda9[1]*2,
          
          skode$Skoda6[1]+skode$Skoda8[1]*3,skode$Skoda6[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda6[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda6[1]+skode$Skoda9[1]*3,
          
          skode$Skoda7[1]*4,skode$Skoda7[1]*3+skode$Skoda8[1],
          skode$Skoda7[1]*3+skode$Skoda9[1],skode$Skoda7[1]*2+skode$Skoda8[1]*2,skode$Skoda7[1]*2+skode$Skoda8[1]+skode$Skoda9[1],
          skode$Skoda7[1]*2+skode$Skoda9[1]*2,
          
          skode$Skoda7[1]+skode$Skoda8[1]*3,skode$Skoda7[1]+skode$Skoda8[1]*2+skode$Skoda9[1],
          skode$Skoda7[1]+skode$Skoda8[1]+skode$Skoda9[1]*2,skode$Skoda7[1]+skode$Skoda9[1]*3,
          
          skode$Skoda8[1]*4,skode$Skoda8[1]*3+skode$Skoda9[1],skode$Skoda8[1]*2+skode$Skoda9[1]*2,
          skode$Skoda8[1]+skode$Skoda9[1]*3,skode$Skoda9[1]*4
)

pricakovana<- c(skoda)*c(verj)
zasavska <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)
zasavska$verj=round(zasavska$verj,digits = 7)
zasavska$pricakovana=round(zasavska$pricakovana,digits = 1)

#GRAFI PORAZDELITEV
skoda3 <- log(skoda)
#ggplot gorenjska
Gorenjska1 <- ggplot(gorenjska, aes(x=skoda3, y=verj))+ geom_point(size = 1.3, color="red")
print(Gorenjska1)


#Najdi krivuljo, polinom, ki se točkam najbilj prilega
fit<-lm(verj~poly(skoda3,2))
plot(skoda3,verj,pch=20,col="blue")

x0 <- seq(min(skoda3), max(skoda3), length = 15)  ## prediction grid
y0 <- predict.lm(fit, newdata = list(skoda3 = x0))  ## predicted values
lines(x0, y0, col = 2)  ## add regression curve (colour: red)

fit$coef
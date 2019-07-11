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

skoda3 <- log(skoda)

regija1 <- ggplot(gorenjska, aes(x=skoda3, y=verj))+geom_line(size = 0.05) + geom_point(size = 1.3, color="red")

print(regija1)

#lines(skoda3, predict(fit2, data.frame(x=skoda3)), col="green")
#quadratic.model <-lm(verj ~ skoda + skoda2)
#summary(quadratic.model)
#timevalues <- seq(255555, 10685043, 50000)
#predictedcounts <- predict(quadratic.model,list(skoda=timevalues, skoda2=timevalues^2))
#plot(skoda, verj, pch=16, xlab = "skoda (s)", ylab = "verj", cex.lab = 1.3, col = "blue")
#lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)

#vsak krog ko grem na novo regijo pobrišem prve 4 vrstice, da je koda za verjetnosti ista
#verjetnosti <-verjetnosti[-c(1,2,3,4),]
#goriška <- data.frame("nizi"= nizi, "skoda"= skoda,"verj"= verj, "pricakovana"= pricakovana)

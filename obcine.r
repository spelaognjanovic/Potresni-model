library(rgeos)

zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
                             pot.zemljevida="OB", encoding="Windows-1250")
A <- gTouches(zemljevid, byid=TRUE)

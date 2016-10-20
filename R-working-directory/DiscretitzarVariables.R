plot(base$vivenda)

# DISCRETITZACIO DE VARIABLES

hist(base$anys.feina)
quantile(cresco$anys.feina, seq(0,1,0,1))

crescoAnysFeina<- factor(cut(cresco$anys.feina, breaks=c(-1,2,5,12,48)))
summary(crescoAnysFeina)

#el summary de sobre queda poc balancejat aixi que no agafem els que valen 2
crescoAnysFeina<- factor(cut(cresco$anys.feina, breaks=c(-1,1.9,5,12,48)))
summary(crescoAnysFeina)

# donem la mitjana d'altura segons el genere
# tapply(altura, genere, mitjana)

tapply(crescoAnysFeina, cresco$anys.feina, median)

# Vivenda
summary(cresco$vivenda)

crescoiF.habi <- factor(cresco$vivenda, levels=1:6) #falta el final escrit a la pissara
crescoiF.habi

llista<-levels(crescoiF.habi)
llista
summary(llista)

#em de tenir en compte els outlayers al principi

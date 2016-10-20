---
  title: "Data Validation"
  author: "Guillem Casanova"
  date: "September 2016"
  output: html_document
  toc: true
  toc_depth: 3
  number_sections: true
---
  
  # Introduction
  
  ## Load data
  
  
setwd("/Users/guillem/Google Drive/FIB/ADEI/R/R-working-directory")

base = read.table("dades.txt",header=T,sep='\t',na.string='99999999')

dim(base)
names(base)
head(base, 12)
tail(base,8)

cresco<-base

save.image("credsco_raw.RData")

# Data Coding and Clearance

## Final Decision: dictamen
summary(cresco)
badInd = cresco[4456,]
badInd
cresco <- cresco[-4456,]
# cresco$dictamen  useless
table(cresco$dictamen)
# Remove observations with missing values (TARGET)

llista<-which( cresco$dictamen == 0);
llista
cresco[3310, 5] # Cel.la 3310,5 
cresco[3310,] # Fila 3310
cresco[c(3310,112),] # Files 3310, 112
cresco[1:4, c(1,3,5)]
cresco[1:4, "dictamen"]
c(3310,112)

# Treiem tots els elements cresco$dictamen que valen 0
cresco<-cresco[-llista,]
paste("f.dict",c("Accepted","Rejected"),sep="-") # genera els strings f.dic-Accepted o f.dic-Rejected
cresco$f.dictamen<-factor(cresco$dictamen, levels=1:2, labels=paste("f.dict",c("Accepted","Rejected"),sep="-"))
summary(cresco$f.dictamen)

# Univariant Exploratory Data Analysis (EDA)
summary(cresco$f.dictamen)
table(cresco$f.dictamen) # Better
round(100*(table(cresco$f.dictamen)/nrow(cresco)),dig=2)

# Graphics
# Pie
piepercent<-round(100*(table(cresco$f.dictamen)/nrow(cresco)),dig=2); piepercent

pie(table(cresco$f.dictamen),col=heat.colors(2),labels=paste(piepercent,"%"))

legend("topright", levels(cresco$f.dictamen), cex = 0.8, fill = heat.colors(2))

# Bar Chart
barplot(table(cresco$f.dictamen),main="Barplot Final Decision Factor",col=c("green","red"))


## Experience in current job (years)

#Now an example on a numeric variable
names(cresco)
summary(cresco$anys.feina)
# Quartils
quantile(cresco$anys.feina)
seq(0, 1, 0.1)
quantile(cresco$anys.feina, probs = seq(0, 1, 0.1))

# Graphics
plot(cresco$anys.feina)
hist(cresco$anys.feina)
hist(cresco$anys.feina,20)
hist(cresco$anys.feina,breaks=seq(0,50,4))
hist(cresco$anys.feina,freq=FALSE,breaks=seq(0,50,4),col=heat.colors(11))
# 
boxplot(cresco$anys.feina, main="Boxplot Years in current Job")
# Calculate upper thresholds for mild and severe outliers
abline(h=20,col="red",lwd=2)
abline(h=40,col="purple",lwd=2)
calcQ(cresco$anys.feina)
llista<-which( cresco$anys.feina > 42);
llista
llista<-which( cresco$anys.feina < -28);
llista

# Function to calculate the outliers thresholds
calcQ <- function(x) {
  s.x <- summary(x)
  iqr<-s.x[5]-s.x[2]
  list(souti=s.x[2]-3*iqr, mouti=s.x[2]-1.5*iqr, min=s.x[1], q1=s.x[2], q2=s.x[3], 
       q3=s.x[5], max=s.x[6], mouts=s.x[5]+1.5*iqr, souts=s.x[5]+3*iqr )
  }

#Analisis de la vivenda
cresco$vivenda
v <- cresco$vivenda
llista <-which(v != 1 & v != 2 & v != 3 & v != 4 & v != 5 & v != 6)
llista
aux<-cresco$vivenda[-llista,]

#Analisis de la estat civil
cresco$vivenda
v <- cresco$estat.civil
llista <-which(v != 1 & v != 2 & v != 3 & v != 4 & v != 5)
llista
aux<-cresco$estat.civil[-llista,]

#Analisis de la tipus feina
cresco$tipus.feina
v <- cresco$tipus.feina
llista <-which(v != 1 & v != 2 & v != 3 & v != 4)
llista
aux<-cresco$tipus.feina[-llista,]

cor(cresco$edat, cresco$despeses)

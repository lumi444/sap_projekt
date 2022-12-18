#ucitavanje podataka
data=read.csv('C://Users/Franjo/OneDrive/Dokumenti/SAP/preprocessed_data.csv')
library(tidyverse)
dim(data)
year2006 = which(data$YrSold=='2006')
year2007 = which(data$YrSold=='2007')
year2008 = which(data$YrSold=='2008')
year2009= which(data$YrSold=='2009')
year2010=which(data$YrSold=='2010')
#prosjeci za broj prodanih nekretnina u odredenoj godini

boxplot(year2006, year2007, year2008,year2009,year2010,
        main="Distribucija prodanih nekretnina po godini izgradnje",
        names=c("06 yrs", "07 yrs", "08 yrs", "09 yrs", "10 yrs"), 
        xlab='Godina izgradnje',ylab="Postotak", col=rainbow(6))
#Prilozeni dijagram prikazuje distribuciju prodanih nekretnina u odredenoj godini,
#iz dijagrama uocavamo da je najveci broj nekretnina prodan u 2009

data %>% group_by(YrSold) %>% summarise(
  uvjeti=n()
) -> summary
summary
#udjeli s obzirom na uvjete prodaje
data %>% group_by(BdrAbove) %>% summarise(
  uvjeti=n()
) -> summary
summary

hist(data$BedroomAbvGr,
     main='Distribucija broja spavacih soba prodanih nekretnina', 
     xlab='Broj spavacih soba',
     ylab='Broj prodanih nekretnina')

#Iz prilozenog histograma uocavamo da ukupan broj prodanih nekretnina ima 

abnorml.number = sum(SaleCondition == "Abnorml")/1460*100
adjland.number = sum(SaleCondition == "AdjLand")/1460*100
alloca.number = sum(SaleCondition == "Alloca")/1460*100
family.number = sum(SaleCondition == "Family")/1460*100
normal.number = sum(SaleCondition == "Normal")/1460*100
partial.number = sum(SaleCondition == "Partial")/1460*100
values = c(abnorml.number, adjland.number, alloca.number,family.number,normal.number,partial.number)
labels = c("Abnorml", "AdjLand", "Alloca","Family","Normal","Partial")
pct = round(values/sum(values)*100, digits = 2)
labels = paste(labels, pct)
labels = paste(labels,"%")
pie(values, labels=labels, col=rainbow(length(labels)))

#dijagram koji prikazuje udjele prodanih nekretnina u pojedinom naselju

data %>% group_by(Neighborhood) %>% summarise(
  uvjeti=n()
) -> summary
summary

blmngtn = which(data$Neighborhood=='Blmngtn')/1460*100
blueste = which(data$Neighborhood=='Blueste')/1460*100
brdale = which(data$Neighborhood=='BrDale')/1460*100
brkside = which(data$Neighborhood=='BrkSide')/1460*100
clearcr = which(data$Neighborhood=='ClearCr')/1460*100
collgcr = which(data$Neighborhood=='CollgCr')/1460*100
crawfor = which(data$Neighborhood=='Crawfor')/1460*100
edwards = which(data$Neighborhood=='Edwards')/1460*100
gilbert = which(data$Neighborhood=='Gilbert')/1460*100
IDOTRR = which(data$Neighborhood=='IDOTRR')/1460*100
labels = c("Blmngtn", "Blueste", "BrDale",
           "BrkSide", "ClearCr", "CollgCr", "Crawfor",
           "Edwards", "Gilbert", "IDOTRR")
par(mar = c(10.5,5,2,2))
boxplot(blmngtn, blueste, brdale, brkside, clearcr, collgcr,
        crawfor, edwards, gilbert, IDOTRR,names=labels,
        ylab="Percentage", las = 2, col=rainbow(length(labels)))


#Zanima nas
#razlikuju li se uspješnosti fondova s obzirom na stil investiranja koji odabiru, odnosno želimo provijeriti imaju
#li fodnovi s određenim stilom investiranja veće povrate nego ostali. Za početak želimo vidjeti ravnaju li
#se povrati svake od tih kategorija po normalnoj razdiobi kako bi mogli primjeniti anovu, test o jednakosti
#sredina.
BedroomAbvGr
data %>% group_by(BedroomAbvGr) %>% summarise(
  uvjeti=n()
) -> summary
summary

data_bed0<- data[data$BedroomAbvGr == c("0"),]
hist(data_bed0$SalePrice)
qqnorm(data_bed0$SalePrice, main="0")
qqline(data_bed0$SalePrice, col="blue")

data_bed1<- data[data$BedroomAbvGr == c("1"),]
hist(data_bed1$SalePrice)
qqnorm(data_bed1$SalePrice, main="1")
qqline(data_bed1$SalePrice, col="blue")

data_bed2<- data[data$BedroomAbvGr == c("2"),]
hist(data_bed2$SalePrice)
qqnorm(data_bed2$SalePrice, main="2")
qqline(data_bed2$SalePrice, col="blue")

data_bed3<- data[data$BedroomAbvGr == c("3"),]
hist(data_bed3$SalePrice)
qqnorm(data_bed3$SalePrice, main="3")
qqline(data_bed3$SalePrice, col="blue")

data_bed4<- data[data$BedroomAbvGr == c("4"),]
hist(data_bed4$SalePrice)
qqnorm(data_bed4$SalePrice, main="4")
qqline(data_bed4$SalePrice, col="blue")

data_bed5<- data[data$BedroomAbvGr == c("5"),]
hist(data_bed5$SalePrice)
qqnorm(data_bed5$SalePrice, main="5")
qqline(data_bed5$SalePrice, col="blue")

data_bed6<- data[data$BedroomAbvGr == c("6"),]
hist(data_bed6$SalePrice)
qqnorm(data_bed6$SalePrice, main="6")
qqline(data_bed6$SalePrice, col="blue")

data_bed7<- data[data$BedroomAbvGr == c("7"),]
hist(data_bed7$SalePrice)
qqnorm(data_bed7$SalePrice, main="7")
qqline(data_bed7$SalePrice, col="blue")

data_bed8<- data[data$BedroomAbvGr == c("8"),]
hist(mean(data_bed8$SalePrice))
qqnorm(mean(data_bed8$SalePrice), main="8")
qqline(mean(data_bed8$SalePrice), col="blue")

data_b0 <- na.omit(data_bed0$SalePrice)
data_b1 <- na.omit(data_bed1$SalePrice)
data_b2 <- na.omit(data_bed2$SalePrice)
data_b8 <- na.omit(mean(data_bed8$SalePrice))
var(data_b0)
var(data_b1)
var(data_b2)
var(data_b8)



summary(data$SalePrice)
plot(data$SalePrice,
     col='blue',
     ylim=c(min(data$SalePrice),max(data$SalePrice)),
     ylab='Sale Price')

#vizualizacija broja katova
data %>% group_by(HouseStyle) %>% summarise(
  uvjeti=n()
) -> summary
summary
fin1.5 = which(data$HouseStyle=='1.5Fin')/1460*100
onestory = which(data$HouseStyle=='1Story')/1460*100
fin2.5 = which(data$HouseStyle=='2.5Fin')/1460*100
twostory = which(data$HouseStyle=='2Story')/1460*100
values_style=c(length(fin1.5),length(onestory),length(fin2.5),length(twostory))
hist(values_style,
     main='House style histogram', xlab='House style',
     ylab='Frequency')

#vizualizacija oblika kuce
data %>% group_by(LotShape) %>% summarise(
  uvjeti=n()
) -> summary
summary
ir1 = which(data$LotShape=='IR1')
ir2 = which(data$LotShape=='IR2')
ir3 = which(data$LotShape=='IR3')
reg = which(data$LotShape=='Reg')
values_shape=c(length(ir1),length(ir2),length(ir3),length(reg))
hist(values,
     main='House shape histogram', xlab='House shape',
     ylab='Frequency')
#scaterplot
boxplot(ir1,ir2,ir3,reg ~ fin1.5,onestory,fin2.5,twostory,data=data)

#oblik broj katova kuce

boxplot(values_style ~ values_shape,data=data,names=c("IR1","IR2","IR3","Reg"))
plot(aov(HouseStyle ~ LotShape, data=data), 2)
oneway.test(HouseStyle ~ LotShape, data=data )
sum(is.na(preprocessed_data$LotShape))
data$HouseStyle

#velicina podruma o kvartu
#h0: velicna podruma ne ovisi o kvartu h1: vel podruma ovisi o kvartu

oneway.test(TotalBsmtSF ~ Neighborhood, data=data )
summary(one.way)


boxplot(TotalBsmtSF ~ Neighborhood,data=data)
plot(aov(TotalBsmtSF ~ Neighborhood, data=data), 2)
#cijene kvadrata nekretnina

hist((data$SalePrice)/(data$GrLivArea),
     main='Cijena kvadrata nekretnine', xlab='Cijena kvadrata',
     ylab='Frequency')

model = lm((data$SalePrice)/(data$GrLivArea) ~ data$BedroomAbvGr)
plot((data$SalePrice)/(data$GrLivArea), data$BedroomAbvGr, xlab="Net annual expense ratio",
     ylab="Fund annual mean return")
abline(model, col="red")
qqnorm(rstandard(model))
qqline(rstandard(model))
plot(fitted(model), resid(model))
abline(0,0, col="red")


boxplot(SalePrice/GrLivArea ~ BedroomAbvGr,data=data)
#prema boxplotu mozemo zakljuciti da broj soba ovisi o cijenu kvadrata nekretnine

library(dplyr)
install.packages("car")
library(car)
leveneTest(SalePrice/GrLivArea ~ BedroomAbvGr, data=data)


plot(aov(SalePrice/GrLivArea ~ BedroomAbvGr, data=data), 2)
#donekle dobro distribuirani nalaze se na istoj crti

oneway.test(SalePrice/GrLivArea ~ BedroomAbvGr, data=data )

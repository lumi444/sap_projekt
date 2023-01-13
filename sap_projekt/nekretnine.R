ames = read.csv("preprocessed_data.csv")
install.packages("viridis")
install.packages("ggmosaic")
library(GGally)
ames$PricePerSqFt <- ames$SalePrice / ames$GrLivArea
library(magrittr) 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(vcd)

# Group the data by neighborhood and calculate the mean price per square foot for each neighborhood
ames_by_neighborhood <- ames %>% group_by(Neighborhood) %>% 
  summarize(MeanPricePerSqFt = mean(PricePerSqFt))

t.test(PricePerSqFt ~ Neighborhood, data = ames, alternative = "two.sided", 
       var.equal = TRUE, conf.level = 0.95, subset = Neighborhood %in% c("CollgCr", "Crawfor"))





#box plot - cijena po kvadratu vs kvart
ggplot(ames, aes(x = PricePerSqFt, y = Neighborhood)) + geom_boxplot()


#bar plot prosjecna cijena cijene kvadrata po kvartu
ggplot(ames_by_neighborhood, aes(x = Neighborhood, y = MeanPricePerSqFt)) + geom_bar(stat = "identity")

tableHSLS <- table(ames$HouseStyle, ames$LotShape)
chi_squared_test <- chisq.test(tableHSLS)
chi_squared_test

#hi-kvadrat test, p-vrijednost je manja od 0.05, iznosi 0.002031. Tako da zakljucujemo da oblik zemljista ne odreduje broj katova


colors <- c("red", "blue", "green", "orange")

# Create a mosaic plot of the contingency table
mosaicplot(tableHSLS,
           col=colors,
           xlab="House Style", ylab = "Lot Shape" ,main = "House Style by Shape of Lot")



install.packages("dplyr")
library(dplyr)


ames_by_bedrooms <- ames %>%
  group_by(BedroomAbvGr) %>%
  summarize(mean_price_sqft = mean(SalePrice/GrLivArea),
            sd_price_sqft = sd(SalePrice/GrLivArea))

aov <- aov(SalePrice/GrLivArea ~ BedroomAbvGr, data = ames)


summary(aov)

real_astate.grouped = aggregate(real_astate[c("GrLivArea","SalePrice")],
                                real_astate[c("BedroomAbvGr")],
                                FUN = sum)

print(real_astate.grouped)

barplot(real_astate.grouped$SalePrice/real_astate.grouped$GrLivArea,
        main = "broj soba",
        ylab = "cijena po kvadratu",
        names.arg = real_astate.grouped$BedroomAbvGr,
        las=2)



######################################darkovo
library(dplyr)   

data$BedroomAbvGr <- as.factor(data$BedroomAbvGr)
data$BedroomAbvGr
class(data$BedroomAbvGr)

class(data$SalePrice)
class(data$GrLivArea)
data$PricePerSqFt <- as.numeric(data$SalePrice) / as.numeric(data$GrLivArea)
data$PricePerSqFt
class(data$PricePerSqFt)

bartlett.test(
  list(data$PricePerSqFt[data$BedroomAbvGr=='0'],
       data$PricePerSqFt[data$BedroomAbvGr=='1'],
       data$PricePerSqFt[data$BedroomAbvGr=='2'],
       data$PricePerSqFt[data$BedroomAbvGr=='3'],
       data$PricePerSqFt[data$BedroomAbvGr=='4'],
       data$PricePerSqFt[data$BedroomAbvGr=='5'],
       data$PricePerSqFt[data$BedroomAbvGr=='6']
  )
)


#buduci da uzorak nema homogene varijance, probati cemo problem rijesiti s  kruskal-wallis
#koji to ne pretpostavlja



kruskal.test(data$PricePerSqFt~data$BedroomAbvGr)
#p vrijednost je manje od 0.05 da odbacujemo hipotezu da ne ovisi o broju soba ????????''

########################################
data$BedroomAbvGr = factor(data$BedroomAbvGr,levels = c(0,1,2,3,4,5,6,7,8),labels = c('0','1','2','3','4','5','6','7','8'))
data$PricePerSqFt <- data$SalePrice / data$GrLivArea
data_by_bedrooms <- data %>%
  group_by(BedroomAbvGr)%>%
  summarize(mean_price_sqft =mean(SalePrice/GrLivArea),
            sd_price_sqft=sd(SalePrice/GrLivArea),
  )

levels(data_by_bedrooms$BedroomAbvGr)
require(nortest)
lillie.test(data$PricePerSqFt)
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='0'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='1'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='2'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='3'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='4'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='5'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='6'])
lillie.test(data$PricePerSqFt[data_by_bedrooms$BedroomAbvGr=='8'])


data = data[complete.cases(data),] 
data = data[data$BedroomAbvGr %in% c(0,1,2,3,4,5,6,7,8),]

for(i in unique(data$BedroomAbvGr)){
  if(length(data$PricePerSqFt[data$BedroomAbvGr==i]) < 2){
    warning(paste("Bedroom ",i," has less than 2 observations"))
  }
  else{
    bartlett.test(data$PricePerSqFt[data$BedroomAbvGr==i])
  }
}





bartlett.test(data$PricePerSqFt ~ data$BedroomAbvGr)
# Perform Bartlett's test on SalePrice variances grouped by number of rooms
bartlett.test(list(data$PricePerSqFt[data$BedroomAbvGr=='0']),list(data$PricePerSqFt[data$BedroomAbvGr=='1']),
              list(data$PricePerSqFt[data$BedroomAbvGr=='2']),list(data$PricePerSqFt[data$BedroomAbvGr=='3']),
              list(data$PricePerSqFt[data$BedroomAbvGr=='4']),list(data$PricePerSqFt[data$BedroomAbvGr=='5']),
              list(data$PricePerSqFt[data$BedroomAbvGr=='6']),list(data$PricePerSqFt[data$BedroomAbvGr=='7']),list(data$PricePerSqFt[data$BedroomAbvGr=='8']))

# Print the test statistics and p-value
print(bartlett_test)

prices = c(data$PricePerSqFt[data$BedroomAbvGr=='0'], data$PricePerSqFt[data$BedroomAbvGr=='1'],
           data$PricePerSqFt[data$BedroomAbvGr=='2'], data$PricePerSqFt[data$BedroomAbvGr=='3'],
           data$PricePerSqFt[data$BedroomAbvGr=='4'], data$PricePerSqFt[data$BedroomAbvGr=='5'],
           data$PricePerSqFt[data$BedroomAbvGr=='6'], data$PricePerSqFt[data$BedroomAbvGr=='7'], 
           data$PricePerSqFt[data$BedroomAbvGr=='8'])
bedrooms = c(rep(0,length(data$PricePerSqFt[data$BedroomAbvGr=='0'])), 
             rep(1,length(data$PricePerSqFt[data$BedroomAbvGr=='1'])),
             rep(2,length(data$PricePerSqFt[data$BedroomAbvGr=='2'])), 
             rep(3,length(data$PricePerSqFt[data$BedroomAbvGr=='3'])),
             rep(4,length(data$PricePerSqFt[data$BedroomAbvGr=='4'])), 
             rep(5,length(data$PricePerSqFt[data$BedroomAbvGr=='5'])),
             rep(6,length(data$PricePerSqFt[data$BedroomAbvGr=='6'])), 
             rep(7,length(data$PricePerSqFt[data$BedroomAbvGr=='7'])),
             rep(8,length(data$PricePerSqFt[data$BedroomAbvGr=='8'])))
bartlett.test(data_by_bedrooms1$BedroomAbvGr ~ data_by_bedrooms1$mean_price_sqft)

summary(data)





bartlett.test(data$PricePerSqFt ~ data$BedroomAbvGr)
lillie.test(creditdata$income[creditdata$education=='elementary'])
lillie.test(creditdata$income[creditdata$education=='secondary'])
lillie.test(creditdata$income[creditdata$education=='university'])

hist(creditdata$income[creditdata$education=='elementary'])
hist(creditdata$income[creditdata$education=='secondary'])
hist(creditdata$income[creditdata$education=='university'])





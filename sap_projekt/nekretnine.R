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

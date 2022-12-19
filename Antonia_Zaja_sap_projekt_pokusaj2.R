
# Deskriptivna analiza

#Učitajmo potrebne pakete.
library(dplyr)

#Učitajmo podatke.

data=read.csv('C://Users/Franjo/OneDrive/Dokumenti/SAP/preprocessed_data.csv')
dim(data)


#Podatci se sastoje od 1460 nekretnina i 81 njihovih znacajki (varijabli).

#Koji su nam to opisi igrača?

names(data)


#Nekretnine su opisani raznim općenitim varijablama (kao što su ime, godine, za koji klub igraju, itd.) te "nogometnim" varijablama (kao što su kvantizirane karakteristike napada, obrane, itd.).


View(data)


#Dodatno, što više varijabli imamo, to je više naš skup podataka nepregledan. 
#Stoga ih je često poželjno izbaciti iz samog dataseta.


summary(data)

sapply(data, class)


Skup podataka se sastoji od "integer" i "character" podataka.


for (col_name in names(data)){
  if (sum(is.na(data[,col_name])) > 0){
    cat('Ukupno nedostajućih vrijednosti za varijablu ',col_name, ': ', sum(is.na(data[,col_name])),'\n')
  }
}

cat('\n Dimenzija podataka: ', dim(data))




#Promotrimo sada kako izgledaju neke od varijabli nad 
#kojima ćemo kasnije provesti analizu. Promotrimo najprije numeričke varijable.

hist(data$SalePrice,main='Sale price histogram', xlab='Sale price', ylab='Frequency')
hist(data$YrSold,main='Year sold histogram', xlab='Year sold', ylab='Frequency')
hist(data$BedroomAbvGr,main='Bedroom number histogram',xlab='Number of bedroom',ylab='Frequency', breaks=50)

Pokušajmo log transformacijom približiti podatke normalnoj distribuciji.

hist(log(data$SalePrice),main='Sale price histogram',xlab='Sale price',ylab='Frequency', breaks=50)
hist(log(data$GrLivArea),main='Year sold histogram',xlab='Year sold',ylab='Frequency', breaks=20)


Promotrimo sada kategoričke varijable. 

barplot(table(data$Neighborhood),las=2,cex.names=.5,main='Neighborhood of houses')
print('Igračeva preferirana noga za udarce: ')
barplot(table(data$LotShape),las=2,main='Shape of the field')




#Sada kada smo dobili bolji uvid u naše podatke, 
#možemo si postaviti zanimljiva pitanja te pokušati odgovoriti 
#na njih koristeći razne statističke alate.



## Jesu li nekretnine u kvartu Blmngtn veće od nekretina u kvartu CollgCr?
table(data$HouseStyle)

houses1.5 = data[data$HouseStyle == "1.5Fin",]
houses1story = data[data$HouseStyle == "1Story",]
houses2.5 = data[data$HouseStyle == "2.5Fin",]
houses2story = data[data$HouseStyle == "2Story",]

cat('Prosječna velicina nekretnina s 1.5 katom ', mean(houses1.5$GrLivArea),'\n')
cat('Prosječna velicina nekretnina s jednim katom ', mean(houses1story$GrLivArea), '\n')
cat('Prosječna velicina nekretnina s 2.5 katom ', mean(houses2.5$GrLivArea),'\n')
cat('Prosječna velicina nekretnina s dva katom ', mean(houses2story$GrLivArea), '\n')
```

```{r}
boxplot(houses1.5$GrLivArea, houses1story$GrLivArea,houses2.5$GrLivArea,houses2story$GrLivArea, 
        names = c('1.5 flour','one flour',
                  '2.5 flour',' two flour'),
        main='Boxplot ')
```

#Postoje indikacije da bi nekretnine s vecim brojem katova trebale biti vece
#od nekretnina s manjim.
#Ovakvo ispitivanje možemo provesti t-testom. Moraju li neke pretpostavke biti zadovoljene za naše podatke?
  

Kako bi mogli provesti test, moramo najprije provjeriti pretpostavke normalnosti i nezavisnosti uzorka. Obzirom da razmatramo dva uzoraka iz dvije različite zemlje, možemo pretpostaviti njihovu nezavisnost. Sljedeći korak je provjeriti normalnost podataka koju najčešće provjeravamo: histgoramom, qq-plotom te KS-testom (kojim provjeravamo pripadnost podataka distribuciji).




Histogrami upućuju na normalnost podataka. Normalnost možemo još provjeriti i qqplot-ovima ili testom koji ispituje normalnost.

```{r}

qqnorm(houses1story$GrLivArea, pch = 1, frame = FALSE,main='Houses with 1')
qqline(houses1story$GrLivArea, col = "steelblue", lwd = 2)

qqnorm(houses2story$GrLivArea, pch = 1, frame = FALSE,main='Houses with 2')
qqline(houses2story$GrLivArea, col = "steelblue", lwd = 2)

```

Pod uvjetom da podatci zadovoljavaju sve pretpostavke možemo 
nastaviti sa t-testom kako bi ispitali da li su hrvatski igrači viši od španjolskih.

Koji test koristiti? Kakve su varijance danih uzoraka?

```{r}
var(houses1story$GrLivArea)
var(houses2story$GrLivArea)
```

Jesu li varijance značajno različite?

### Test o jednakosti varijanci

U programskom paketu R test o jednakosti varijanci je implementiran u funkciji `var.test()`, koja prima uzorke iz dvije populacije čije varijance uspoređujemo.

Dakle, ispitajmo jednakost varijanci naših danih uzoraka.

```{r}
var.test(houses2story$GrLivArea,houses1story$GrLivArea)

```

p-vrijednost od  nam govori da nećemo odbaciti hipotezu $H_0$ da su varijance naša dva uzorka jednaka.

Provedimo sada t-test uz pretpostavku jednakosti varijanci.

```{r}
# Bitan je poredak kojim funkciji 't.test()' prosljedjujemo uzorke!
t.test(houses2story$GrLivArea,houses1story$GrLivArea, alt = "greater", var.equal = TRUE)
```

Zbog jako male p-vrijednost možemo odbaciti $H_0$ hipotezu o jednakosti prosječnih vrijednosti u korist $H_1$, odnosno možemo reći da su nekretnine s 2 kata u prosjeku vece od onih s jednim.



#
Programski paket R nudi široku podršku za rad s kategorijskim podatcima - 
  od factor tipa podataka, do raznih statističkih alata i 
testova za analizu kategorijskih varijabli. Kod analize factor tipa podataka, 
moramo paziti koje su moguće vrijednosti (`levels()`) naše varijable, 
a koje se stvarno pojavljuju u podatcima.


U ovom slučaju vidimo da se sve moguće vrijednosti pojavljuju u podatcima.

Sada možemo združiti podatke ovisno da li je njihova pozicija desno, lijevo ili na centru terena. Pozicije igrača imaju raspodjelu kao na slici.




# ZDRZIVANJE
for (column_name in c("LotShape")){
  data$LotShape[data$LotShape == column_name] = "IR1";
}

for (column_name in c("LotShape")){
  data$LotShape[data$LotShape == column_name] = "IR2";
}
for (column_name in c("LotShape")){
  data$LotShape[data$LotShape == column_name] = "IR3";
}

for (column_name in c("LotShape")){
  data$LotShape[data$LotShape == column_name] = "Reg";
}



Kontingencijsku tablicu jedne kategorijske varijable moguće je dobiti pozivanjem funkcije `table()`.

```{r}
tbl = table(data$LotShape)
print(tbl)
```

Izbacimo poziciju vratara i zamjenskih igrača i pogledajmo kontingencijsku tablicu varijabli pozicije i preferirane noge za udarce.

```{r}
tbl = table(data[data$LotShape == "IR1" | data$LotShape == "IR2" | data$LotShape == "IR3"| data$LotShape == "Reg",]$LotShape, 
            data[data$LotShape == "IR1" | data$LotShape == "IR2" | data$LotShape == "IR3"| data$LotShape == "Reg",]$HouseStyle)
tbl
```

Kontingencijskoj tablici možemo dodati i sume redaka i stupaca na sljedeći način.

```{r}
added_margins_tbl = addmargins(tbl)
print(added_margins_tbl)
```

Test nezavisnosti $\chi^2$ test u programskom paketu R implementiran je u funkciji `chisq.test()` koja kao ulaz prima kontingencijsku tablicu podataka koje testiramo na nezavisnost. Ispitajmo nezavisnost pozicije igrača na terenu i njegove preferirane noge za udarce.

Pretpostavka testa je da očekivana frekvencija pojedinog razreda mora biti veća ili jednaka 5 (`chisq.test()` pretpostavlja da je ovaj uvjet zadovoljen stoga je prije provođenja testa potrebno to provjeriti).

```{r}
for (col_names in colnames(added_margins_tbl)){
  for (row_names in rownames(added_margins_tbl)){
    if (!(row_names == 'Sum' | col_names == 'Sum') ){
      cat('Očekivane frekvencije za razred ',col_names,'-',row_names,': ',(added_margins_tbl[row_names,'Sum'] * added_margins_tbl['Sum',col_names]) / added_margins_tbl['Sum','Sum'],'\n')
    }
  }
}
```


Sve očekivane frekvencije nisu vece od 5. Ne Možemo nastaviti sa $\chi^2$ testom. Ali inace

```{r}
chisq.test(tbl,correct=F)
```


Odbacujemo H0 u korist H1 koja kaže da je pozicija igrača na terenu i njegova preferirana noga za udarce zavisna.

#oVISI LI VELICINA PODRUMA O KVARTU
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "Blmngtn";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "Blueste";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "BrDale";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "BrkSide";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "ClearCr";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "CollgCr";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "Crawfor";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "Edwards";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "Gilbert";
}
for (column_name in c("Neighborhood")){
  data$Neighborhood[data$Neighborhood == column_name] = "IDOTRR";
}

```{r}
tbl = table(data$Neighborhood)
print(tbl)
```
```{r}

tbl = table(data[data$Neighborhood == "Blmngtn" | data$Neighborhood == "Blueste" | 
                   data$Neighborhood == "BrDale"| data$Neighborhood == "BrkSide" | 
                 data$Neighborhood == "ClearCr" | data$Neighborhood == "CollgCr" | 
                   data$Neighborhood == "Crawfor"| data$Neighborhood == "Edwards"
                 | 
                   data$Neighborhood == "Gilbert"| data$Neighborhood == "IDOTRR",]$Neighborhood, 
            data[data$Neighborhood == "Blmngtn" | data$Neighborhood == "Blueste" | 
                   data$Neighborhood == "BrDale"| data$Neighborhood == "BrkSide" | 
                   data$Neighborhood == "ClearCr" | data$Neighborhood == "CollgCr" | 
                   data$Neighborhood == "Crawfor"| data$Neighborhood == "Edwards"
                 | 
                   data$Neighborhood == "Gilbert"| data$Neighborhood == "IDOTRR",]$TotalBsmtSF)
tbl
```

Kontingencijskoj tablici možemo dodati i sume redaka i stupaca na sljedeći način.

```{r}
added_margins_tbl = addmargins(tbl)
print(added_margins_tbl)
```

Test nezavisnosti $\chi^2$ test u programskom paketu R implementiran je u funkciji `chisq.test()` koja kao ulaz prima kontingencijsku tablicu podataka koje testiramo na nezavisnost. Ispitajmo nezavisnost pozicije igrača na terenu i njegove preferirane noge za udarce.

Pretpostavka testa je da očekivana frekvencija pojedinog razreda mora biti veća ili jednaka 5 (`chisq.test()` pretpostavlja da je ovaj uvjet zadovoljen stoga je prije provođenja testa potrebno to provjeriti).

```{r}
for (col_names in colnames(added_margins_tbl)){
  for (row_names in rownames(added_margins_tbl)){
    if (!(row_names == 'Sum' | col_names == 'Sum') ){
      cat('Očekivane frekvencije za razred ',col_names,'-',row_names,': ',(added_margins_tbl[row_names,'Sum'] * added_margins_tbl['Sum',col_names]) / added_margins_tbl['Sum','Sum'],'\n')
    }
  }
}



Sve očekivane frekvencije nisu vece od 5. Ne Možemo nastaviti sa $\chi^2$ testom. Ali inace

```{r}
chisq.test(tbl,correct=F)
```
## Linearna regresija

Linearna regresija korisna je u raznim istraživačkim i praktičnim situacijama, a daje odgovore na nekoliko bitnih pitanja:
  
  -   Postoji li veza između ulazne varijable (ili više ulaznih varijabli) - regresora, i izlazne varijable (reakcije)?
  -   Koliko je jaka ta veza?
  -   Koje ulazne varijable najviše utječu na izlaznu varijablu i koliko je jak taj efekt?
  -   Možemo li predvidjeti izlaz za neke nove vrijednosti ulaznih varijabli i s kojom točnošću?

data=read.csv('C://Users/Franjo/OneDrive/Dokumenti/SAP/preprocessed_data.csv')

summary(data)


Kako bi znali predvidjeti cijenu nekretnine, možemo ispitati različite varijable koje bi 
mogle utjecati na cijenu:
  
  -   velicina kuce
-   godina izgradnje
-   broj soba


Kad promatramo utjecaj samo jedne nezavisne varijable X na neku zavisnu varijablu Y, grafički je moguće dobiti jako dobar dojam o njihovom odnosu - tu je najčešće od pomoći scatter plot.

```{r scatter plots}

plot(data$GrLivArea,data$SalePrice) #kvadratura vs cijena

plot(data$BedroomAbvGr,data$SalePrice) #broj soba vs cijena

plot(data$YearBuilt,data$SalePrice) #

plot(data$GarageArea,data$SalePrice) #

plot(data$PoolArea,data$SalePrice) #


fit.livarea = lm(SalePrice~GrLivArea,data=data) #
fit.bedroom = lm(SalePrice~BedroomAbvGr,data=data)
fit.yearbuilt = lm(SalePrice~YearBuilt,data=data)
fit.garagearea = lm(SalePrice~GarageArea,data=data)

plot(data$GrLivArea,data$SalePrice) #graficki prikaz podataka
lines(data$GrLivArea,fit.livarea$SalePrice,col='red') #graficki prikaz procijenjenih vrijednosti iz modela
plot(data$BedroomAbvGr,data$SalePrice) #graficki prikaz podataka
lines(data$BedroomAbvGr,fit.bedroom$SalePrice,col='red') #graficki prikaz procijenjenih vrijednosti iz modela
plot(data$YearBuilt,data$SalePrice) #graficki prikaz podataka
lines(data$YearBuilt,fit.yearbuilt$SalePrice,col='red') #graficki prikaz procijenjenih vrijednosti iz modela
plot(data$GarageArea,data$SalePrice) #graficki prikaz podataka
lines(data$SalePrice,fit.garagearea$fitted.values,col='red') #graficki prikaz procijenjenih vrijednosti iz modela


```


### Normalnost reziduala i homogenost varijance

Normalnost reziduala moguće je provjeriti grafički, pomoću kvantil-kvantil plota (usporedbom s linijom normalne razdiobe), te statistički pomoću Kolmogorov-Smirnovljevog testa.

```{r res}

selected.model = fit.livarea

plot(selected.model$residuals) #gledajuci reziduale na ovaj nacin tesko je suditi o normalnosti

#histogram je vrlo interpretativan
hist((selected.model$residuals))
hist(rstandard(selected.model))

#q-q plot reziduala s linijom normalne distribucije
qqnorm(rstandard(selected.model))
qqline(rstandard(selected.model))

plot(selected.model$fitted.values,selected.model$residuals) #reziduale je dobro prikazati u ovisnosti o procjenama modela



#KS test na normalnost 
ks.test(rstandard(fit.garagearea),'pnorm')
install.packages("nortest")
library(nortest)
require(nortest)
lillie.test(rstandard(fit.livarea))

summary(fit.livarea)//najveci utjecaj

summary(fit.bedroom)/0.27

summary(fit.garagearea)/0.3882

summary(fit.yearbuilt)/0.27


cor(data$YearBuilt,data$SalePrice)

cor.test(data$YearBuilt,data$SalePrice)


## Višestruka regresija


fit.liv = lm(SalePrice ~ GrLivArea + BedroomAbvGr, data) #regresija s jako koreliranim varijablama
summary(fit.liv)

cor(data$GrLivArea,data$BedroomAbvGr)

```

Regresija s jako koreliranim ulaznim varijablama će uglavnom dati neke rezultate, 
ali na temelju njih ne možemo donositi nikakve zaključke. 
U slučaju savršene linearne zavisnosti ili koreliranosti 
ulaznih varijabli, procjena regresijskog modela će biti 
nestabilna i barem jedan koeficijent će biti NA.

Stoga je potrebo odabrati onaj podskup varijabli za koje smatramo da 
objašnjavaju različite efekte u podatcima i nisu međusobno (previše) korelirane.

```{r cor}

cor(cbind(bike.sharing.data$temp,bike.sharing.data$atemp,bike.sharing.data$hum,bike.sharing.data$windspeed)) # korelacijski koeficijenti parova regresora

```

```{r visestruka regresija}
fit.multi = lm(cnt ~ atemp + hum + windspeed, bike.sharing.data)
summary(fit.multi)


```

```{r residuali - visestruka regresija}
plot(selected.model$fitted.values,selected.model$residuals) #reziduali u ovisnosti o procjenama modela

plot(bike.sharing.data$dteday,selected.model$residuals) #reziduali u ovisnosti o datumu

#KS test na normalnost 
ks.test(rstandard(fit.windspeed),'pnorm')

#Lillieforsov test na normalnost
require(nortest)
lillie.test(rstandard(fit.windspeed))

```

Zašto su sad varijable hum i windspeed toliko "značajnije" nego kad ih koristimo same? 
  Često se dogodi i obrnut slučaj - uključivanjem dodatnih varijabli pojedine varijable mogu "prestati" biti značajne. U višestrukoj regresiji interakcije (korelacije) varijabli međusobno i sa zavisnom varijablom dolaze do izražaja - moguće su različite interpretacije. Može se tvrditi da je uključivanje varijable temp dodatno "očistilo" reziduale modela u kojem bi se koristio samo hum ili windspeed i time je dio varijance koji objašnjavaju ove dvije varijable došao do izražaja. U slučaju da su temp i hum ili windspeed objašnjavali iste efekte u podatcima, očekivali bismo da će uključivanje temp uzrokovati da hum ili windspeed "prestanu" biti značajni.

Ove interakcije su uzrok različitih fenomena u statistici, a jedan od poznatijih je i Simpsonov paradoks (<https://en.wikipedia.org/wiki/Simpson%27s_paradox>).

Model višestruke regresije koji smo ovako dobili objašnjava cca. $46\%$ varijance u podatcima - generalno je teško govoriti koliki je $R^2$ "dovoljan" za kakve podatke budući da to upravo najviše ovisi o samom području primjene - za razne društvene i ekonomske studije (bilo što vezano uz ljudsko ponašanje) će već $30\%$ biti zadovoljavajući rezultat, dok za neke fizikalne procese ni $80\%$ nije dovoljno dobar model. U konkretnom slučaju, budući da se ipak radi o nečem vezanom uz ljudsko ponašanje, ovaj rezultat se čini dobar, ali kao što se vidi u analizi reziduala (grafički prikaz u odnosu na izlaz modela i u odnosu na datum) - postoje još neki efekti u podatcima koji ovaj model ne uspjeva objasniti.

## Kategorijske nezavisne varijable

U skupu podataka raspolažemo s nekim kategorijskim varijablama, npr. season (godišnje doba), mnth (mjesec), holiday (indikator radi li se o prazniku taj dan), weekday (dan u tjednu), weathersit (vremenska situacija). Kategorijske varijable se mogu uključiti kao regresori u analizu, ali je prethodno potrebno provjeriti nekoliko stvari:
  
  -   radi li se o varijabli na nominalnoj ili ordinalnoj skali,
-   ima li varijabla linearan efekt na izlaznu varijablu,
-   predstavlja li određena kategorijska varijabla nešto što je određenom metričkom varijablom već predstavljeno.

U konkretnom slučaju, varijabla season je samo varijabla nešto grublje granulacije od varijable mnth, a za obje bismo očekivali da objašnjavaju sličan efekt u podatcima kao i varijabla temp. Varijable holiday i weathersit bi mogle biti korisne i zanimljive.

Korištenje kategorijskih varijabli s više od dvije kategorije kao int vrijednosti u regresiji se ne preporuča za nominalne varijable, iako u tom obliku mogu izgledati korisne u modelima.

```{r kategorijske ulazne varijable - sirove}

boxplot(cnt~season,data=bike.sharing.data) #kvadratni dijagram se moze koristiti za graficki provjeriti linearnost efekta kategorijske varijable na neku izlaznu varijablu

fit.multi.1 = lm(cnt ~ atemp + hum + windspeed + season, bike.sharing.data)
summary(fit.multi.1)

```

Rezultati upućuju na to da je ovako predstavljena varijabla season značajna u modelu, no model je vrlo vjerojatno samo uhvatio efekt vrlo malih vrijednosti izlaza za zimu (season = 1) i ne može objasniti efekt smanjenih vrijednosti za jesen (season = 4) u odnosu na proljeće i ljeto.

Za predstavljanje kategorijskih varijabli kao ulaz regresijskog modela postoje različite tehnike, a jedna od najjednostavnijih i najčešće korištenih su tzv. dummy varijable. Svaka kategorija u kategorijskoj varijabli predstavljena je svojom vlastitom indikatorskom varijablom koja poprima vrijednost 1 u slučaju da originalna kategorijska varijabla poprima vrijednost te kategorije, a 0 inače. Jednostavno generiranje dummy varijabli dostupno je u paketu fastDummies.

```{r kategorijske ulazne varijable - dummy varijable}

require(fastDummies)
bike.sharing.data.d = dummy_cols(bike.sharing.data,select_columns='season')

#procjena modela s dummy varijablama
fit.multi.d = lm(cnt ~ atemp + hum + windspeed + holiday + season_1 + season_2 + season_3, bike.sharing.data.d)
summary(fit.multi.d)

```

Dummy varijable će uvijek biti linearno zavisne ako ih sve koristimo u regresijskim modelima (objašnjenje: ako znamo da vrijednost kategorijske varijable nije ni jedna od 3 kategorije, onda sigurno znamo da je 4. kategorija) - stoga je uvijek potrebno isključiti jednu od dummy varijabli iz modela. Bez obzira na to koje varijable uključili, ukupni model će biti isti, ali samo zaključivanje o pojedinim dummy varijablama u slučajevima kad imamo više od dvije kategorije će biti nešto kompliciranije.

## Vremenski zavisne varijable

U nekim slučajevima znamo da izlazna varijabla ima i izraženu vremensku zavisnost - u konkretnom slučaju možemo biti sigurni da, ukoliko znamo današnji broj iznajmljenih bicikala, mala je vjerojatnost da će sutrašnji biti previše različit, čak i kad modeliramo efekte vremena, temperature itd. To je uostalom vidljivo i u grafičkim prikazima reziduala u ovisnosti u datumu za gore navedene modele.

Postoji jednostavan način na koji se neki od ovih vremenskih efekata mogu modelirati bez upotrebe složenijih modela - u regresiju se kao ulazna varijabla uključi vremenski pomaknuta izlazna varijabla koja označava "prošlost" koja je u trenutku modeliranje uglavnom poznata. Konkretno, uz sve navedene varijable, za modelirati varijablu cnt u trenutku $t$ možemo uključiti i samu varijablu cnt u trenutku $t-1$.

```{r vremenski pomak u zavisnoj varijabli}

#vremenski pomak varijable cnt
bike.sharing.data.d$lag.cnt = c(NA,bike.sharing.data.d$cnt[1:length(bike.sharing.data.d$cnt)-1])

#procjena modela s vremenski pomaknutom varijablom cnt na ulazu
fit.multi.d.timelag = lm(cnt ~ lag.cnt + atemp + hum + windspeed + holiday + season_1 + season_2 + season_3, bike.sharing.data.d)
summary(fit.multi.d.timelag)

plot(bike.sharing.data.d$dteday[2:length(bike.sharing.data.d$dteday)],fit.multi.d.timelag$residuals) #reziduali u ovisnosti o datumu


```

## Transformacije podataka, dodavanje interakcijskih članova

U nekim situacijama, u svrhu izgradnje boljeg modela poželjno je nad ulaznim ili izlaznim varijablama primjeniti transformacije, najčešće $f(x) = \log x$ ili $f(x) = e^x$. Također, moguće je u model regresije dodavati tzv. interakcijske članove ili kvadrate, kubove, ...itd. ulaznih varijabli, npr. $x_1^2$, $x_1x_2$, $x_2^2$.

U oba slučaja modifikacije se primjenjuju na temelju pretpostavki o prirodi interakcije i modelu. Na primjeru temperature, u jednom od prvih grafova se mogao vidjeti potencijalno nelinearan efekt temperature - na najvišim temperaturama broj iznajmljenih bicikala se ipak smanjivao (što ima smisla).

```{r transformacija}

# moguce je provjeriti gore navedenu tvrdnju prvo na primjeru samo temperature
fit.atemp.sq = lm(cnt ~ atemp + I(atemp^2),bike.sharing.data.d)
summary(fit.atemp.sq)

#jednostavan nacin za graficki prikazati nelinearne krivulje
f = function(x, coeffs)
  return(coeffs[[1]] + coeffs[[2]] * x + coeffs[[3]] * x^2)
plot(bike.sharing.data$atemp,bike.sharing.data$cnt) 
curve(f(x, fit.atemp.sq$coefficients), add = TRUE, col = "red")

```

Uključivanjem ovako transformiranih varijabli moguće je dodatno poboljšati ukupni model višestruke regresije.

```{r model s transformacijom}

#model regresije sa svim varijablama
fit.multi.d.timelag.sq = lm(cnt ~ lag.cnt + atemp + I(atemp^2) + hum + windspeed + holiday + season_1 + season_2 + season_3, bike.sharing.data.d)
summary(fit.multi.d.timelag.sq)

```

## Odabir parametara modela

U odabiru konačnog modela koji biste preporučili tvrtci za iznajmljivanje bicikala, potrebno je voditi se i principom jednostavnosti - jednostavniji model je uglavnom preferiraniji ukoliko je jednako dobar kao i neki alternativni složeniji model. Budući da će modeli s više varijabli u pravilu uvijek objašnjavati veći udio varijance od modela s manjim podskupom istih varijabli, nije moguće usporediti modela s različitim brojem varijabli gledajući samo njihove greške.

Pri odabiru modela u odnosu za velik broj razmatranih varijabli moguće je koristiti različite tehnike (tzv. model selection) koje nisu dio ovog kolegija. No, kao jedan od jednostavnijih alata za usporedbu modela različitih broja parametara moguće je koristiti i prilagođeni koeficijent determinacije $R_{adj}^2$, koji penalizira dodatne parametre u modelu.

U ovom slučaju, varijabla holiday potencijalno nije toliko korisna u modelu i možda se može izbaciti.

```{r smanjenje broja parametara}

#model s varijablom holiday
fit.multi.d.timelag.sq = lm(cnt ~ lag.cnt + atemp + I(atemp^2) + hum + windspeed + holiday + +season_1 + season_2 + season_3, bike.sharing.data.d)
summary(fit.multi.d.timelag.sq)

#model bez varijable holiday
fit.multi.d.timelag.sq.final = lm(cnt ~ lag.cnt + atemp + I(atemp^2) + hum + windspeed + season_1 + season_2 + season_3, bike.sharing.data.d)
summary(fit.multi.d.timelag.sq.final)


```

Rezultati upućuju na to da varijabla holiday ipak daje određenu korisnu informaciju u modelu, čak i kad koristimo $R_{adj}^2$.

Druga često korištena metoda je jednostavno izbacivanje onih regresora koji nemaju značajne koeficijente - no zbog interakcija među regresorima u multivarijatnoj regresiji to nije uvijek pouzdana metoda. Također, u slučaju jako velikog broja varijabli se mogu javiti i problemi ponovoljenih usporedbi (višestrukog testiranja).

## Zaključak

Konačan model sadržava relevantne varijable koje objašnjavaju čak preko $80\%$ varijance broja iznajmljenih bicikala dnevno. Osim metričkih varijabli temperature zraka, vlažnosti i vjetra, uključen je i kvadrat temperature zraka (zbog nelinearnog efekta), kategorijska varijabla koja ukazuje na praznike, dummy varijable za kategoriju vremenske situacije, te prethodna ("jučerašnja") vrijednost broja iznajmljenih bicikala.

Sve navedene varijable osim holiday su značajne na razini 0.01, kao i sam model, na što upućuju rezultati t-testova pojedinih koeficijenata i F-testa čitavog modela.



Odbacujemo H0 u korist H1 koja kaže da je pozicija igrača na terenu i njegova preferirana noga za udarce zavisna.




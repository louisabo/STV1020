##########################
##                      ##
##  STV1020 Vår 2021    ##
##  Script R-seminar 4  ##
##  Navn                ##
##                      ##
##########################

#### Laste inn data ####

# Setter working directory
getwd()
setwd()

# Installerer relevante pakker
install.packages("")
library()

# Late inn datasett fra https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv 
data <- read.csv("")

# Lagre datasett til en ny fil .rda
save(data, file = ".rda")

load("")

# Inspiserer datasettet
View()
head()
tail() 
summary() 
range()

#### Missing - NA - NOT AVAILBLE ####

# Finne antall missing i hele datasettet
sum(is.na(data)) 

# Finne antall missing på bestemte variabler
sum(is.na(data$variabel1))
sum(is.na(data$variabel2))

# Summary gir også info om NAs
summary()

# Håndtere missing: Sjekk ut hjelpefilen for NA.
?NA

# Håndtere missing: Ulike typer og bruk av subset fra tidyverse
data %>% data
  drop_na(variabel3)
  
# Sjekker at det ble riktig
sum(is.na(data$variabel3))

#### Statistiske mål ####

# Rask oversikt over viktige statistiske mål
summary()

# Gjennomsnitt
mean()
# Husk på missing!

# Modus/typetall
mode()

# Forsøk å ta gjennomsnitt med en dikotom variabel/variabel på nominalnivå
mean(data$kjonn, 
     na.rm = TRUE) 
# Diskutér 

# Standardavvik, gjennomsnittlig avstand fra gjennomsnittet 
sd()
# Husk på missing
       
# Varians, standardavviket ^2 (lagrer i eget objekt)
varians <- var(data$variabel4)
sqrt(varians) 

#### Univariat analyse ####

# Rask oversikt igjen
summary(data)
       
# tibble() og str() finner ut av hva slags variabler det er i datasettet. Vi må installere tibble-pakken 
# og hente den i biblioteket
install.packages("")
library()

tibble()
str()
    
## Kategoriske variabler ##

# Frekvenstabeller for kategoriske variabler 
table(data$variabel5)

# Lagrer tabellen i et objekt, og deretter gjør det om til df
tabell <- table(data$kjonn)
tabell1 <- data.frame(tabell)

# Bruker stargazer-pakken til å eksportere tabellen
stargazer(tabell1, 
          type = "text" eller "html", 
          out = "") # .text eller .html

# Relativ fordeling
tabell2 <- prop.table(table(data$variabel5))
tabell2

# Søylediagram for grafisk beskrivelse av tabellene, bruker ggplot 
install.packages("ggplot")
library(ggplot2)

ggplot(data, aes(internettbruk)) + 
  geom_bar(width = 1)

## Kontinuerlige variabler ##

# Frekvenstabeller for kontinuerlige variabler. Må omkodes til kategorier. 
omkodet_alder <- cut(data$alder, 
                            breaks = c(16, 30, 45, 60, 75, 90)) # 16-30
                                                                # 31-45 
                                                                # 46-60 
                                                                # 61-75
                                                                # 76-90
table()

# Histogrammer
ggplot(data, aes(alder)) + 
  geom_histogram(bins = 20, 
                 color = "white", 
                 fill = "grey")   
# Vi kan også legge til flere argumenter. Bruk hjelpefilen

# Eksportere tabeller med deskriptiv statistikk til Word
stargazer(data, 
          type = "text")
       
#### Bivariat analyse #### 
       
## To kateogriske variabler ##

# Krysstabell
krysstabell <- table(data$variabel6, data$variabel7)
krysstabell
# Tolk tabellen

# Krystabell i relative tall 
prop.table(krysstabell, margin = )

# Kjikvadrattesten
chisq.test(krysstabell)
# X-squared
       
# Søylediagrammer
ggplot(data, aes(x = internettbruk , 
                 fill = as.factor(kjonn))) + 
  geom_bar(position = "dodge") + #eller "fill"
  labs(fill = "kjonn")

## To kontinuerlige variabler ## 
         
# Pearsons r for alder og utdanning.
R <- cor(x = data$alder, 
         y = data$utdanning, 
         use = "pairwise.complete.obs", 
         method = "pearson")
R
# Hva forteller dette oss?

# Korrelasjonsmatrise
cor(data, 
    use = "pairwise.complete.obs")
       
# Spredningsdiagram (kan legge til flere argumenter for å få et mer "fancy" spredningsdiagram. Sjekk hjelpefilen)
ggplot(data, aes(alder, utdanning)) + 
  geom_point() + 
  labs(title = "")

# Spredningsdiagram med linje 
ggplot(data, aes(alder, utdanning)) + 
  geom_smooth(method = "lm") + 
  labs(title = "")

# Spredningsdiagram med linje og punktestimater
ggplot(data, aes(alder, utdanning)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "")


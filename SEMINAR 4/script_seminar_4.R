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

setwd("~/Desktop")
# Installerer relevante pakker
# install.packages("")

# Har du dem allerede bare last dem inn 
library(tidyverse)
# install.packages("stargazer")
library(stargazer)

# Late inn datasett fra  
# https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv


data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")

# Lagre datasett til en ny fil .rda
save(data, file = "")

rm(data)

load("interdata_louisa.rda")

# Inspiserer datasettet
View(data)
head(data)
tail(data) 
summary(data$internettbruk) 

range(data$alder)



#### Missing - NA - NOT AVAILBLE ####

# Finne antall missing i hele datasettet
sum(is.na(data)) 

# Finne antall missing på bestemte variabler
sum(is.na(data$internettbruk))
sum(is.na(data$variabel2))

# Summary gir også info om NAs


# Håndtere missing: Sjekk ut hjelpefilen for NA.
?NA


# Håndtere missing: Ulike typer og bruk av subset fra tidyverse
data <- data %>%
drop_na()
summary(data)

# Sjekker at det ble riktig
sum(is.na(data$variabel3))

#### Statistiske mål ####

# Dere må kunne 
# SD, typetall og mean 

# Rask oversikt over viktige statistiske mål
summary()

# Gjennomsnitt
mean(data$alder)
# Husk på missing!

# Modus/typetall
mode()

# Forsøk å ta gjennomsnitt med en dikotom variabel/variabel på nominalnivå
mean(data$kjonn, 
     na.rm = TRUE) 
# Standardavvik, gjennomsnittlig avstand fra gjennomsnittet 
sd()

# Varians, standardavviket -- kvadratrot (lagrer i eget objekt)
varians <- var(data$internettbruk)
varians # Måler spredning men ikke kvadrert 
sqrt(varians) # samme som SD 
sd(data$internettbruk)

mean(data$internettbruk)

#### Univariat analyse ####

# Rask oversikt igjen
summary(data)

#### NUMERIC ELLER INTEGER
####  Forholdstalls nivå -- kontinuerlig med nullpunkt 
####  Intervallnivå -- tilfeldig nullpunkt som temperatur eller ¨

#### FACTOR ELLER CHARACHTER 
####  Ordinalnivå -- CHARACHTER ELLER FACTOR
####  Nominalnivå -- CHARACHTER 
str(data)

# DU KAN ENDRE KLASSE 

data$kjonn_1 <- as.factor(data$kjonn)

# Frekvenstabeller for kategoriske variabler 
table(data$kjonn)

# Lagrer tabellen i et objekt, og deretter gjør det om til df
tabell <- table(data$alder)
tabell
tabell1 <- data.frame(tabell)
tabell1

# Bruker stargazer-pakken til å eksportere tabellen

stargazer(tabell1, 
          type = "text") 

# Relativ fordeling
tabell2 <- prop.table(table(data$internett))
tabell2


# Eksportere tabeller med deskriptiv statistikk til Word
summary(data)

stargazer(data, 
          type = "text")



#### Bivariat analyse #### 

## To kateogriske variabler ##

# Krysstabell
krysstabell <- table(data$kjonn, data$tillit)
krysstabell
# Tolk tabellen


# Kjikvadrattesten
# Når p-verdien < 0.05 er det signifikant smh. (mindre)
# Når p-verdien > 0.05 er det ikke signifikant.(større)
chisq.test(krysstabell)
# X-squared

# Krystabell i relative tall 
prop.table(krysstabell, margin =1 )

## To kontinuerlige variabler ## 

# Pearsons r for alder og utdanning 
# og alder og internettbruk
R <- cor(x = data$alder, 
         y = data$internettbruk, 
         use = "pairwise.complete.obs",
         method = "pearson")
R

# DU KAN VISUALISERE SAMMENHENGEN VHA GGPLOT
ggplot(data, aes(x = alder, y= internettbruk)) + 
  geom_smooth(method = lm) + 
  labs(title = "")


# Korrelasjonsmatrise
cor(data, 
    use = "pairwise.complete.obs", 
    method = "pearson")


### EXTRA OPPGAVER 


## Lag plott med rett linje som beskriver sammenhengen mellom 
## følgende variable -- mål også korrelasjonen med pearson R
# 1) Utdanning og internettbruk 
# 2) Alder og utdanning 
# 3) Utdanning og tillit 
# 4) Tillit og alder 
# 5) Internettbruk og tillit 
# Hvilken vei peker linjen når det er negativ smh og hvilken vei peker pilen
# når det er positiv smh?


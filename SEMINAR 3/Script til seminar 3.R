
#### ---------------------- Seminar 3 ------------------------ ####

#### Tema for seminar 
#### 1. Repetisjon: mappestruktur
#### 2. Repetisjon: laste inn data
#### 3. Omkoding av variabler 
#### 4. Subsetting av datasett 
#### 5. Plotting med ggplot 

#### 1. Repetisjon av mappestruktur ####
## Først fjerner vi alt som er i environment 
rm(list = ls())

#### 2. Laste inn data ####

# Installerer pakken 
install.packages("gapminder")

# Så åpner vi pakken 
library(gapminder) 

# Opprett et objekt av datasettet 
gapminder <- gapminder

# Sett working directory (endre koden til din mappestruktur)
setwd("c:/Users/ingvi/OneDrive/Seminarleder/STV1020 V21 R/Seminar 3/")

# Lagre data i R format 
save(gapminder, file = "gapminder.Rdata")

# Last inn det lagrede datasettet 
rm(gapminder) # først fjerner jeg det eksisterende datasettet
load("gapminder.Rdata")


#### 3. Omkoding av variabler #####
# Få oversikt over data 
show(variable.names(gapminder))
head(gapminder)
summary(gapminder)

# Matematisk omkoding
summary(gapminder$year)
gapminder$year_1952 <- gapminder$year - 1952
summary(gapminder$year_1952)

# Se om omkodingen er gjort riktig 
table(gapminder$year_1952, gapminder$year)
table((gapminder$year_1952 + 1952) == gapminder$year)

# Snu skalaen
gapminder$year_2007 <- gapminder$year_1952*(-1) + 55
table(gapminder$year_1952, gapminder$year_2007)

## Omkoding med ifelse() 
# year
gapminder$year_0 <- ifelse(gapminder$year == 1952, 0, gapminder$year)

table(gapminder$year_0)
table(gapminder$year_0, gapminder$year)

# lifeExp
summary(gapminder$lifeExp)

gapminder$lifeExp_2 <- ifelse( gapminder$lifeExp < 60.71, 1, gapminder$lifeExp)
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp < 60.71, 1, NA)
gapminder$lifeExp_2 <- ifelse(gapminder$lifeExp < 60.71, 1,
                              ifelse(gapminder$lifeExp > 60.71, 2, gapminder$lifeExp))

table(gapminder$lifeExp_2)

# Omkode til dummyvariabel med ifelse()
table(gapminder$continent)

gapminder$continent_dum <- ifelse(gapminder$continent == "Asia", 1, 0)

table(gapminder$continent_dum, gapminder$continent)


#### 4 Subsetting av datasett #####
# Installer og åpne pakke 
install.packages("dplyr")
library(dplyr)

# Velg variabler 
gapminder %>% 
  select(pop)

gapminder %>% 
  select(pop, country, year)

# Filtrer enheter
gapminder %>% 
  filter(year == 1952)

mean(gapminder$pop) # finn gjennomsnittet av variabelen pop

gapminder %>% 
  filter(year == 1952, 
         pop > 29601212)

gapminder %>% 
  filter(year == 1952, 
         pop > mean(pop))

# Subsett data 
gapminder_2 <- gapminder %>% 
  select(pop, country, year)

gapminder_2 <- gapminder %>% 
  select(pop, country, year) %>%
  filter(year == 1952, 
         pop > mean(pop))


#### 5 Plotting med ggplot ####

# Vi skal bruke pakken ggplot2: 
install.packages("ggplot2")
library(ggplot2)

# Alternativt laste inn tidyverse: 
install.packages("tidyverse")
library(tidyverse)

## Vi bygger plottene lag for lag: 

# Først, fortelle hvilke data vi vil bruke:
ggplot(data = gapminder)

# Fortell ggplot hva vi vil vise på x-aksen: 
ggplot(data=gapminder, aes(x=continent))

# Fortelle hva slags plott vi vil lage:
ggplot(data = gapminder, aes(x=continent)) + geom_bar()

# Legge inn "fill" i aesthetics for å skille kontinentene med ulike farger:
ggplot(gapminder, aes(x=continent, fill=continent)) + geom_bar()

# Endre teksten til x- og y-aksen, og gi plottet en tittel:
ggplot(gapminder, aes(x=continent, fill=continent)) + geom_bar() +
  labs(x = "Kontinenter", y = "Antall land/år")


#### 6 Ulike typer plott ####

## 1. Histogram over forventet levealder
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram()

# Vi setter binwidth til 1 (ett år per stolpe)
ggplot(gapminder, aes(lifeExp)) +
  geom_histogram(binwidth = 1)

# Vi kan hente ut enda mer info ved å legge inn fill: 
ggplot(gapminder, aes(lifeExp, fill=continent)) +
  geom_histogram(binwidth = 1)

## 2. Box plot over forventet levealder per kontinent:
ggplot(data=gapminder, aes(x=continent, y=lifeExp, fill=continent)) + 
  geom_boxplot()

## 3. Density plot/tetthetsplott

# Tetthetsplott/density plott for samme variabel: 
ggplot(data=gapminder, aes(x=lifeExp)) + 
geom_density()

# Legge til spesifikasjoner om tykkelse, farge osv. i geom_density: 
ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density(size=1.5, fill="pink", alpha=0.3)

# Hva skjer hvis vi endrer alpha til 1 og size til 0.5?

ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density(size=0.5, fill="pink", alpha=1)

# Separerer plottene ved bruk av facet_wrap: 
ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density(size=0.5, fill="pink", alpha=1) + 
  facet_wrap(vars(continent))


## 4. Scatterplot over to kontinuerlige variabler

# Vi legger inn informasjon om både x- og y-aksen, samt geom_point: 
ggplot(data=gapminder, 
       mapping= aes(x=lifeExp, 
                    y=gdpPercap)) + geom_point()

# Legger til linje som viser gjennomsnittet i observasjonene: 
ggplot(data=gapminder, mapping= aes(x=lifeExp, y=gdpPercap)) +
  geom_point() + geom_smooth()

# Skiller kontinentene fra hverandre med farger:
ggplot(data=gapminder, mapping = aes(x=lifeExp, y=gdpPercap, col=continent)) +
  geom_point() + geom_smooth() 

# Facet wrap for å skille plottene, svart farge på linja:
ggplot(data=gapminder, aes(x=lifeExp, y=gdpPercap, col= continent)) +
  geom_point() + geom_smooth(colour="black") + 
  facet_wrap(vars(continent)) 

# Ny tekst til x- og y-aksen, samt tittel
ggplot(data=gapminder, mapping= aes(x=lifeExp, y=gdpPercap, col=continent)) +
  geom_point() + geom_smooth(colour="black") + facet_wrap(vars(continent)) +
  labs(x = "Forventet levealder", 
       y = "GDP per capita", 
       title = "Et plot med Gapminderdata") 


#### 7 Lagring av plott####

# Lage objekter av plott: 
gm_h <- ggplot(gapminder, aes(lifeExp, fill=continent)) +
  geom_histogram(binwidth = 1)

# Bygge videre på plott:
gm_h + labs(title = "Et plot med Gapminderdata")
  
# Bruk ggsave, gi plottet et navn, og spesifiser formatet du vil lagre i. 
# Det blir lagra i ditt working directory. 
ggsave(filename = "gdplevealder.png")

# eller som pdf
ggsave(filename = "gdplevealder.pdf")

# Du trenger egentlig ikke skrive "filename", så lenge du bruker ggsave. 
ggsave("gdplevealder.png")



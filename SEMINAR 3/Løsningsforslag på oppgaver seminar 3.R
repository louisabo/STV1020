rm(list = ls())

#### --------------------- Fasit på oppgaver til seminar 3 ---------------------- ####

#### Oppgave 1####
setwd("")
load("gapminder.Rdata")


#### Oppgave 2 ####
gapminder$continent_5 <- ifelse(gapminder$continent == "Asia", 1, 
                                ifelse(gapminder$continent == "Americas", 2, 
                                       ifelse(gapminder$continent == "Oceania", 3,
                                              ifelse(gapminder$continent == "Europe", 4,
                                                     ifelse(gapminder$continent == "Africa", 5, NA)))))

table(gapminder$continent, gapminder$continent_5)


#### Oppgave 3 ####
gapminder$lifeExp_1000 <- gapminder$lifeExp * 1000

summary(gapminder$lifeExp)
summary(gapminder$lifeExp_1000)

table((gapminder$lifeExp_1000 / 1000) == gapminder$lifeExp)


#### Oppgave 4 ####
library(dplyr)
gapminder_2 <- gapminder %>% 
  filter(year == 1977) %>%
  select(pop, country, lifeExp) 

#### Oppgave 5 ####
gapminder_3 <- gapminder %>% 
  filter(continent == "Europe",
         lifeExp >= 75) # Her sier jeg at veriden skal være større > eller den samme = som 75. 



#### Oppgave 6 ####
#Dataene i Gapminder er hentet inn hvert femte år. Vi er interessert i å vite 
#om det er like mange observasjoner for hvert av årene. Lag et søylediagram for å undersøke dette. 

ggplot(gapminder, aes(x=year)) + geom_bar()

#### Oppgave 7 ####
#a) Lag et histogram for variabelen GDP per capita (gdpPercap). 
#Sett bredden på hvert intervall til 100. 

ggplot(gapminder, aes(x=gdpPercap)) + geom_histogram(binwidth=100)

#b) Lag et densityplott for den samme variabelen. Gjør linja rød, og sett tykkelsen til 2.0. 
ggplot(gapminder, aes(x=gdpPercap)) + geom_density(size=2.0, col = "red")

#### Oppgave 8 ####
# a) Lag et scatterplott over forventet levealder (lifeExp) og befolkningstall (pop).

ggplot(gapminder, aes(x=lifeExp, y=pop)) + geom_point()

# b) Del opp observasjonene i ulike farger for hvert kontinent.

ggplot(gapminder, aes(x=lifeExp, y=pop, col=continent)) + geom_point()

#c) Kan du bruke facet_wrap for å gjøre plottet mer oversiktig? 

ggplot(gapminder, aes(x=lifeExp, y=pop, col=continent)) + 
  geom_point() + facet_wrap(vars(continent))

#### Oppgave 9 ####
#a) Plott en graf (ei linje) over hvordan forventet levealder har endret seg over tid.
ggplot(data=gapminder, aes(x=year, y=lifeExp)) + geom_smooth()

#b) Hvordan ser denne trenden ut på ulike kontinenter? Lag ei linje for hvert kontinent.
ggplot(data=gapminder, aes(x=year, y=lifeExp, col=continent)) + geom_smooth()

#### Oppgave 10 ####
#Legg til en tittel på plottet fra forrige oppgave, samt nye labels til x- og y-aksen.
#Lagre plottet ditt som et objekt i environment, og lagre det deretter på pcen din, både 
#som png- og pdf-fil. 

#Gir plottet tittel
ggplot(data=gapminder, 
       aes(x=year, y=lifeExp, col=continent)) + 
  geom_smooth() + 
  labs(x="År", y="Forventet levealder", title= "Forventet levealder fra 1952 til 2007")

#Oppretter objekt
levealder <- ggplot(data=gapminder, 
                    aes(x=year, y=lifeExp, col=continent)) + geom_smooth() + 
  labs(x="År", y="Forventet levealder", title= "Forventet levealder fra 1952 til 2007")

#Lagrer som png
ggsave("levealderplott.png", plot = levealder)

#Lagrer som pdf
ggsave("levealderplott.pdf", plot = levealder)


#### Ekstraoppgaver ####
# Oppgave 1 
gapminder_africa <- gapminder %>% 
  filter(year == 1952,
         continent == "Africa") 

gapminder_africa_2 <- gapminder_africa %>%
  filter(lifeExp > mean(lifeExp))

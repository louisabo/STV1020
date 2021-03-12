## VELKOMMEN TIL R SEMINAR 2 ## 

# I DAG SKAL VI GJENNOMGÅ FØLGENDE 
# Men først en liten gjennomgang av fasiten 
# Fra oppgavene jeg håper alle har gjort! 

# 0. Working directory og datasett

setwd("~/Desktop/STV1020/SEMINAR 2")


# 1. Pakker

install.packages("foreign")
library(foreign)

library(tidyverse) # Denne pakken gjør livet lettere


# 2. Laste inn data 

# Ulike pakker til dette 

# f.eks excel 
# f.eks csv 

# Legg merke til koden som brukes 
# Og hvilken pakke den kommer fra

df <- read.dta("ESS9NO.dta")

# 3. Organisering av arbeidet 

df <- df%>% 
  rename(news = nwspol, 
         interest = polintr, 
         age = yrbrn)
  
table(df$age) # Året respondenten er født i 

# Vi lager en variabel som er ren alder
# Dataene er samlet inn i 2018 så derfor tar vi - 2018
df$age <- 2018-df$age 

table(df$age)

# Se på antall variabler: Her er det mye å leke med. 
# Vi velger ut de vi skal bruke 

df <- df %>% select(news, interest, vote, age)

# 4. Målenivå

# Nominal 
# Ordinal 
# Intervall 
# Forholdstall 

# 5. Klasser og målenivå 
# Litt annerledes i R 
# Bruk summary eller str eventuelt se levels
# Eventuelt class
# Du kan også omkode klasse 

# 6. Utforske data
# Vis filter 
# repeter head/tail 
# View 
# Range 
# Unique
# Her er også summary relevant 

# 7. Plotting

# Hvordan kan vi visualisere hvordan fordelingen av politisk interesse er?
# Her kan vi bruke geom_bar for å lage et histogram.

ggplot(data = df, aes(x = interest)) + geom_bar()


# Hvor mange innenfor hvert nivå av politisk interesse stemte?

ggplot(data = df, aes(x = interest)) + geom_bar(aes(fill=vote), 
                                                position = "dodge") 

# "dodge" forteller at du vil at "vote"-søylene skal være ved siden av hverandre. 


# Hvordan fordeler respondentenes alder og tiden de bruker på nyheter seg?

ggplot(data = df, aes(x = news)) +
  geom_histogram(bins = 5) # bins sier hvor mange søyler vi skal ha

ggplot(data = df, aes(x = age)) +
  geom_histogram(binwidth = 10) # bindwidth sier hvor stor hver søyle skal være


# Hvordan fordeler tiden man bruker på nyheter på alder?
# Her kan vi bruke geom_point for å lage et spredningsplott.

ggplot(data = df, aes(x = age, y = news)) +
  geom_point(alpha = 0.2) 

# Alpha gjør punktene gjennomsiktige, så jo mer solid et punkt er, jo flere 
# enheter er på dette punktet.


# Hvordan fordeler alder seg på interesse? Vi kan lage et boksplott med 
# geom_boxplot.

ggplot(data = df, aes(x = interest, y = age)) +
  geom_boxplot() 



# Hvis dere vil utforske hvordan man kan tilpasse de ulike diagrammene vi har
# sett på og mange andre, kan denne siden være nyttig: 
# https://www.r-graph-gallery.com/index.html


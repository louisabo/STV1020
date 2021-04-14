
##########################
####                  ####
####     STV1020      ####
####                  ####
#### Løsningsforslag  ####
####    Seminar 6     ####
####                  ####
##########################

setwd("C:/Users/carol/Documents/R/STV1020")

# Laster inn nødvendige pakker
library(tidyverse)
library(stargazer)
library(haven)
library(ggplot2)



####   OPPGAVE 1   ####

# Laster inn datasettet og lagrer det som et objekt ved navn "dta". 
dta <- read_dta("ESS9NO.dta")



####   OPPGAVE 2   ####

dta2 <- dta %>%
  select(gndr, agea, eduyrs, grspnum) %>%
  rename(alder = agea, utdanning = eduyrs, inntekt = grspnum) %>%
  mutate(kjønn = if_else(gndr == 1, 0, 1))

# Oppretter et nytt objekt ved navn "dta2".
# Vi binder sammen funksjoner med dette tegnet %>%
# Relevante variabler inkluderes ved hjelp av "select" funksjonen.
# Variabelnavn endres ved hjelp av "rename" funksjonen. 
# Variabelen "gndr" omkodes ved hjelp av "mutate" og "if_else" funksjonen.



####   OPPGAVE 3   ####

## Fjerner enheter som har missing. 
dta2 <- na.omit(dta2)

# Dersom vi fjernet enheter med missing fra det opprinnelige datasettet (dta)
# ville vi stått igjen med 0 observasjoner, fordi alle enhetene mangler 
# observasjoner på en eller flere variabler.  



####   OPPGAVE 4   ####

# Ettersom inntekt består av mange siffer, vil R ofte endre notasjonen.
# Dette er en kode man kan kjøre for å få numeriske verdier i regresjonstabellen. 
options(scipen =999)

# Multippel regresjon
mod1 <- lm(inntekt ~ utdanning +
             alder+
             kjønn,
           data = dta2)

# Presenterer resultatene i en fin tabell ved hjelp av stargazer
stargazer(mod1, 
          type = "text", # Indikerer at vi ønsker outputet i tekst-format. 
          digits = 2, # Spesifiserer antall desimaler
          title = "OLS regresjon", # Tittel
          dep.var.labels = "Inntekt", # Navn på AV
          covariate.labels = c("Utdanning","Alder", "Kjønn")) # Navn på UV

# Eksempel på tolkning: 
# Sammenhengen mellom alder og inntekt er statistisk signifikant (p<0.01) 
# og positiv. Det vil si at for hver skalaenhets økning i alder, vil inntekt 
# øke med 6195 kroner, gitt at de andre variablene holdes konstant.   



####   OPPGAVE 5   ####

# Plotter egresjonslinjen til utdannings-koeffisienten.
ggplot(dta2, # Spesifiserer datasetet vi jobber i.
       aes(x = utdanning, # Den uavhengige variablenen vi er interessert i.
           y = inntekt)) + # Avhengig variabel.
  # ylim(0, 3000000)+ # Man kan definere den øvre og nedre grensen til AV hvis man ønsker.
  geom_point() +  # Enhetene som punkter
  stat_smooth(method = "lm", col = "red") # En lineær regresjonslinje.

# Plottet illustrerer at flertallet av respondentene tjener mellom 0 og 2.5 million.
# Imidlertid er det et par uteliggere med svært stor residual, noe som potensielt
# kan påvirke styrken og/eller retningen på parameterestimatene.



####   OPPGAVE 6   ####

# Legger residual-variabelen til dta2. 
dta2$resid1 <- mod1$residuals

# Residual-fordelingen: Tetthetsplot
ggplot(dta2,
       aes(x = resid1)) +
  geom_density() + theme_classic()

# Residual-fordelingen er svakt høyreskjev, og noe spissere enn normalfordelingen. 

# Alternativt: Histogram
ggplot(dta2, aes(x = resid1)) + 
  geom_histogram(binwidth = 10000, 
                 color = "blue")



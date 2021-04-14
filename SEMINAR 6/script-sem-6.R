
###########Seminar 6 STV1020#######
########Multippel regresjon########

library(tidyverse)
library(stargazer)
library(haven)

##Pdf dokumentet inneholder enda mer informasjon om de ulike kodene
##Husk også å bruke hjelpefilene flittig dersom dere lurer på hvordan ulike funksjoner brukes



########Preprossesering##############

####Laster inn datasettet. Bruker funksjonen read_dta fra haven pakken.
ESS8SE <- read_dta("ESS8SE.dta")



###Vi går igjennom datasettet vårt og ser hva slags informasjon vi har 
head(ESS8SE)



##Vi har mange variabler som ikke gir substansiell mening

ESS8SE$prtvtdes #Hva sier denne variabelen?


##Vi subsetter datasettet vårt, tar kun med variablene vi trenger til regresjonen

ess <- ESS8SE %>% 
  select(gndr, agea, eduyrs, nwspol, stfdem, vote)



##Vi bruker table() og complete.cases() for å sjekke omfanget av missing verdier. 

table(complete.cases(ess))

##Bruker funksjonen na.omit() til å kaste ut enheter med missing verdier
ess <- na.omit(ess)


###Sjekk hvordan avhengig variabel ser ut
str(ess$stfdem)
summary(ess$stfdem)
##Hvilket målenivå har denne variabelen? Må vi omkode?



##variabelen gndr er kodet 1 = mann og 2 = kvinne.
table(ess$gndr)
##Vi vil kode om denne slik at mann = 0 og kvinne = 1
##Vi bruker mutate() til å opprette en ny variabel
##if_else() bruker en logisk test for å tilskrive den nye variabelen nye verdier
ess <- ess %>%
  mutate(kjønn = if_else(gndr == 1, 0, 1))



##sjekk hvordan vote variabelen er kodet. Hva slags omkodning kan vi gjøre?
table(ess$vote)




##Vi vil ha en dikotom variabel som fanger hvorvidt personen stemte ved siste valg eller ikke
ess <- ess %>%
  mutate(vote2 = if_else(vote >= 2, 0, 1))
#Respondentene som er har verdien 2 eller høyere får verdien 0, mens respondentene med 1 får 1
##Sjekk ?if_else() for hjelpefilen til denne funksjonen



##Sjekk omkodingen med table()
table(ess$vote2)

##Vi endrer navnene på variablene, for å gjøre det hele mer oversiktelig
##Vi bruker rename()
ess <- ess %>%
  rename(alder = agea, 
         utdanning = eduyrs, 
         nyheter = nwspol, 
         demokrati = stfdem)



##Vi kan sjekke at omkodningen og navnebyttene er korrekte ved å bruke head()
head(ess)





#######OLS regresjonen########
##Vi bruker lm() for å kjøre en OLS i R


mod1 <- lm(demokrati ~ vote2 + nyheter + utdanning +
             alder + kjønn, data = ess)

summary(mod1) ##Ser på oppsummeringen av regresjonsmodellen




##Her har jeg lagt til ett samspillsledd mellom evne til politisk deltakelse og 
##konsum av politiske nyheter
##En måte å legge til samspill på er ved å bruke * mellom samspillsvariablene

mod2 <- lm(demokrati ~ vote2 * nyheter + utdanning +
             alder + kjønn, data = ess)

summary(mod2) #Legg merke til at vi for koeffisienter for både samspills effekten og for de to variablene individuelt




#####Plotting######

##Her plotter vi regresjonslinjen for utdannings variabelen vår.
ggplot(ess, #Spesifiserer datasetet vi jobber i
       aes(x = utdanning, ##Uavhengige variablenen vi er interessert i på x aksen
           y = demokrati)) + ##Avhhengig variabel på y aksen
  stat_smooth(method = "lm", col = "red") ##Dette er koden for en lineær regresjonlinje
                                          ##Viktig å spesifisere method = "lm" for å få lineær linje




##Videre skal vi se på noen grafiske verktøy for å vurdere om enkelte
##forutsetninger for OLS er oppfylt

##Da må vi først lagre restleddene og verdiene fra modellen vår i datasettet

ess$mod1Resid <- resid(mod1)
ess$mod1Fitted <- mod1$fitted.values


##Vurdere restleddenes fordeling med et histogram, er restleddene våre normalfordelte?

ggplot(ess,
       aes(x = mod1Resid)) + 
  geom_histogram()


##Nå skal vi forsøke å lage en figur som plotter restleddene mot modellens verdier
##Dette gjør vi for å vurdere om modellen inneholder hetroskedastisitet
ggplot(ess,
       aes(x = mod1Fitted, 
           y = mod1Resid)) +
  geom_point() + 
  geom_smooth() 

##Hvordan ser denne figuren ut? 


##Vi kan også bruke plot() for å få ulike figurer for diagnostistikk
##Vi vil få 4 ulike plot med denne funksjonen
plot(mod1)





#####Regresjonstabeller###########


##Vi lager en fin regresjonstabell med begge modellene våre side ved side

stargazer(mod1, mod2, ##Legger til begge modellene
          type = "text", ##Spesifiserer hva slags type tabell vi vil ha
          title = c("Modeller"), ##Tittel på tabellen
          covariate.labels = c("Stemte ved forrige valg", ##Forklarende navn på variabler
                               "Politiske nyheter",
                               "Utdanning",##Legger disse inn i samme rekkefølge som i regresjonsformelen
                               "Alder",
                               "Kjønn",
                               "Samspill"),
          dep.var.labels = c("Tilfredshet med demokratiet"))##et forklarende navn på avhengig variabel



##Det finnes flere måter å lagre tabellen på se ?stargazer, type = agrumentet
##men vi ønsker ofte å bruke tabellene i eksempelvs word
##Vi kan lagre tabellen som en html-fil og deretter bruke den i eksempelvis word
##Dette gjør vi ved å sette type = "html",
## deretter legger vi til argumentet out = "navnpåtabell.htm"
stargazer(mod1, mod2, 
          type = "html", 
          title = c("Modeller"), 
          covariate.labels = c("Stemte ved forrige valg", 
                               "Politiske nyheter",
                               "Utdanning",
                               "Alder",
                               "Kjønn",
                               "Samspill"),
          dep.var.labels = c("Tilfredshet med demokratiet"),
          out = "regtabell.htm")






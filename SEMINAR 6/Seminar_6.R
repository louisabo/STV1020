### SEMINAR 6

# PAKKER 
library(tidyverse)
library(stargazer)
library(modelr)


####Laster inn datasettet. 
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")


###Vi går igjennom datasettet vårt og ser hva slags informasjon vi har 
head(data)

##Vi bruker table() og complete.cases() for å sjekke omfanget av missing verdier. 

table(complete.cases(data))

##Bruker funksjonen na.omit() til å kaste ut enheter med missing verdier
df <-  data %>% 
  drop_na()

head(df)

###Sjekk hvordan variablene ser ut 
str(df)
summary(df)

##variabelen gndr er kodet 1 = mann og 2 = kvinne.
table(df$kjonn)
##Vi vil kode om denne slik at mann = 0 og kvinne = 1
##Vi bruker mutate() til å opprette en ny variabel
##if_else() bruker en logisk test for å tilskrive den nye variabelen nye verdier
df <- df %>%
  mutate(kjonn = if_else(kjonn == 1, 0, 1))

table(df$kjonn)


## Sjekker verdiene på de variablene vi er interessert i
table(df$internett) # Begynner på 1, jeg vil den skal begynne på 0

df$internett <- df$internettbruk-1

table(df$tillit)

table(df$utdanning)

table(df$alder)

## Fjerner den variabelen jeg ikke vil ha i datasettet 

df <- df %>% 
  select(kjonn, alder, tillit, internett, utdanning)


#######OLS regresjonen########
##Vi bruker lm() for å kjøre en OLS i R


mod1 <- lm(tillit ~ internett, data = df)

summary(mod1)

stargazer(mod1, type = "text")##Ser på oppsummeringen av regresjonsmodellen

# Vi legger til en til variabel 

mod2 <- lm(tillit ~ internett + alder, data = df)


stargazer(mod2, type = "text") # Se på resultatene 

# Vi kan bygge på måten vi presenterer resulatene våre 

stargazer(mod2, type = "text", 
          dep.var.caption = "OLS regresjon",
          dep.var.labels = "Politisk tillit", 
          covariate.labels = c("Internettbruk", 
                               "Alder"))

# Vi bygger videre: 

# Husk å nevne samspillsledd! 
3.4-0.128
mod3 <- lm(tillit ~ internett + alder + utdanning + kjonn, data = df)

stargazer(mod1, mod2, mod3, type = "text", 
          dep.var.caption = "OLS regresjon",
          dep.var.labels = "Politisk tillit", 
          covariate.labels = c("Internettbruk", 
                               "Alder", 
                               "Utdanning", 
                               "Kvinner"))




### Vi kan plotte regresjonen vår. 
library(modelr)
# Hvilke prediksjoner er det jeg tar ut?

prediksjoner <- df %>% 
  add_predictions(mod3, "tillit")

# Jeg predikerer tillit basert på de predikerte verdiene fra modellen
ggplot(df, aes(utdanning, tillit)) + 
  geom_point()+
  geom_smooth(data =prediksjoner, method = lm, size = 1, colour = "blue")



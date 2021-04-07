## Seminar 5 ### 

# Begynner med repetisjon fra forrige gang 
# Pearson's R og linear sammenheng - tolkning og plotting 

# Setter working directory for å lagre output 
# Laster pakker 

library(tidyverse)
library(stargazer)
library(modelr)
# Laser inn data 
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")

View(data) # Samme data som forrige seminar 

# Husker dere variablene? 

# Vi må se på NA: 
# Viser flere måter å gjøre det på: 

table(complete.cases(data)) # Viser ufullstendige rader

table(is.na(data$internettbruk)) # Viser antall NA på variabel

# Vi kan legge NA i et eget datasett:

missing_2 <- data %>% 
  filter_all(any_vars(is.na(.)))

# Vi kan også lage en ny variabel som flagger NA-rader

data <- data %>% 
  mutate(complete = complete.cases(.), 
         internett_na = is.na(internettbruk))

View(data) # Ser på hva vi får 

table(data$internett_na)
table(data$complete)

## Videre så fjerner jeg alle NA 

data <- data %>% 
  drop_na() %>% 
  select(1:5) # Velger de fem første variablene i datasettet

# Vi skal nå kode om variabelen kjonn 1 skal bli 0 og 2 skal bli 1
# Nå er 1 = mann og 2 = kvinne, jeg vil at de skal ha verdiene 0 og 1

data$kjonn_1 <- ifelse(data$kjonn == 1, 0, 1)

# Sjekker at det blir riktig 
table(data$kjonn_1)

# Vi skal også kode om utdanning 
table(data$utdanning) # Oversikt 

# Vi skal lage tre nivåer 
# 0-10 = lav
# 11-20 = mellom 
# 21-30 = høy 
# 31-37 = ekstrem

# Vi kan bruke ifelse og vi kan bruke mutate 
# Mutate er fra tidyverse og er bra på store omkodinger 
# Mutate er også bra på utregninger 

data <- data %>% 
  mutate(utdanning_1 = case_when(utdanning >=0 & utdanning <= 10 ~ "lav", 
                                 utdanning >=11 & utdanning <= 20 ~ "middels",
                                 utdanning >= 21 & utdanning <= 30 ~ "hoy",
                                 utdanning >=30 & utdanning <= 37 ~ "veldig hoy"))

# Ser at det blir riktig: 
table(data$utdanning_1) 

table(data$utdanning_1, data$utdanning)

# Vi kan bruke den nye variabelen til å lage en dikotom variabel: 
# 0 er lav og middels utdanning og 1 er høy og veldig utdanning 

# Dette kan vi gjøre med ifelse enten i eller uten mutate(tidyverse)

data$utdanning_2 <- ifelse(data$utdanning_1 == "lav", 0, 
                           ifelse(data$utdanning_1 == "middels", 0, 
                                  ifelse(data$utdanning_1 == "hoy", 1, 
                                         ifelse(data$utdanning_1 == "veldig hoy", 1, 
                                                ifelse(data$utdanning_1)))))

# Ser at det blir riktig 

table(data$utdanning_2)
table(data$utdanning_1, data$utdanning_2)

# Vi kan variablene dataene våre: 

ggplot(data, aes(utdanning_1)) + 
  geom_bar()


ggplot(data, aes(utdanning_2)) + 
  geom_bar() + 
  scale_x_discrete() + 
  xlab("Utdanning") + 
  ylab("frekvens")


#### Før vi går videre så sjekker vi klasse 

str(data) 

# Vi subsetter og tar med de variablene vi er interessert i 

df <- data %>% 
  select(internettbruk, alder, utdanning, tillit, kjonn_1)

df <- df %>% 
  rename(kjonn = kjonn_1)

###### Regresjon 

# Vi ønsker å estimere hvordan variablene henger sammen 
# Variablene må være numeriske 
# Vi ønsker å måle smh mellom alder og internettbruk 

mod <- lm(internettbruk ~ alder, data = df)
summary(mod)

# For mer oversikt bruk stargazer 

stargazer(mod, type = "text")

# Vi må tolke outputen! Hva ser vi? 
# Tenk på målenivå: 
# alder = antall år 
# internettbruk = scala 1-5 

# Konstantledd 
# Estimat 

# Jeg synes det å plotte gjør det enklere 
# for å forstå "outputen" 
# Men som vanlig begynn med målenivå og deretter 
# retning og styrke 


# Lager et prediksjonsplott 

 prediksjoner <- df %>% 
  add_predictions(mod, "internettbruk") 
 
 

ggplot(df, aes(alder, internettbruk)) + 
  geom_smooth(data = prediksjoner, colour = "red", 
            size = 1) + 
  ggtitle("Linær sammenheng mellom alder og internettbruk")


### Vi kan se konfidence intervall med 
confint(mod)


#### Regresjonsanalyse med dikotom variabel 


mod1 <- lm(tillit ~ kjonn, data = df)
summary(mod1)

# Vi kan også printe disse resultatene med 
# stargazer 

stargazer(mod1, type = "text")

# Vi må tolke!! 


# Plotter resultatene 

prediksjoner1 <- df %>% 
  add_predictions(mod1, "tillit")


# Det ser litt rart ut 
ggplot(df, aes(kjonn, tillit)) + 
  geom_line(data = prediksjoner1)

# Vi kan legge på argumenter: 
ggplot(df, aes(kjonn, tillit)) + 
  geom_line(data = prediksjoner1) + 
  scale_x_discrete() + 
  ggtitle("Smh mellom tillit og kjonn er negativ") + 
  xlab("Viser smh for kvinner, mann er ref")



##### Ekstra oppgaver 

### Oppgave 1 
# Lag en regresjon med utdanning og kjonn 
# Tolk resultatene 
# Hva er konfidensintervallet til estimatene? 
# Plott resultatene 

### Oppgave 2 
# Lag en regresjon med utdanning og tillit 
# Tolk resultatene 
# Hva er konfidensintervallet til estimatene? 
# Plott resultatene

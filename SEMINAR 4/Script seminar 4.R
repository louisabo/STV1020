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

# Kake- og søylediagram for grafisk beskrivelse av tabellene
barplot() # Søylediagram
pie() # Kakediagram

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
hist(data$alder, 
              breaks = 5) # Vi definerer fem kategorier, som brytes der R ønsker

# Vi kan også definere våre egne bruddpunkter for kategoriene
       hist(data$alder, 
            breaks = c(16, 30, 45, 60, 75, 90), 
            probability = FALSE, # Prøv med TRUE og FALSE
            # Vi kan også legge til flere argumenter. Bruk hjelpefilen ?hist 
            # for å finne ut hva de betyr:
            main = "", 
            xlab = "",
            ylab = "",
            col = "")

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
barplot(krysstabell, 
        beside = TRUE, 
        main = "Søylediagram over kjønn og internettbruk",
        names.arg = c("Kvinner", "Menn"))
legend("topleft", 
       bty = "n", 
       legend = c("1", "2", "3", "4", "5"), 
       fill = c(gray(0.1), gray(0.3), gray(0.5), gray(0.7), gray(0.9)))
       
## To kontinuerlige variabler ## 
         
# Pearsons r for alder og utdanning.
R <- cor(x = data$alder, 
         y = data$utdanning, 
         use = "pairwise.complete.obs", 
         method = "")
R
# Hva forteller dette oss?

# Korrelasjonsmatrise
cor(data, 
    use = "pairwise.complete.obs")
       
# Spredningsdiagram (kan legge til flere argumenter for å få et mer "fancy" spredningsdiagram. Sjekk hjelpefilen)
plot(x = data$alder,  
     y = data$utdanning)  
# Her legger vi til en støttelinje
scatter.smooth(data$utdanning ~ data$alder)

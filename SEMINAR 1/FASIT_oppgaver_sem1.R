## Fasit oppgaver seminar 1 

# OPPGAVE 1 

# Lager variabel navn 
navn <- c("Louisa", "Aleksander", "Alida", "Atle", "Charlotte", "Dagny", 
          "Dinah") 

# Lager variabel alder 
alder <- c(26, 26, 22, 24, 23, 23, 21)

# Lager variabel hjemby
hjemby <- c("stavanger", "bergen", "oslo", "trondheim", 
            "asker", "oslo", "oslo")

# Lager variabel favoritt film 

film <- c("Inglorious bastards", "The hateful 8", 
          "Pulp Finction", "Malcom and Marie", "The Shining", 
          "Tenet", "Inception")

# Lager datasett 

data <- data.frame(alder, navn, hjemby, film, 
                   stringsAsFactors = FALSE)

View(data) # ser datasettet

######### OPPGAVE 2 

summary(data) # printer alle variablene 
# og gir median, sd, og gjennomsnitt 

# Du kan også bruke: 
mean(data$alder) # gjennomsnitt

sd(data$alder) # standardavvik 

median(data$alder) # median 

############ OPPGAVE 3 

# Jeg bruker google maps for å finne avstanden
# her ser jeg på avstanden fra hjemby til Oslo
# Tall er i mil
# Merk at komma i tall er punktum i R og at "vanlig komma" 
# brukes for å skille mellom objekter
# 0 er de som bor i Oslo

til_oslo <- c(52.5, 72.7, 0,  49.2, 2.34, 0, 0)

# Legger til i datasettet 

# Du kan gjøre sånn:
data$til_oslo <- til_oslo

# Du kan også gjøre sånn: 

data <- data.frame(navn, alder, film, hjemby, 
                   til_oslo, 
                   stringsAsFactors = FALSE)

#### OPPGAVE 4 

table(data$navn, data$til_oslo)

# Aleksander er lengst hjemmefra 

#### OPPGAVE 6

# Enkel løsning til plot 

hist(data$alder)

#Eventuelt finn svaret i PDF-en som ligger ute
# I siste del står følgende: 

#### Last inn pakken 

# Skriv følgende kode
install.packages(tidyverse)

# Deretter 
library(tidyverse)


# Bruk koden ggplot 

ggplot(data, aes(alder)) + 
  geom_histogram()

## Dette skal ytterligere gjennomgås 
## neste seminar! Fortvil ikke! 



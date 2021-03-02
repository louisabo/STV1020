
##########  VELKOMMEN TIL SEMINAR 1 ##############

# I dag skal vi gjennomgå følgende 

# Introduksjon av R og RStudio 

# Bli kjent med mappe-struktur og filer 

# Opprette objekter og bli kjent med hverandre!
# Introduksjon til Klasser aka variabel målenivå
# Introduksjon til dataframes - datasett - data

# Lage objekter 

navn <- c("Louisa", "Alma", "Eirik", 
          "Emma", "Kristin", "Lavrans", 
          "Liv Fanny", "Oda", "Rahwa", "Vibeke", "Younes")

kjonn <- c(1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0)

kjonn_bokstav <- c("jente", "jente", "gutt", "jente", "jente",
                   "gutt", "jente", "jente", "jente", "jente", "gutt")

alder <- c(26, 22, 25, 21, 21, 23, 22, 20, 30, 22, 20)

studie <- c("stv", "inter", "stv", "stv", "inter", 
            "stv", "stv", "stv", "utv", "stv", "stv")

hjemby <- c("stavnger", "kolbotten", "vennesla", "skien", 
            "bergen", "lillestrøm", "tønsberg", "steinskjær", 
            "trondheim", "oslo", "stavanger")


om_oss <- data.frame(alder, hjemby, navn, studie, kjonn,
                     stringsAsFactors = FALSE)
View(om_oss)

om_oss$kjonn_bokstav <- kjonn_bokstav


# Hvordan legge til flere variabler i et datasett du har laget
# Indeksering 

table(om_oss$kjonn_bokstav)
table(om_oss$studie)

table(om_oss$kjonn_bokstav, om_oss$alder)

# R som kalkulator  -- enkle utreninger 
# Gjennomsnitt
mean(alder)
mean(om_oss$alder)

# Standardavvik
sd(om_oss$alder)
sd(alder)

# Median 
median(om_oss$alder)
median(alder)

summary(om_oss)

# Pakker 

install.packages("tidyverse")

library(tidyverse)

ggplot(data = om_oss, aes(x = alder, y = kjonn)) +
  geom_point()

table(om_oss$alder)

ggplot(om_oss, aes( alder, kjonn)) +
  geom_point()


# Plotting --
# Hvordan lage et plot: 
# Hjelpefiler i R 

# Hvordan google vet at alt 
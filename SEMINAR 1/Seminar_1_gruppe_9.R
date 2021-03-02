
##########  VELKOMMEN TIL SEMINAR 1 ##############

# I dag skal vi gjennomgå følgende 
# Introduksjon av R og RStudio 

# Bli kjent med mappe-struktur og filer 

# Opprette objekter og bli kjent med hverandre!
# Introduksjon til Klasser aka variabel målenivå


navn <- c("Louisa", "Amund", "Ana Synnøve", "Eskil", 
          "Henrik", "Laila", "Martin", "Ole", "Sebastian", "Vebjørn", 
          "Younes")

alder <- c(26, 22, 20, 20, 22, 29, 19, 20, 20, 20, 20)

studie <- c("stv", "lek", "stv", "stv", "stv", "inter", 
            "stv", "stv", "stv", "stv", "stv")

bosted <- c("stavanger", "sel", "trondheim", "kolsås", "oslo", 
            "sogn og fjordane", "oslo", "oslo", "tønsberg", "oslo", 
            "stavanger")

kjonn <- c("jente", "gutt", "jente", "gutt", 
           "gutt", "jente", "gutt", "gutt", "gutt", "gutt", 
           "gutt")

# Introduksjon til dataframes 

om_oss <- data.frame(alder, navn, bosted, studie, kjonn,
                     stringsAsFactors = FALSE)

View(om_oss)

# Hvordan legge til flere variabler i et datasett du har laget

om_oss$kjonn_1 <- kjonn

# Indeksering 

table(om_oss$alder)

table(om_oss$kjonn)

table(om_oss$kjonn, om_oss$alder)

# R som kalkulator  -- enkle utreninger 

# gjennomsnitt ?? 
mean(alder)

mean(om_oss$alder)

ny_alder <- om_oss$alder

# median 

median(om_oss$alder)
# standardavvik 

sd(om_oss$alder)

# Pakker 

install.packages("tidyverse")
library(tidyverse)

# Lage et plott

ggplot(data = om_oss, aes(x = alder)) +
  geom_histogram()

table(om_oss$alder)



# Hjelpefiler i R 

# Hvordan google vet at alt

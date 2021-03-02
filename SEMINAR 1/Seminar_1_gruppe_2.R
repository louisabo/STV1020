
##########  VELKOMMEN TIL SEMINAR 1 ##############

# I dag skal vi gjennomgå følgende 

# Introduksjon av R og RStudio 

# Bli kjent med mappe-struktur og filer 

# Opprette objekter og bli kjent med hverandre!

# Husk at nesten all kode må lukkes med ()
# All tekst skal inni "her"
# For flere gjenstander i et objekt må du ta c forann () -> c("innhold")

navn <- c("Louisa", "Aleksander", "Alida", "Atle", "Charlotte", "Dagny", 
          "Dinah") 

alder <- c(26, 26, 22, 24, 23, 23, 21)

studie <- c("stv", "inter", "inter", "inter", "stv", "stv", "utv")

hjemby <- c("stavanger", "bergen", "oslo", "trondheim", 
            "asker", "oslo", "oslo")


######### Introduksjon til Klasser aka variabel målenivå

# Vi har så langt fire variable med to ulike målevnivå
# Kategoriske variable - i R er dette faktor 
# Numerisk -- har deltall 
# Integer -- har ikke deltall -- kun hele tall 

########## Introduksjon til dataframes 

data <- data.frame(navn, alder, hjemby, studie,
                   stringsAsFactors = FALSE)

View(data)

# Hvordan legge til flere variabler i et datasett du har laget

# Indeksering 

table(data$alder)
table(data$studie)

table(data$alder, data$studie)

# R som kalkulator  -- enkle utreninger 
# Hjelpefiler i R 

mean(alder)
mean(data$alder)
sd(data$alder)

## Noen basekoder til å hente informasjon ut av en dataframe
head(data)
tail(data)
summary(data)

# Hvordan google vet at alt 


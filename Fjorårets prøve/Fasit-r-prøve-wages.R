
### Oppgave 1 

# Laser inn data

data <- read.csv("https://raw.githubusercontent.com/erlendmed/STV1020_V20/master/wages.csv")

### Oppgave 2

show(variable.names(data))

sd(data$wages, na.rm=T)

# Standardavviket til inntekt er 7883 pund. 

sd(data$education, na.rm=T)

# Standardavviket til utdanning er 3.4 år. 

summary(data$wages)

# Min verdi er 2300 pund, 
# Max verdi er 49 920 pund. 
# Gjennomsnitts-lønn er 15 553 pund
# Missing er 3278

summary(data$education)

# Min verdi er 0 år med fullført utdanning
# Max verdi er 20 år med fullført utdanning 
# Gjennomsnitt er 12.5 år med fullført utdanning
# Missing er 249

### Oppgave 3 
library(tidyverse)
ggplot(data, aes(wages)) + 
  geom_histogram()

# Plottet viser fordeling av inntekt. 
# De fleste tjener mellom 7-17 omtrent. 

### Oppgave 4 

ggplot(data, aes(education, wages)) + 
  geom_point()

# Plottet viser en fordeling hvor høyere 
# utdanning henger sammen med en høyere lønn. 

### Oppgave 5 

table (data$education, data$sex)

### Oppgave 6 

table(data$sex)

data$gender <- ifelse(data$sex == "Female", 1, 0)

table(data$sex, data$gender)

summary(data$education) # Må fjerne NA -- enten før eller i omkodingen

data <- data %>% 
  drop_na()

data$education_bin <- ifelse(data$education > mean(data$education), 1, 0)

table(data$education, data$education_bin)

###  Oppgave 7 

df <- data %>% 
  select(wages, education, sex, education_bin, gender )



### Oppgave 8 

mod <- lm(wages~ education, data = data)


summary(mod)

# Når education øker med 1 år ekstra fullført utdanning
# øker inntekt med ca. 792 pund.
# Estimatet er signifikant og positivt. 
# Når antall år med fullført utdanning er 0 er inntekt 4971 pund. 

### Oppgave 9 

mod1 <- lm(wages ~ education + sex, data=data)

summary(mod1)

# Den estimerte inntekten for kvinner 
# med null år fullført utdanning er ca 3019 pund.

# Den estimerte inntekten for menn 
# med null år fullført utdanning er 3019 + 3512 = 6531 pund.

# Modellen estimerer at dersom antall år med fullført utdanning 
# øker med 1, så øker inntekt med 807 pund for begge kjønn. 

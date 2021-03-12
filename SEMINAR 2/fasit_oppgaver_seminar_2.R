###### Seminar 2: Fasit #######

# 1. Bruk koden read.dta("ESS9NO.dta") for å få ESS-datasettet med alle 
# variablene igjen. 

df <- read.dta("ESS9NO.dta")

# Variabelen "trstplt" måler tillit til politikere og 
# variabelen "gndr" er en dikotom variabel for respondentens kjønn. Gi disse to 
# variablene, samt "yrbrn" navn som du synes er mer intuitive. Du skal bruke 
# disse navnene i de neste oppgavene der det står "trstplt", "gndr", og "yrbrn". 

df <- df %>% rename(
  trust = trstplt,
  gender = gndr, 
  age = yrbrn)

# Gjør "yrbrn" (året man er født) om til alderen respondenten hadde i 2018.

df$age <- 2018-df$age

# 2. Lag et datasett som bare inneholder "trstplt", "gndr", "yrbrn", og "vote". 

df <- df %>% select(trust, gender, age, vote)

# 3. Hvilken klasse har "trstplt" og "gndr"? 

class(df$trust) # factor
class(df$gender) # factor

# 4. Hvor mange nivåer har "trstplt"? Hvor mange har "complete trust" til 
# politikere? Er noen av nivåene mindre relevante? 

levels(df$trust) 
# 10, når man ser bort i fra "Refusal", "Don't Know", og "No answer". NA er 
# ikke et nivå.

summary(df$trust) 
# 8 har "complete trust" til politikere. Ingen har verdiene "Refusal", 
# "Don't Know" eller "No answer", og disse måler ikke politisk tillit.

# 5. Hvor mange nivåer har "gndr"? Hvor mange kvinner er det i datasettet?

levels(df$gender) # 2, når man ser bort i fra NA.
summary(df$gender) # Det er 629 kvinner i datasettet.

# 6. Lag et histogram som viser fordelingen av "trstplt". Si noe om hvordan 
# verdiene fordeler seg.

ggplot(data = df, aes(x = trust)) + geom_bar()

# 7. Er det noe forskjell mellom hvor mange menn og kvinner som stemte? Vis 
# dette ved hjelp av et diagram.

# Forslag til diagram man kan bruke: 
ggplot(data = df, aes(x = gender)) + geom_bar(aes(fill=vote), 
                                              position = "dodge")

# 8. Er det en forskjell i spredningen av alder for de som stemte og de som 
# ikke stemte, samt de som var berettiget til å stemme? Vis dette ved hjelp av 
# et diagram.

# Forslag til diagram man kan bruke: 
ggplot(data = df, aes(x = vote, y = age)) +
  geom_boxplot() 
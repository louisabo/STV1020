## Fasit oppgaver seminar 6 

## Oppgave 1 
norge <- read.csv("https://raw.githubusercontent.com/louisabo/STV1020/main/SEMINAR%206/data_seminar_6.csv")


## Oppgave 2 

names(norge)

table(complete.cases(norge)) # per rad 

sum(is.na(norge)) # missing overall 

## Oppgave 3 

table(norge$venstre_hoyre)

norge$venstre_hoyre <- ifelse(norge$venstre_hoyre == "Right", 10, 
                        ifelse(norge$venstre_hoyre == "Left", 0,
                               norge$venstre_hoyre))

table(norge$venstre_hoyre)

# Oppgave 4 
table(norge$tillit)

norge$tillit <- ifelse(norge$tillit == "Complete trust", 10, 
                              ifelse(norge$tillit == "No trust at all", 0,
                                     norge$tillit))

table(norge$venstre_hoyre)

## Oppgave 5

str(norge)

## Oppgave 6 

norge$tillit <- as.numeric(norge$tillit)

norge$venstre_hoyre <- as.numeric(norge$venstre_hoyre)

str(norge)

## Oppgave 7 

summary(norge$innvandring)
summary(norge$tillit)
summary(norge$venstre_hoyre)

## Oppgave 8 

norge1 <- norge %>% 
  drop_na()

## Oppgave 9 

mod <- lm(innvandring ~ alder  + kjonn + tillit + 
            venstre_hoyre, data = norge1)



stargazer(mod, type = "text")

# Alle variablene er signfikante. 
# Tillit til politikere er positivt korrelert med 
# positiv holdning til innvandring. 
# Mens jo lenger høyre man stiller seg politisk og alder 
# er negativt korrelert med positiv holdning til innvandring.
# Kvinner er mer positive til innvandrere enn menn. 

# Konstantleddet viser verdien på variabelen 
# Innvandring for en kvinne, med ingen politisk tillit
# som plasserer seg helt til venstre i politikken. 

# Variabelen venstre_hoyre viser at når en kvinne
# flytter seg mer til høyre på skalaen fra venstre til høyre 
# i politikken så går den positive holdningen til innvandring 
#ned med 0.214 skalaenhet. 

# Tillit viser at når tillit til politikere øker med 1 skalaenhet
# Så øker positiv holdning til innvandring med 0.304 skalaenheter. 

# Koeffisienten for menn viser at menn (sammenlignet med kvinner)
# er mindre positive till innvandrere med ca 0.270 skalaenheter

# Når alder øker med et år så minker positiv holdning til innvandrer 
# med 0.011 skalaenheter. 

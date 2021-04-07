
# Laster inn pakke 

library(tidyverse)

# Laser inn data 
data <- read.csv("https://raw.githubusercontent.com/louisabo/STV4020A/master/SEMINAR3/internett.csv")


# Fjerner NA 

data <- data %>% 
  drop_na()
# Måler korrelasjon 

cor(data, 
    use = "pairwise.complete.obs", 
    method = "pearson")

# Husk at Pearson tolker styrke og retning 
# Antar linear smh og har en skala på (-1, 1).

# Lager plots 

ggplot(data, aes(alder, internettbruk)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom alder og internettbruk er negativ")

ggplot(data, aes(alder, tillit)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom alder og tillit er negativ")

ggplot(data, aes(alder, utdanning)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom alder og utdanning er negativ")

ggplot(data, aes(utdanning, tillit)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom utdanning og tillit er positiv")

ggplot(data, aes(internettbruk, tillit)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom internettbruk og tillit er positiv")

ggplot(data, aes(utdanning, internettbruk)) + 
  geom_smooth(method = lm) + 
  ggtitle("Smh mellom utdanning og internettbruk er positiv")


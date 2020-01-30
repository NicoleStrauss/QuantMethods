# read in directly from website: 
trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')
#Examine structure of dataset
str(trees)
#Subset for Acer rubrum and Abies fraser"
Acer <- subset(trees, species == "Acer rubrum")
Abies <- subset (trees, species == "Abies fraseri")
#Explore data for Acer
plot(cover ~ streamdist, data = Acer, xlab = "streamdist", ylab ="cover")
Acer_stream = lm(cover ~ streamdist, data = Acer)
abline(Acer_stream)
summary(Acer_stream)

plot(cover ~ beers, data = Acer, xlab = "beers", ylab ="cover")
Acer_beers = lm(cover ~ beers, data = Acer)
abline(Acer_beers)
summary(Acer_beers)
    
plot(cover ~ tci, data = Acer, xlab = "tci", ylab ="cover")
Acer_tci = lm(cover ~ tci, data = Acer)
abline(Acer_tci)
summary(Acer_tci)

plot(cover ~ elev, data = Acer, xlab = "elev", ylab ="cover")
Acer_elev = lm(cover ~ elev, data = Acer)
abline(Acer_elev)
summary(Acer_elev)
    
#Both main effects model as determined by R^2 - beers and elev
pairs(~ cover + beers + elev, data=Acer)
Acer_main = lm(cover~beers + elev, data = Acer)
summary(Acer_main)
#Acer interaction
Acer_int = lm(cover~elev +tci + streamdist + beers, data=Acer)
#Acer anova
Anova(Acer_int, type =3)
Anova(Acer_main, type =3)
#Explore Abies data
plot(cover ~ streamdist, data = Abies, xlab = "streamdist", ylab ="cover")
Abies_stream = lm(cover ~ streamdist, data = Abies)
abline(Abies_stream)
summary(Abies_stream)

plot(cover ~ beers, data = Abies, xlab = "beers", ylab ="cover")
Abies_beers = lm(cover ~ beers, data = Abies)
abline(Abies_beers)
summary(Abies_beers)

plot(cover ~ tci, data = Abies, xlab = "tci", ylab ="cover")
Abies_tci = lm(cover ~ tci, data = Abies)
abline(Abies_tci)
summary(Abies_tci)

plot(cover ~ elev, data = Abies, xlab = "elev", ylab ="cover")
Abies_elev = lm(cover ~ elev, data = Abies)
abline(Abies_elev)
summary(Abies_elev)
#Elevation is definitely the most significant predictor
#of Abies cover as determined by R^2
#Main models effect for Abies
pairs(~ cover + elev + beers, data=Abies)
Abies_main = lm(cover~elev + beers, data = Abies)
summary(Abies_main)
#Abies interaction models
Abies_int = lm(cover~elev +tci + streamdist + beers, data=Abies)
#Abies Anova
library(car)
Anova(Abies_main, type =3)
Anova(Abies_int, type = 3)
##Homework Question 1: The only model that explains cover significantly
# is the elevation model for Abies. This makes sense given that this species
#is a habitat specialist. Acer is a generalist, and does not have specific
#requirements that limit its range in this dataset. I am unsure
#about models that reveal violations of assumptions.

#GLM Acer
acer_poi = glm(cover ~ tci + elev + streamdist + beers, data = Acer, 
               family='poisson')
summary(acer_poi)

#GLM Abies
abies_poi = glm(cover ~ tci + elev + streamdist + beers, data = Abies,
               family='poisson')
summary(abies_poi)


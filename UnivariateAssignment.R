# read in directly from website: 
trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')
#Examine structure of dataset
str(trees)

# Convert from long to wide format
#Subset for Acer rubrum and Abies fraseri
Acer <- subset(trees, species == "Acer rubrum")
Abies <- subset (trees, species == "Abies fraseri")
Acer_wide <- reshape (Acer, idvar = "plotID", timevar="species", direction ="wide")
Acer_wide[is.na(Acer_wide)] <- 0
str(Acer_wide)
Abies_wide <- reshape (Abies, idvar = "plotID", timevar="species", direction ="wide")
Abies_wide[is.na(Abies_wide)] <- 0
str(Abies_wide)




#Explore data for Acer
plot(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`streamdist.Acer rubrum`, data = Acer_wide, xlab = "streamdist", ylab ="cover")
Acer_stream = lm(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`streamdist.Acer rubrum`, data = Acer_wide)
abline(Acer_stream)
summary(Acer_stream)

plot(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`beers.Acer rubrum`, data = Acer_wide, xlab = "beers", ylab ="cover")
Acer_beers = lm(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`beers.Acer rubrum`, data = Acer_wide)
abline(Acer_beers)
summary(Acer_beers)
    
plot(Acer_wide$`cover.Acer rubrum`~Acer_wide$`tci.Acer rubrum`, data = Acer_wide, xlab = "tci", ylab ="cover")
Acer_tci = lm(Acer_wide$`cover.Acer rubrum`~Acer_wide$`tci.Acer rubrum`, data = Acer_wide)
abline(Acer_tci)
summary(Acer_tci)

plot(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`elev.Acer rubrum`, data = Acer_wide, xlab = "elev", ylab ="cover")
Acer_elev = lm(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`elev.Acer rubrum`, data = Acer_wide)
abline(Acer_elev)
summary(Acer_elev)

plot(Acer_wide$`cover.Acer rubrum`~Acer_wide$`disturb.Acer rubrum`, data = Acer, xlab = "disturb", ylab ="cover")
Acer_disturb = lm(Acer_wide$`cover.Acer rubrum`~Acer_wide$`disturb.Acer rubrum`, data = Acer)
abline(Acer_disturb)
summary(Acer_disturb)
    
#Both main effects model as determined by R^2 - beers and elev
pairs(~ Acer_wide$`cover.Acer rubrum` + Acer_wide$`beers.Acer rubrum` + Acer_wide$`elev.Acer rubrum`, data=Acer_wide)
Acer_main = lm(Acer_wide$`cover.Acer rubrum` ~Acer_wide$`beers.Acer rubrum` + Acer_wide$`elev.Acer rubrum`, data=Acer_wide)
summary(Acer_main)
#Acer interaction
Acer_int = lm(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`elev.Acer rubrum` + Acer_wide$`tci.Acer rubrum` + Acer_wide$`streamdist.Acer rubrum` +Acer_wide$`beers.Acer rubrum`, data= Acer_wide)
summary(Acer_int)
#Acer anova
Anova(Acer_int, type =3)
Anova(Acer_main, type =3)
#Explore Abies data
plot(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`streamdist.Abies fraseri`, data = Abies_wide, xlab = "streamdist", ylab ="cover")
Abies_stream = lm(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`streamdist.Abies fraseri`, data = Abies_wide)
abline(Abies_stream)
summary(Abies_stream)

plot(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`beers.Abies fraseri`, data = Abies_wide, xlab = "beers", ylab ="cover")
Abies_beers = lm(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`beers.Abies fraseri`, data = Abies_wide)
abline(Abies_beers)
summary(Abies_beers)

plot(Abies_wide$`cover.Abies fraseri`~Abies_wide$`tci.Abies fraseri`, data = Abies_wide, xlab = "tci", ylab ="cover")
Abies_tci = lm(Abies_wide$`cover.Abies fraseri`~ Abies_wide$`tci.Abies fraseri`, data = Abies_wide)
abline(Abies_tci)
summary(Abies_tci)

plot(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`elev.Abies fraseri`, data = Abies_wide, xlab = "elev", ylab ="cover")
Abies_elev = lm(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`elev.Abies fraseri`, data = Abies_wide)
abline(Abies_elev)
summary(Abies_elev)

plot(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`disturb.Abies fraseri`, data = Abies_wide, xlab = "disturb", ylab ="cover")
Abies_disturb = lm(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`disturb.Abies fraseri`, data = Abies_wide)
abline(Abies_disturb)
summary(Abies_disturb )

#Elevation is definitely the most significant predictor
#of Abies cover as determined by R^2. Disturbance is next. 
#Main models effect for Abies

pairs(~ Abies_wide$`cover.Abies fraseri`+ Abies_wide$`elev.Abies fraseri` + Abies_wide$`disturb.Abies fraseri`, data=Abies_wide)
Abies_main = lm(Abies_wide$`cover.Abies fraseri`~Abies_wide$`elev.Abies fraseri` + Abies_wide$`disturb.Abies fraseri`, data = Abies_wide)
summary(Abies_main)
#Abies interaction models
Abies_int = lm(Abies_wide$`cover.Abies fraseri`~Abies_wide$`elev.Abies fraseri` + Abies_wide$`tci.Abies fraseri` + Abies_wide$`streamdist.Abies fraseri` + Abies_wide$`disturb.Abies fraseri` + Abies_wide$`beers.Abies fraseri`, data=Abies_wide)
#Abies Anova
library(car)
Anova(Abies_main, type =3)
Anova(Abies_int, type = 3)
##Homework Question 1: The only model that explains cover significantly
# is the elevation model for Abies. This makes sense given that this species
#is a habitat specialist. Disturbance is the second most signifcant factor for 
# Abies. Acer is a generalist, and does not have specific
#requirements that limit its range in this dataset. I am unsure
#about models that reveal violations of assumptions.

#GLM Acer
acer_poi = glm(Acer_wide$`cover.Acer rubrum` ~ Acer_wide$`tci.Acer rubrum` + Acer_wide$`elev.Acer rubrum` + Acer_wide$`streamdist.Acer rubrum` + Acer_wide$`beers.Acer rubrum`, + Acer_wide$`disturb.Acer rubrum`, data = Acer_wide, 
               family='poisson')
summary(acer_poi)
pseudo_r2_acer = function(acer_poi) {
    1 - acer_poi$deviance / acer_poi$null.deviance }

pseudo_r2_acer(acer_poi)

#GLM Abies
abies_poi = glm(Abies_wide$`cover.Abies fraseri` ~ Abies_wide$`tci.Abies fraseri` + Abies_wide$`elev.Abies fraseri` + Abies_wide$`streamdist.Abies fraseri` + Abies_wide$`beers.Abies fraseri` + Abies_wide$`disturb.Abies fraseri`,  data = Abies_wide,
               family='poisson')
summary(abies_poi)
pseudo_r2_abies = function(abies_poi) {
    1 - abies_poi$deviance / abies_poi$null.deviance }
pseudo_r2_abies(abies_poi)
# A large degree of variation is accounted for by the Abies poisson model, while
#very little variation is explained by the Acer model. 

#Question 3: A significant portion of the cover distribution of Abies is explained
#by elevation, and to a lesser degree, by disturbance. There are no correlations
#with cover for Acer, reinforcing that this is a habitat generalist. 





library(vegan)
library(nlme)
library(dplyr)
library(ggplot2)
data(BCI)
## UTM Coordinates (in metres)
BCI_xy = data.frame(x = rep(seq(625754, 626654, by=100), each=5), 
                    y = rep(seq(1011569,  1011969, by=100), len=50))
?BCI
plot(BCI_xy)



#species abundance
species <- colSums(BCI)
mean(species)
max(species)
min(species)


# Selected Virola.sebifera as common and platypodium.elegans as rare 
sp <- data.frame(BCI$Virola.sebifera, BCI$Platypodium.elegans)

ggplot(BCI_xy, aes(x = x, y = y, size = sp[ , 1])) +
    geom_point() +
    scale_size_continuous(range = c(1,5))

ggplot(BCI_xy, aes(x = x, y = y, size = sp[ , 2])) +
    geom_point() +
    scale_size_continuous(range = c(1,5))
ggplot(BCI_xy, aes(x = x, y = y)) +
    geom_point() +
    geom_point(aes(sp$BCI.Virola.sebifera colour ='red')) +
    geom_point(aes (sp$BCI.Platypodium.elegans, colour ='blue'))


#Mantel test for significance 
sp_dist <- dist(sp)
xy_dist <- dist(BCI_xy)

sp_mantel = mantel(xy_dist, sp_dist)
sp_mantel

#Question 2

library(tidyverse)
sp_ids <- select(BCI, "Cordia.lasiocalyx", "Hirtella.triandra",
           "Picramnia.latifolia", "Quassia.amara",
           "Tabernaemontana.arborea", "Trattinnickia.aspera", 
           "Xylopia.macrantha")
class(sp_ids)

single_pred <- select(BCI, "Cordia.lasiocalyx", "Drypetes.standleyi")
col_brks = hist(single_pred, plot=F)$breaks
col_indices = as.numeric(cut(single_pred, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col=cols[col_indices])

single_dist <- dist(single_pred)
xy_dist <- dist(BCI_xy)

single_mantel = mantel(xy_dist, single_dist)
single_mantel

single_dat <- data.frame(single_pred, BCI_xy)
single_lm = gls(Drypetes.standleyi~ Cordia.lasiocalyx, data=single_dat)
plot(Variogram(single_lm, form= ~ x + y))

#Correlation between Cordia lasiocalyx and Drypetes standleyi are significant based on mantel test.

mult_pred <-  select(BCI, "Cordia.lasiocalyx", "Hirtella.triandra",
                                      "Picramnia.latifolia", "Quassia.amara",
                                      "Tabernaemontana.arborea", "Trattinnickia.aspera", 
                                      "Xylopia.macrantha", "Drypetes.standleyi")

col_indices = as.numeric(cut(mult_pred, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col=cols[col_indices])

mult_dist <- dist(mult_pred)
xy_dist <- dist(BCI_xy)

mult_mantel = mantel(xy_dist, mult_dist)
mult_mantel

class(mult_dat)
mult_dat <- data.frame(mult_pred, BCI_xy)

mult_lm = gls(Drypetes.standleyi ~ Cordia.lasiocalyx + Hirtella.triandra +
              Picramnia.latifolia + Quassia.amara +
              Tabernaemontana.arborea + Trattinnickia.aspera + 
              Xylopia.macrantha, data=mult_dat)



plot(Variogram(mult_lm, form= ~ x + y))



#Residuals for single variable model
max_dist = max(xy_dist) / 2
res = residuals(single_lm)
plot(dist(single_dat[, c('x', 'y')]), dist(res))
lines(lowess(dist(single_dat[, c('x', 'y')]), dist(res)), col='red', lwd=2)
abline(v = max_dist, col='red', lwd=3, lty=2)

single_exp = update(single_lm, corr=corExp(form=~x + y))
plot(Variogram(single_exp, maxDist = max_dist))

#Does not fit the plots, nugget present
plot(Variogram(single_exp, resType='normalized', maxDist = max_dist))

#single variable model with nugget
single_exp_nug = update(single_exp, corr=corExp(form=~x + y, nugget=T))
plot(Variogram(single_exp_nug, maxDist = max_dist))
plot(Variogram(single_exp_nug, resType='n', maxDist = max_dist))

single_rat_nug = update(single_lm, corr=corRatio(form=~x + y, nugget=T))
# examine fit of error model to model residuals
plot(Variogram(single_rat_nug, maxDist = max_dist))

plot(Variogram(single_rat_nug, resType='n', maxDist = max_dist))

# compare the single variable models
anova(single_lm, single_exp, single_exp_nug, single_rat_nug, test=F)

#The rational model with nugget seems to be the 'best' model
summary(single_rat_nug)

#check for spatial map with nugget - does seem to show spatial structure
col_brks = hist(residuals(single_exp_nug), plot=F)$breaks
col_indices = as.numeric(cut(residuals(single_exp_nug), col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col=cols[col_indices])

#Residuals for multiple variable model
max_dist = max(xy_dist) / 2
res = residuals(mult_lm)
plot(dist(mult_dat[, c('x', 'y')]), dist(res))
lines(lowess(dist(mult_dat[, c('x', 'y')]), dist(res)), col='red', lwd=2)
abline(v = max_dist, col='red', lwd=3, lty=2)

mult_exp = update(mult_lm, corr=corExp(form=~x + y))
plot(Variogram(mult_exp, maxDist = max_dist))

#Does not fit the plots, nugget present
plot(Variogram(mult_exp, resType='normalized', maxDist = max_dist))

#multiple variable model with nugget
mult_exp_nug = update(mult_exp, corr=corExp(form=~x + y, nugget=T))
plot(Variogram(mult_exp_nug, maxDist = max_dist))
plot(Variogram(mult_exp_nug, resType='n', maxDist = max_dist))

mult_rat_nug = update(mult_lm, corr=corRatio(form=~x + y, nugget=T))
# examine fit of error model to model residuals
plot(Variogram(mult_rat_nug, maxDist = max_dist))

plot(Variogram(mult_rat_nug, resType='n', maxDist = max_dist))

# compare the multiple variable models
anova(mult_lm, mult_exp, mult_exp_nug, mult_rat_nug, test=F)

#The AIC values for all of these are pretty close. The exponential model seems to be the 'best' model, then the
#exponential with nugget
summary(mult_exp)
summary(mult_exp_nug)
#check for spatial map with nugget - Some structure, but not as good as the single predictor map
col_brks = hist(residuals(mult_exp_nug), plot=F)$breaks
col_indices = as.numeric(cut(residuals(mult_exp_nug), col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(BCI_xy, cex=2, pch=19, col=cols[col_indices])

#For the single predictor, adding the spatial error terms resulted in a better fit model, with lower AIC value. 
#Accounting for nugget allowed for a better model. The model with multiple variables was affected less. This may
#be due to the presence of multiple variables which introduce more complexity and uncertainty to the analysis.
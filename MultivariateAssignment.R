
#REad in data
library(vegan)
data(dune)
data(dune.env)
?dune
#Indirect ordination.
dune_mds <- metaMDS(dune)
str(dune_mds)
#plot moisture model
 plot(dune_mds, type='n')
text(dune_mds, 'sp', cex=.5)
# generate vector of colors 
color_vect = rev(terrain.colors(6))[-1]
points(dune_mds, 'sites', pch=19, 
       col=color_vect[dune.env$Moisture])
legend('topright', paste("Moisture =", 1:5, sep=''), 
       col=color_vect, pch=19)
#Question 1: #MDS creates a plot that reduces stress between points by grouping the most similar ones together. The graph
#shows that species can be grouped by amount of moisture, as each species nearest each other tend to have the same moisture
#level (same color dots on the graph). 

#CCA 
cca_dune <- cca(dune ~., data=dune.env)
RsquareAdj(cca_dune, 100)
anova(cca_dune, permutations = 999)
anova (cca_dune, by= "margin", permutations =999) 
#Plot CCA
plot(cca_dune, type= 'n', scaling=1)
orditorp(cca_dune, display='sp', cex=0.5, scaling=1, col='orange')
text(cca_dune, display='bp', col='red')

#CCA with moisture, management, and manure

cca_top <- cca(dune ~ dune.env$Moisture + dune.env$Management + dune.env$Manure, data=dune.env)
RsquareAdj(cca_top, 100)
anova(cca_top, permutations = 999)
anova (cca_top, by= "margin", permutations =999)

plot(cca_top, type= 'n', scaling=1)
orditorp(cca_top, display='sp', cex=0.5, scaling=1, col='orange')
text(cca_top, display='bp', col='red')

#CCA with moisture

cca_moisture <- cca(dune ~ dune.env$Moisture, data=dune.env)
RsquareAdj(cca_moisture, 100)
anova(cca_moisture, permutations = 999)
anova (cca_moisture, by= "margin", permutations =999)

plot(cca_moisture, type= 'n', scaling=1)
orditorp(cca_moisture, display='sp', cex=0.5, scaling=1, col='orange')
text(cca_moisture, display='bp', col='red')

#CCA with moisture and management

cca_moist_manage <- cca(dune ~ dune.env$Moisture + dune.env$Management, data=dune.env)
RsquareAdj(cca_moist_manage, 100)
anova(cca_moist_manage, permutations = 999)
anova (cca_moist_manage, by= "margin", permutations =999)
plot(cca_moist_manage, type= 'n', scaling=1)
orditorp(cca_moist_manage, display='sp', cex=0.5, scaling=1, col='orange')
text(cca_moist_manage, display='bp', col='red')

#CCA with moisture and manure

cca_moist_manure <- cca(dune ~ dune.env$Moisture + dune.env$Manure, data=dune.env)
RsquareAdj(cca_moist_manure, 100)
anova(cca_moist_manure, permutations = 999)
anova (cca_moist_manure, by= "margin", permutations =999)

plot(cca_moist_manure, type= 'n', scaling=1)
orditorp(cca_moist_manure, display='sp', cex=0.5, scaling=1, col='orange')
text(cca_moist_manure, display='bp', col='red')

#Moisture appears to be the most significant variable. When manure is added to the model, the F test values goes up; However, the F
#test value is higher with management, and the adjusted R square value is less reduced. 

#Question 3: The indirect ordination indicated that moisture was the most significant variable. The full model ordination also showed
#heavy loading on the moisture variable, but also on management and manure. The reduced CCA models were not very helpful in
#illuminating a correlation between variables. 










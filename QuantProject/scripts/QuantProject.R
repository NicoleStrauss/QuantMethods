##Create a directory for git repo
#load packages
library(tidyr)
library(vegan)
install.packages('devtools')
library(devtools)
install.packages(c('bindrcpp','glue','pkgconfig','tibble','plyr','dplyr'))
library(dplyr)
install_github('MoBiodiv/mobr')

#Upload data for amphibian research
env <- read.csv('./data/Master_env_file.csv')
comm_mod <-  read.csv('./data/comm_mod.csv')
comm_hx <- read.csv('./data/comm_hx.csv')

str(env)
class(env)

#Select water chemistry values for PCA analysis
water_hx <- env %>%
    select(Pond, pH..Clemson.extension.3.7.00., PO4.P..ppm., K..ppm., Ca..ppm., Mg..ppm., Na..ppm., Cl..ppm., B..ppm., SO4..S..ppm., NO3.N..ppm., TDS..ppm...tot..dissolved.solid,EC..MMHOS.CM...electric.conduct, HCO3..MEQ.L., CO3..MEQ.L., SAR) 
  
water_mod <- env %>% 
    select (Pond, X2018pH.clemson., X2018PO4.P..ppm., X2018K..ppm., X2018Ca..ppm., X2018Mg..ppm., X2018Na..ppm., X2018Cl..ppm., X2018B..ppm., X2018SO4..S..ppm., X2018NO3.N..ppm., X2018TDS..ppm...tot..dissolved.solid, X2018EC..MMHOS.CM...electric.conduct, X2018HCO3..MEQ.L., X2018CO3..MEQ.L., X2018SAR, X2018Zn, X2018Cu, X2018Mn, X2018RSC) 

#PCA analysis - historic
PCA_hx <- rda(water_hx, scale=TRUE)
PCA_hx
str(PCA_hx)
summary(PCA_hx)
loadings(PCA_hx)
PCA_hx$CA$eig
round(PCA_hx$CA$eig / PCA_hx$tot.chi, 2)
plot(PCA_hx)
biplot(PCA_hx)

#PCA analysis - modern
PCA_mod <- rda(water_mod)
PCA_mod
str(PCA_mod)
summary(PCA_mod)
loadings(PCA_mod)
PCA_mod$CA$eig
round(PCA_mod$CA$eig / PCA_mod$tot.chi, 2)
plot(PCA_mod)
biplot(PCA_mod)

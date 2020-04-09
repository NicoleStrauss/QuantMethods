##Create a directory for git repo
#load packages
library(tidyr)
library(vegan)
install.packages('devtools')
library(devtools)
install.packages(c('bindrcpp','glue','pkgconfig','tibble','plyr','dplyr'))
library(dplyr)
devtools::install_dev("remotes")
remotes::install_github('MoBiodiv/mobr')
library(readr)
library(dplyr)

#Upload data for amphibian research
env <- read.csv('./data/Master_env_file.csv')

comm_mod <-  read.csv('./data/comm_mod.csv')
comm_hx <- read.csv('./data/comm_hx.csv')

comm_mod = as.matrix(comm_mod)
comm_mod = ifelse(is.na(comm_mod), 0, comm_mod)
comm_hx = as.matrix(comm_hx)
comm_hx = ifelse(is.na(comm_hx), 0, comm_hx)

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

PCA_hx$CA$eig
scores(PCA_hx)

round(PCA_hx$CA$eig/sum(PCA_hx$CA$eig),2)

#PCA analysis - modern
str(water_mod)
water_mod$X2018PO4.P..ppm. = as.numeric(as.factor(water_mod$X2018PO4.P..ppm.))
water_mod$X2018K..ppm. = as.numeric(as.factor(water_mod$X2018K..ppm.))
 water_mod$X2018Zn = as.numeric(as.factor(water_mod$X2018Zn))
    water_mod$X2018Cu = as.numeric(as.factor(water_mod$X2018Cu))
 water_mod$X2018Mn = as.numeric(as.factor(water_mod$X2018Mn))

PCA_mod <- rda(water_mod)
PCA_mod
str(PCA_mod)
summary(PCA_mod)
loadings(PCA_mod)
PCA_mod$CA$eig
round(PCA_mod$CA$eig / PCA_mod$tot.chi, 2)
plot(PCA_mod)
biplot(PCA_mod)

PCA_mod$CA$eig
scores(PCA_mod)

round(PCA_mod$CA$eig/sum(PCA_mod$CA$eig),2)

#Spatial analysis hx
env_xy <- env %>% select(X, Y)
plot(env_xy)
sr_hx = apply(comm_hx, 1, function(x) sum(x > 0))
hist(sr_hx)
plot(env_xy, cex = sr_hx/max(sr_hx))

col_brks = hist(sr_hx, plot=F)$breaks
col_indices = as.numeric(cut(sr_hx, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(env_xy, cex=2, pch=19, col=cols[col_indices])

# calculate Euclidean distance between richness and spatial coordinates
sr_dist_hx = dist(sr_hx)
xy_dist_hx = dist(env_xy)

max_dist_hx = max(xy_dist_hx) / 2

# plot result
plot(xy_dist_hx, sr_dist_hx)
abline(lm(sr_dist_hx ~ xy_dist_hx), lwd=3, col='red')
lines(lowess(xy_dist_hx, sr_dist_hx), lwd=3, col='pink')
abline(v = max_dist_hx, col='red', lwd=3, lty=2)

#correlation hx
obs_cor_hx = cor(xy_dist_hx, sr_dist_hx)
obs_cor_hx
# carry out a permutation test for significance:
nperm = 1000
null_cor_hx = obs_cor_hx
for (i in 2:nperm) {
    # shuffle the rows of the spatial coordinates
    tmp_xy_hx = env_xy[sample(nrow(env_xy)), ]
    # correlation between the shuffled spatial coordinates and sr_dist
    null_cor[i] = cor(dist(tmp_xy_hx), sr_dist_hx)
}
# compute the p-value
sum(null_cor_hx >= obs_cor_hx) / nperm

#mantel hx
sr_mantel_hx = mantel(xy_dist_hx, sr_dist_hx)

sr_mantel_hx


# compare the two approaches graphically using stacked boxplots
boxplot(list(null_cor_hx, sr_mantel_hx$perm), horizontal = T, boxwex = 0.5,
        names = c('mine', 'theirs'), xlab='Correlation')
abline(v=obs_cor_hx, col='red')

## compute bray curtis distance for the community matrix
comm_dist_hx = vegdist(comm_hx)
plot(xy_dist_hx, comm_dist_hx)
abline(lm(comm_dist_hx ~ xy_dist_hx), lwd=3, col='red')
lines(lowess(xy_dist_hx, comm_dist_hx), lwd=3, col='pink')
lines(lowess(xy_dist_hx, comm_dist_hx, f=0.1), lwd=3, col='blue')

abline(v = max_dist_hx, col='red', lwd=3, lty=2)

comm_mantel_hx = mantel(xy_dist_hx, comm_dist_hx)
comm_mantel_hx



#Spatial analysis mod
env_xy <- env %>% select(X, Y)
plot(env_xy)
comm_mod[is.na(comm_mod)] <- 0
sr_mod <-  apply(comm_mod, 1, function(x) sum(x > 0))
hist(sr_mod)
plot(env_xy, cex = sr_mod/max(sr_mod))

col_brks = hist(sr_mod, plot=F)$breaks
col_indices = as.numeric(cut(sr_mod, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(env_xy, cex=2, pch=19, col=cols[col_indices])

# calculate Euclidean distance between richness and spatial coordinates
sr_dist_mod = dist(sr_mod)
xy_dist_mod = dist(env_xy)

max_dist_mod = max(xy_dist_mod) / 2

# plot result
plot(xy_dist_mod, sr_dist_mod)
abline(lm(sr_dist_mod ~ xy_dist_mod), lwd=3, col='red')
lines(lowess(xy_dist_mod, sr_dist_mod), lwd=3, col='pink')
abline(v = max_dist_mod, col='red', lwd=3, lty=2)

#correlation hx
obs_cor_mod = cor(xy_dist_mod, sr_dist_mod)
obs_cor_mod
# carry out a permutation test for significance:
nperm = 1000
null_cor_mod = obs_cor_mod
for (i in 2:nperm) {
    # shuffle the rows of the spatial coordinates
    tmp_xy_mod = env_xy[sample(nrow(env_xy)), ]
    # correlation between the shuffled spatial coordinates and sr_dist
    null_cor_mod[i] = cor(dist(tmp_xy_mod), sr_dist_mod)
}
# compute the p-value
sum(null_cor_mod >= obs_cor_mod) / nperm

#mantel hx
sr_mantel_mod = mantel(xy_dist_mod, sr_dist_mod)

sr_mantel_mod


# compare the two approaches graphically using stacked boxplots
boxplot(list(null_cor_mod, sr_mantel_mod$perm), horizontal = T, boxwex = 0.5,
        names = c('mine', 'theirs'), xlab='Correlation')
abline(v=obs_cor_mod, col='red')

## compute bray curtis distance for the community matrix
comm_dist_mod = vegdist(comm_mod)
plot(xy_dist_mod, comm_dist_mod)
abline(lm(comm_dist_mod ~ xy_dist_mod), lwd=3, col='red')
lines(lowess(xy_dist_mod, comm_dist_mod), lwd=3, col='pink')
lines(lowess(xy_dist_mod, comm_dist_mod, f=0.1), lwd=3, col='blue')

abline(v = max_dist_mod, col='red', lwd=3, lty=2)

comm_mantel_mod = mantel(xy_dist_mod, comm_dist_mod)
comm_mantel_mod

#No spatial signaling for hx or comm data separately. Spatial analysis of both data sets together

comm_both <- comm_hx + comm_mod

#Spatial analysis both
env_xy <- env %>% select(X, Y)
plot(env_xy)
comm_both[is.na(comm_both)] <- 0
sr_both <-  apply(comm_both, 1, function(x) sum(x > 0))
hist(sr_both)
plot(env_xy, cex = sr_both/max(sr_both))

col_brks = hist(sr_both, plot=F)$breaks
col_indices = as.numeric(cut(sr_both, col_brks))
cols = rev(terrain.colors(length(col_brks)))
plot(env_xy, cex=2, pch=19, col=cols[col_indices])

# calculate Euclidean distance between richness and spatial coordinates
sr_dist_both = dist(sr_both)
xy_dist_both = dist(env_xy)

max_dist_both = max(xy_dist_both) / 2

# plot result
plot(xy_dist_both, sr_dist_both)
abline(lm(sr_dist_both ~ xy_dist_both), lwd=3, col='red')
lines(lowess(xy_dist_both, sr_dist_both), lwd=3, col='pink')
abline(v = max_dist_both, col='red', lwd=3, lty=2)

#correlation hx
obs_cor_both = cor(xy_dist_both, sr_dist_both)
obs_cor_both
# carry out a permutation test for significance:
nperm = 1000
null_cor_both = obs_cor_both
for (i in 2:nperm) {
    # shuffle the rows of the spatial coordinates
    tmp_xy_both = env_xy[sample(nrow(env_xy)), ]
    # correlation between the shuffled spatial coordinates and sr_dist
    null_cor_both[i] = cor(dist(tmp_xy_both), sr_dist_both)
}
# compute the p-value
sum(null_cor_both >= obs_cor_both) / nperm

#mantel both
sr_mantel_both = mantel(xy_dist_both, sr_dist_both)

sr_mantel_both


# compare the two approaches graphically using stacked boxplots
boxplot(list(null_cor_both, sr_mantel_both$perm), horizontal = T, boxwex = 0.5,
        names = c('mine', 'theirs'), xlab='Correlation')
abline(v=obs_cor_both, col='red')

## compute bray curtis distance for the community matrix
comm_dist_both = vegdist(comm_both)
plot(xy_dist_both, comm_dist_both)
abline(lm(comm_dist_both ~ xy_dist_both), lwd=3, col='red')
lines(lowess(xy_dist_both, comm_dist_both), lwd=3, col='pink')
lines(lowess(xy_dist_both, comm_dist_both, f=0.1), lwd=3, col='blue')

abline(v = max_dist_both, col='red', lwd=3, lty=2)

comm_mantel_both = mantel(xy_dist_both, comm_dist_both)
comm_mantel_both
#Running spatial analysis with both datasets reveals a more significant spatial signal. The observed value is only slightly 
#greater than what would be expected, so it is still a weak correlation.

# mobR analysis hx
   hx_mob <- make_mob_in(comm_hx, env, coord_names=c("Y","X"))
hx_mob

#Fire frequency and measures of diversity, hx
hx_stats_ff <- get_mob_stats(hx_mob, group_var = "ff_91_00")
plot(hx_stats_ff, 'S')
plot(hx_stats_ff, 'N')
plot(hx_stats_ff, 'S_n')

#Year since burn and diversity, hx
hx_stats_ysb <- get_mob_stats(hx_mob, group_var = "YSB_00")
plot(hx_stats_ysb, 'S')
plot(hx_stats_ysb, 'N')
plot(hx_stats_ysb, 'S_n')

#SAR had the greatest effect in the PCA for hx data 
hx_stats_sar <- get_mob_stats(hx_mob, group_var = "SAR")
plot(hx_stats_sar, 'S')
plot(hx_stats_sar, 'N')
plot(hx_stats_sar, 'S_n')

#There were not any significant P values for any plots of hx data

# mobR analysis mod

mod_mob <- make_mob_in(comm_mod, env, coord_names=c("Y","X"))

mod_mob

#Fire frequency and measures of diversity, mod
mod_stats_ff <- get_mob_stats(mod_mob, group_var = "ff_00_17")
plot(mod_stats_ff, 'S')
plot(mod_stats_ff, 'N')
plot(mod_stats_ff, 'S_n')

#Year since burn and diversity, mod
mod_stats_ysb <- get_mob_stats(mod_mob, group_var = "YSB_17")
plot(mod_stats_ysb, 'S')
plot(mod_stats_ysb, 'N')
plot(mod_stats_ysb, 'S_n')

#TDS had the greatest effect in the PCA for modern data 
mod_stats_tds <- get_mob_stats(mod_mob, group_var = "X2018TDS..ppm...tot..dissolved.solid")
plot(mod_stats_tds, 'S')
plot(mod_stats_tds, 'N')
plot(mod_stats_tds, 'S_n')

#P value for TDS on the alpha value of species richness was <.05

#Combined data sets
both_mob <- make_mob_in(comm_both, env, coord_names=c("Y","X"))

both_mob

#Fire frequency and measures of diversity, both
both_stats_ff <- get_mob_stats(both_mob, group_var = colSums("ff_91_00" + "ff_00_17"))
plot(both_stats_ff, 'S')
plot(both_stats_ff, 'N')
plot(both_stats_ff, 'S_n')

#Year since burn and diversity, both
both_stats_ysb <- get_mob_stats(both_mob, group_var = "YSB_17")
plot(both_stats_ysb, 'S')
plot(both_stats_ysb, 'N')
plot(both_stats_ysb, 'S_n')
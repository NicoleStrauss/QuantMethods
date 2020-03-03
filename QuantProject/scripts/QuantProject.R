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

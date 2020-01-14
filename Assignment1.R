# Read in data
tgpp <- read.csv('./data/tgpp.csv')
head(tgpp)
#Question 1
colnames(tgpp)
#Question 2: 11 rows, 4080 columns
dim(tgpp)
ncol(tgpp)
nrow (tgpp)
#Question 3
sapply(tgpp, class)
#Question 4
tgpp[c(1, 5, 8), c(3, 7, 10)]
#Question 5
?plot
plot(tgpp$scale), tgpp$richness, xlab = "Scale", ylab = "Richness", col="darkorange2") 
plot(tgpp$scale), tgpp$richness, xlab = "Scale", ylab = "Richness", col="darkorange2", log='xy') 

# Log plot Question: The plot is log transformed, and the data points are more linear. 

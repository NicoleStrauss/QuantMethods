tgpp <- read.csv('./data/tgpp.csv')
head(tgpp)
colnames(tgpp)

dim(tgpp)
ncol(tgpp)
nrow (tgpp)
sapply(tgpp, class)
tgpp[c(1, 5, 8), c(3, 7, 10)]
?plot
plot(1:length(tgpp$scale), tgpp$richness, xlab = "Scale", ylab = "Richness", col="darkorange2") 
plot(1:length(tgpp$scale), tgpp$richness, xlab = "Scale", ylab = "Richness", col="darkorange2", log='xy') 
plot(1:length(tgpp$richness), tgpp$scale, xlab = "Richness", ylab = "Scale", col="darkorange2")
plot(1:length(tgpp$richness), tgpp$scale, xlab = "Richness", ylab = "Scale", col="darkorange2", log='xy')

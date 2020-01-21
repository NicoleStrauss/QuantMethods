# Hey, I tried to finish these Qs Monday night after work, but it appears
# I've got the flu.I finished what I could this evening, but I will go back and 
#complete the rest of the questions for practice.
#Question 1: The loop creates a matrix that shows the mean of all columns by 
#Iris species
#Question 2: 
head(iris)

#unique vector of species names
sp_ids = unique(iris$Species)
#create a matrix with rows=number of species, columns= columns in iris 
output = matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(output) = sp_ids
colnames(output) = names(iris[ , -ncol(iris)])

#create a nested for loop where i separates variables by species ID

for(i in seq_along(sp_ids)) {
  iris_sp = subset(iris, subset=Species == sp_ids[i], select=-Species)
  #j selects each observation in columns of iris species
  for(j in 1:(ncol(iris_sp))) {
    x = 0
    y = 0
    if (nrow(iris_sp) > 0) {
      # k selects observation by row
      for(k in 1:nrow(iris_sp)) {
        # [k,j] is each observation in the dataset. x sums up the values of the observations
        x = x + iris_sp[k, j]
        # y sums up the number of observations
        y = y + 1
      }
      #output equals the mean of each column for each species
      output[i, j] = x / y 
    }
  }
}
output
#Question 3:  y= # of observations, x = number of observations, so that the output, x/y, equals the mean
#Question 4: 
sp_ids = unique(iris$Species)

output = matrix(0, nrow=length(sp_ids), ncol=ncol(iris)-1)
rownames(output) = sp_ids
colnames(output) = names(iris[ , -ncol(iris)])

#Question5: I couldn't quite finish this function to display the y values properly.



x <- 1:10
for (i in seq_along(x))  {
  (y <- cumsum(x))
  print (y)
}


#  a test file for function NNetIterations
#' contain two correct dimention examples and two invalide dataset



data(SAheart, package = "ElemStatLearn")
y.vec = as.vector(SAheart[,dim(SAheart)[2]])
X.mat = SAheart[,-dim(SAheart)[2]]
is.train = rep(TRUE,length(y.vec))
fold.vec = sample(rep(1:2), length(y.vec),TRUE)
is.train[which(fold.vec == 1)] = FALSE
is.train[which(fold.vec != 1)] = TRUE
max.iterations = 30L
step.size = 0.5
n.hidden.units = 15L
NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
## this will return: X.mat must be a numberic matrix!


X.mat = data.matrix(X.mat)
NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
#return the expected type


#example 2 : regression
data(prostate, package = "ElemStatLearn")
y.vec = as.vector(prostate[,dim(prostate)[2]-1])
X.mat = prostate[,c(-dim(prostate)[2],-dim(prostate)[2]-1)]
is.train = rep(0,length(y.vec))
fold.vec = sample(rep(1:2), length(y.vec),TRUE)
is.train[which(fold.vec == 1)] = FALSE
is.train[which(fold.vec != 1)] = TRUE
max.iterations = 30L
step.size = 0.5
n.hidden.units = 15L
NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
## this will return: X.mat must be a numberic matrix!


X.mat = data.matrix(X.mat)
NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
#this will return the expected type

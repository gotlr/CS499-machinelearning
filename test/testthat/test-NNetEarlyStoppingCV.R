#' a test file for function NNetEarlyStoppingCV
#' contain two correct dimention examples and two invalide dataset



#example 1: binary classification

data(SAheart, package = "ElemStatLearn")
y.vec = as.vector(SAheart[,dim(SAheart)[2]])
X.mat = SAheart[,-dim(SAheart)[2]]
n.folds = 4
fold.vec = sample(rep(1:n.folds), length(y.vec),TRUE)
max.iterations = 30L
step.size = 0.5
n.hidden.units = 15L
NNetEarlyStoppingCV(X.mat,y.vec,fold.vec,max.iterations,step.size,n.hidden.units,n.folds=4)
## this will return: X.mat must be a numberic matrix!


X.mat = data.matrix(SAheart[,-dim(SAheart)[2]])
NNetEarlyStoppingCV(X.mat,y.vec,fold.vec,max.iterations,step.size,n.hidden.units,n.folds=4)
#return the expected type


#example 2 : regression
data(prostate, package = "ElemStatLearn")
y.vec = as.vector(prostate[,dim(SAheart)[2]-1])
X.mat = SAheart[,c(-dim(SAheart)[2],-dim(SAheart)[2]-1)]
n.folds = 4
fold.vec = sample(rep(1:n.folds), length(y.vec),TRUE)
max.iterations = 30L
step.size = 0.5
n.hidden.units = 15L
NNetEarlyStoppingCV(X.mat,y.vec,fold.vec,max.iterations,step.size,n.hidden.units,n.folds=4)
## this will return: X.mat must be a numberic matrix!


X.mat = data.matrix(SAheart[,-dim(SAheart)[2]])
NNetEarlyStoppingCV(X.mat,y.vec,fold.vec,max.iterations,step.size,n.hidden.units,n.folds=4)

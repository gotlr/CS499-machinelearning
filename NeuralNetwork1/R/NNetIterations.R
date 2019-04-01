#'Neural networks for regression and binary classification
#'
#'Training by using nerual network with gradient descending
#'(real numbers for regression, probabilities for binary classification).
#'
#'@param X.mat (feature matrix, n_observations x n_features)
#'@param y.vec (label vector, n_observations x 1)
#'@param max.iterations (int scalar > 1)
#'@param step.size
#'@param n.hidden.units (number of hidden units)
#'@param is.train (logical vector of size n_observations,
#'TRUE if the observation is in the train set, FALSE for the validation set)
#'
#'@return pred.mat (n_observations x max.iterations matrix of predicted values or n x k)
#'@return W.mat:final weight matrix(n_features+1 x n.hidden.units or p+1 x u)
#'@return v.vec: final weight vector (n.hidden.units+1 or u+1).
#'@return predict(testX.mat):
#'a function that takes a test features matrix and returns a vector of predictions
#' (real numbers for regression, probabilities for binary classification)
#' The first row of W.mat should be the intercept terms;
#' the first element of v.vec should be the intercept term.
#'
#' @export
#'
#' @examples
#' data(ozone, package = "ElemStatLearn")
#' y.vec <- ozone[, 1]
#' X.mat <- as.matrix(ozone[,-1])
#' num.train <- dim(X.mat)[1]
#' num.feature <- dim(X.mat)[2]
#' X.mean.vec <- colMeans(X.mat)
#' X.std.vec <- sqrt(rowSums((t(X.mat) - X.mean.vec) ^ 2) / num.train)
#' X.std.mat <- diag(num.feature) * (1 / X.std.vec)
#' X.scaled.mat <- t((t(X.mat) - X.mean.vec) / X.std.vec)





NNetIterations <- function(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train){


  if(!all(is.matrix(X.mat),is.numeric(X.mat))){
    stop("X.mat must be a numberic matrix!")
  }


  if (!all(is.vector(y.vec), is.numeric(y.vec),length(y.vec) == nrow(X.mat))) {
    stop("y.vec must be a numeric vector of the same number of rows as X.mat!")
  }


  if(!all(max.iterations>=1, is.integer(max.iterations))){
    stop("max.iterations must be an interger greater or equal to 1!")
  }

  if(!all(is.numeric(step.size), 0<step.size, step.size<1)){
    stop("step.size must be a number between 0 and 1!")
  }

  if(!all(n.hidden.units>=1, is.integer(n.hidden.units))){
    stop("n.hidden.units must be an interger greater or equal to 1!")
  }

  if(!all(is.logical(is.train), length(is.train)==nrow(X.mat))){
    stop("y.vec must be a logical vector of the same number of rows as X.mat!")
  }

  n.obeservations <- nrow(X.mat)
  n.features <- ncol(X.mat)

  #compute a scaled input matrix, which has mean=0 and sd=1 for each column
  #X.scale.mat=scale(X.mat,center = TRUE,scale = TRUE)
  X.mat.min = colMeans(X.mat)

  X.temp=t(t(X.mat)-X.mat.min)

  X.mat.sd=sqrt(colSums((X.temp)^2)/dim(X.mat)[1])

  X.scaled.mat=t(t(X.temp)/X.mat.sd)

  #find(split) the train set and validation set
  train.index = which(is.train==TRUE)
  validation.index = which(is.train!=TRUE)
  X.scaled.train = X.scale.mat[train.index,]
  y.train = y.vec[train.index]
  X.scaled.validation = X.scale.mat[validation.index,]
  y.validation = y.vec[validation.index]

  pred.mat = matrix(0,n.obeservation, max.iterations)
  W.mat = matrix(rnorm(n.obeservations*n.hidden.units),n.features,n.hidden.units)
  v.vec = rnorm(n.hidden.units)

  sigmoid = function(x){
    return(1/(1+exp(-x)))
  }








}

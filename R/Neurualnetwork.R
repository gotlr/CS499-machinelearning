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
#NNetIterations <- function(X.mat,y.vec,max.iterations,step.size,n.hidden.units){


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
    stop("is.train must be a logical vector of the same number of rows as X.mat!")
   }

  if (all(y.vec %in% c(0,1)))
    is.binary = 1 else
    is.binary = 0

  n.observations <- nrow(X.mat)
  n.features <- ncol(X.mat)


 #find(split) the train set and validation set


  train.index = which(is.train==TRUE)
  validation.index = which(is.train!=TRUE)
  X.train = X.mat[train.index,]
  y.train = y.vec[train.index]
  X.validation = X.mat[validation.index,]
  y.validation = y.vec[validation.index]







  #compute a scaled input matrix, which has mean=0 and sd=1 for each column

  X.scaled.train = scale(X.train,center = TRUE,scale = TRUE)

  X.scaled.validation = scale(X.validation,center = TRUE,scale = TRUE)

  X.scaled.mat = scale(X.mat,center = TRUE,scale = TRUE)





  pred.mat = matrix(0,n.observations, max.iterations)
  v.mat = matrix(runif((n.features+1)*n.hidden.units),n.features+1,n.hidden.units)
  w.vec = runif(n.hidden.units+1)
  v.gradient=rep(0,n.hidden.units+1)
  w.gradient=matrix(0,n.features+1,n.hidden.units)


  sigmoid = function(x){
    return(1/(1+exp(-x)))
  }

  desigmoid=function(x){
    return(sigmoid(x)/(1-sigmoid(x)))

  }
  for(iteration in 1:max.iterations){
    X.a.mat = (cbind(1,X.scaled.train))%*%v.mat
    X.z.mat = sigmoid(X.a.mat)
    #X.b.vec = X.z.mat %*% v.vec + interception.vec
    X.b.vec = (cbind(1,X.z.mat)) %*% w.vec

     if(is.binary){
     ##binary classification
     pred.mat[,iteration] = sigmoid(cbind(1,sigmoid(cbind(1,X.scaled.mat)%*%v.mat))%*%w.vec)
     y.tilde.train = y.train
     y.tilde.train[which(y.tilde.train==0)] = -1 # change y into non-zero number
     delta.w = -y.train/(1+exp(y.train*X.b.vec))
     delta.v = diag(as.vector(delta.w)) %*% desigmoid(X.a.mat)
     }else{


    ##if regression
    #pred.mat[train.index,iteration] = cbind(1,sigmoid(cbind(1,X.scaled.train)%*%v.mat))%*%w.vec
    #pred.mat[validation.index,iteration] = cbind(1,sigmoid(cbind(1,X.scaled.validation)%*%v.mat))%*%w.vec
    #pred.mat[,iteration] = cbind(1,sigmoid(cbind(1,X.scaled.mat)%*%v.mat))%*%w.vec
    delta.w = X.b.vec - y.train
    delta.v = diag(as.vector(delta.w)) %*% desigmoid(X.a.mat)
    #
     }



    w.gradient = (t(cbind(1,X.z.mat))%*%delta.w)/n.observations
    v.gradient = (t(cbind(1,X.scaled.train)) %*% delta.v)/ n.observations
    w.vec = w.vec - step.size*as.vector(w.gradient)
    v.mat = v.mat - step.size*v.gradient
    }

 # pred.mat = sigmoid(X.scaled.mat %*% W.mat) %*% v.vec

    #v.vec = c(interception.vec,as.vector(v.vec))



 # result.list = list(
 #   pred.mat = pred.mat,
 #   v.mat = v.mat,
 #   w.vec = w.vec,
 #   prediction = function(testX.mat){
 #     if(is.binary){
 #     prediction.vec = sigmoid(cbind(1,sigmoid(cbind(1,testX.mat)%*%v.mat))%*%w.vec)
 #     }else{
 #
 #     prediction.vec = cbind(1,sigmoid(cbind(1,testX.mat)%*%v.mat))%*%w.vec
 #     }
 #     return (prediction.vec)
 #   }
 # )
 #
 # return(result.list)
  return(pred.mat)
  }

NNetEarlyStoppingCV <-
  function(X.mat, y.vec,fold.vec,max.iterations,step.size,n.hidden.units,n.folds = 4){


    #fold.vec = sample(rep(1:n.folds), length(y.vec),TRUE)  in test file
     mean.train.loss.vec =  rep(0,max.iterations)
     mean.validation.loss.vec =  rep(0,max.iterations)
     is.train = rep(TRUE,length(y.vec))




    for(fold.number in 1:n.folds){

       is.train[which(fold.vec == fold.number)] = FALSE
       is.train[which(fold.vec != fold.number)] = TRUE
       #X.scaled.mat = scale(X.train,center = TRUE,scale = TRUE)
        #
         train.index = which(is.train==TRUE)
         validation.index = which(is.train!=TRUE)
         X.train = X.mat[train.index,]
         y.train = y.vec[train.index]
         X.validation = X.mat[validation.index,]
         y.validation = y.vec[validation.index]



      return.list = NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)
      prediction.train = return.list$pred.mat[train.index,]
      prediction.validation =  return.list$pred.mat[validation.index,]


       mean.train.loss.vec = mean.train.loss.vec + colMeans(abs(prediction.train - y.train))
       mean.validation.loss.vec = mean.train.loss.vec + colMeans(abs(prediction.validation - y.validation))

    }
    selected.steps = which.min(mean.validation.loss.vec)



    #result.list = list(
     #mean.train.loss.vec = mean.train.loss.vec,
     #mean.validation.loss.vec = mean.validation.loss.vec,
     #selected.steps = selected.steps
    #)

    #return(result.list)
  }









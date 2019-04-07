#' a function using nerual network through cross validation
#'
#' use K-fold cross validation based on the folds IDs provided in fold.vec(randomly)
#'
#' for each validarion/train split, use NNetIterations to compute the predictions
#' for all observations
#'
#' compute mean.validation.loss.vec, which is a vector(with max.iterations elements)
#' of mean validation loss over all K folds
#'
#' comput mean.train.loss.vec, analogous to above but for the train data
#'
#' minimize the mean validation loss to determine selected.steps,
#' the optimal number of steps/iterations
#'
#' finally use NNetIteration(max.iterations=selected.steps) on the whole training data set
#'
#' @param X.mat : n x p
#' @param y.vec : vector n x 1
#' @param fold.vec : number of validation/training sets
#' fold.vec = samole(1:n.folds,length(y.vec))
#' @param max.iterations
#' @param step.size
#' @n.hidden.units
#' @n.folds = 4
#'
#' @return mean.validation.loss
#' @return mean.train.loss.vec
#' @return selected.steps




NNetEarlyStoppingCV <-
  function(X.mat,
           y.vec,
           fold.vec,
           max.iterations,
           step.size,
           n.hidden.units,
           n.folds = 4){


    #fold.vec = sample(rep(1:n.folds), length(y.vec),TRUE)  in test file
    mean.train.loss.vec =  rep(0,max.iterations)
    mean.validation.loss.vec =  rep(0,max.iterations)
    is.train = rep(0,length(y.vec))



    for(fold.number in 1:n.folds){
      is.train[which(fold.vec == fold.number)] = FALSE
      is.train[which(fold.vec != fold.number)] = TRUE
      pred.mat = NNetIterations(X.mat,y.vec,max.iterations,step.size,n.hidden.units,is.train)$pred.mat
      mean.train.loss.vec = mean.train.loss.vec + colMeans(pred.mat[which(is.train == TRUE),])
      mean.validation.loss.vec = mean.validation.loss.vec + colMeans(pred.mat[which(is.train != TRUE),])
    }
    selected.steps = which.min(mean.validation.loss.vec)


    result.list = list(
     mean.train.loss.vec = mean.train.loss.vec,
     mean.validation.loss.vec = mean.validation.loss.vec,
     selected.steps = selected.steps
    )


  }

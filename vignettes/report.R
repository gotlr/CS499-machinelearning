library(NeuralNetwork1)
# This is a test script for general data


data(spam, package = "ElemStatLearn")
data(SAheart, package = "ElemStatLearn")
data(zip.train, package = "ElemStatLearn")
zip.train <- zip.train[zip.train[,1] %in% c(0,1),]
data(prostate, package = "ElemStatLearn")
data(ozone, package = "ElemStatLearn")

data.list = list(
  spam = list(
    dataset = as.matrix(spam[, 1:57]),
    labels = ifelse(spam$spam == "spam", 1, 0)),

  SAheart = list(
    dataset = as.matrix(SAheart[, c(1:4,6:9)]),
    labels = SAheart$chd),

  zip.train = list(
    dataset = as.matrix(zip.train[, -1]),
    labels = zip.train[, 1]),

  prostate = list(
    dataset = as.matrix(prostate[, 1:8]),
    labels = prostate$lpsa),

  ozone = list(
    dataset = as.matrix(ozone[,-1]),
    labels = ozone[, 1])
)


n.folds = 4L
max.iterations = 1000L
step.size = 0.5
n.hidden.units = 5L
fold.vec = fold.vec = sample(rep(1:4), length(y.vec),TRUE)

for (data.name in names(data.list)) {
  X.mat = data.list[[data.name]]$dataset
  y.vec = data.list[[data.name]]$labels

  mean.loss.train =
  NNetEarlyStoppingCV(X.mat, y.vec,fold.vec,max.iterations,
                      step.size,n.hidden.units,n.folds = 4)$mean.train.loss.vec

  mean.loss.validation =
  NNetEarlyStoppingCV(X.mat, y.vec,fold.vec,max.iterations,
                        step.size,n.hidden.units,n.folds = 4)$mean.validation.loss.vec

  plot(c(1:max.iterations),mean.loss.validation,type="o",xlim = c(1,max.iterations), ylim=c(0,1),
       xlab="interations",ylab="mean.validation",
       col="red",main="loss value",pch=c(15))

  par(new=TRUE)

  plot(c(1:max.iterations),mean.loss.train,type="o",xlim = c(1,max.iterations), ylim=c(0,1),
       xlab="interations",ylab="mean.validation",
       col="blue",main="loss value",pch=c(15))

}

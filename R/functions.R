#' Find NAs in a dataset
#'
#' This function finds the number of missing values in a dataset 
#' and the columns where they are found.
#'
#' @param dataframe A data frame, data frame extension (e.g. a tibble) or a matrix
#' @return A list with the number of missing values and 
#' the columns where they are found.
#' @export
findNA = function(dataframe){
  dataframe = as.data.frame(dataframe)
  na = which(is.na(dataframe), arr.ind = T)
  n = length(unique(na[,1]))
  columns = apply(dataframe, 2, anyNA)
  l = list(n, columns)
  return(l)
}

#' Transform the columns of a dataset
#'
#' This function transform the columns of a dataset into factors or numerics 
#' depending on the number of different observations found (<=10 factor, otherwise 
#' numeric).
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param return_factors Should the factor variables be returned? Defaults to TRUE.
#' @return A dataframe with the alterations.
#' @export
mutate_column = function(data, return_factors = T){
  data = as.data.frame(data)
  data = dplyr::mutate_if(data, is.character, as.factor)
  n = ncol(data)
  index = c()
  for(i in 1:n){
    column = data[,i]
    if(is.factor(column)){
      if(nlevels(column)>10){
        data = dplyr::mutate_at(data, i, as.numeric)
      } else {
        index = c(index, i)
      }
    }
  }
  if(!is.null(index)){
    factors = data[,index]
    if(return_factors) cat("Factors: ", colnames(factors))
  }
  return(data)
}

#' Transform the columns of a dataset
#'
#' This function turns factors into dummies, checks for near zero covariates 
#' in the dataset and checks for highly correlated variables.
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param factors The names of the factor variables of the dataset.
#' @param cut The threshold that determines high correlation.
#' @return A dataframe with the alterations.
#' @export
preprocess = function(data, factors, cut = .75){
  dummies = caret::dummyVars(as.formula(paste("~", paste(factors, collapse = "+"))), 
                             data = data, fullRank = T)
  Dummies = predict(dummies, newdata = data)
  data = cbind(data, Dummies)
  data = dplyr::mutate_at(data, names(as.data.frame(Dummies)), as.factor)
  data = dplyr::select(data, -all_of(factors))
  nzv = caret::nearZeroVar(data, names = T)
  cat("Near zero variable: ", nzv)
  cor = psych::mixedCor(dplyr::mutate_all(data, as.numeric), method = "spearman")
  Cor = caret::findCorrelation(as.matrix(cor$rho), cutoff = cut, 
                               verbose = T, names = T)
  cat("\n", "Variable with high correlation: ", Cor)
  return(data)
}

#' Mean, standard deviation and histogram
#'
#' This function calculates the mean and standard deviation of a numeric
#' column in a dataset and returns its histogram.
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param x The name of the numeric column to be used.
#' @param name The name of the variable to be plotted on the histogram.
#' @param color The color of the histogram bars.
#' @return The mean and the standard deviation of the variable.
#' @export
hists = function(data, x, name, color){
  i = match(x, names(data))
  mean = mean(data[, i])
  sd = sd(data[, i])
  hist(data[, i], xlab = name, main = paste("Histogram of", name, collapse = " "),
       col = color)
  return(cat("Mean: ", round(mean,3), "/ Standard deviation: ", round(sd,3)))
}

#' Draws boxplots according to factors
#'
#' This function plots a boxplot for a numeric variable for each factor 
#' of a variable in the database.
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param y The name of the numeric variable to be used.
#' @param fill The name of the factor variable.
#' @param title The text for the title.
#' @param ylab The text for the y axis.
#' @param namelegend The text for the legend.
#' @param labellegend The text for the legend labels.
#' @return Boxplots.
#' @export
boxplots = function(data, y, fill, title, ylab, namelegend, labellegend){
  graph = ggplot2::ggplot(data) +
    ggplot2::geom_boxplot(ggplot2::aes_string(y = y, fill = fill)) +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position="bottom") +
    nord::scale_fill_nord("aurora", name = namelegend, labels = labellegend) +
    ggplot2::ggtitle(title) + ggplot2::ylab(ylab)
  return(graph)
}

#' Draws a barplot according to factors
#'
#' This function plots a barplot for a factor variable according 
#' to the factors of another variable in the dataset. 
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param x The name of the first factor variable.
#' @param fill The name of the second factor variable.
#' @param title The text for the title.
#' @param xlab The text for the x axis.
#' @param namelegend The text for the legend.
#' @param labellegend The text for the legend labels.
#' @return Barplot.
#' @export
barplots = function(data, x, fill, title, xlab, lx, namelegend, labellegend){
  dt <- dplyr::summarise(dplyr::group_by_at(data, dplyr::vars(x, fill)), N=dplyr::n())
  graph = ggplot2::ggplot(dt) +
    ggplot2::geom_bar(ggplot2::aes_string(x = x, fill = fill, y = 'N'), color="black", 
                              position = "fill", stat = "identity", width = .5) + 
    ggplot2::ggtitle(title) + ggplot2::theme_minimal() +
    nord::scale_fill_nord("aurora", name = namelegend, labels = labellegend) + 
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::xlab(xlab) + ggplot2::ylab("Proportion") +
    ggplot2::scale_x_discrete(labels = lx)
}

#' Compares different machine learning methods
#'
#' This function compares different machine learning methods 
#' through cross validation.
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param response The name of the response variable.
#' @param models A character vector with the names of the methods to be compared.
#' The names must be as in names(caret::getModelInfo()).
#' @param n The number of folds to be used for cross validation.
#' @param k The number of complete sets of folds.
#' @param seed A single value.
#' @param tune An integer number of default values for each parameter to be tested.
#' @return A list with the performance results of the methods.
#' @export
modelsML = function(data, response, models, n = 10, k = 3, seed = 11, tune = 3){
  tc = caret::trainControl(method = "repeatedcv", number = n, repeats = k)
  N = length(models)
  M = list()
  for(i in 1:N){
    set.seed(seed)
    M[[i]] = caret::train(as.formula(paste(response, "~.")), data = data, 
                          method = models[i], trControl = tc, tuneLength = tune)
  }
  results = caret::resamples(M)
  summary = summary(results)
  scale = list(x = list(relation = "free"), y = list(relation = "free"))
  boxplots = lattice::bwplot(results, scales = scale)
  comb = gtools::combinations(N, 2, summary$models)
  plots = list()
  for(i in 1:nrow(comb)){
    plots[[i]] = lattice::xyplot(results, models = comb[i,])
  }
  dif = diff(results, models = summary$models)
  HT = summary(dif)
  return(list(summary, boxplots, plots, HT))
}

#' Transforms a factor variable into a numeric variable
#'
#' This function transforms a factor variable into a numeric variable.
#'
#' @param x The factor variable.
#' @return A numeric variable.
#' @export
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#' Splits the data
#'
#' Splits the data into training, training label, test and test label.
#'
#' @param data A data frame or a data frame extension (e.g. a tibble).
#' @param response The name of the response variable.
#' @param pct The percentage of data that goes to training.
#' @return A list with the partitions.
#' @export
train_test = function(data, response, pct = .8){
  i = match(response, names(data))
  inTrain = caret::createDataPartition(data[, i], p = pct, list = F)
  data = dplyr::mutate_if(data, is.factor, as.numeric.factor)
  data = as.matrix(data)
  train = data[inTrain, -i]
  rownames(train) <- 1:nrow(train)
  train_label = as.matrix(data[inTrain, i])
  rownames(train_label) <- 1:nrow(train_label)
  test = data[-inTrain, -i]
  rownames(test) <- 1:nrow(test)
  test_label = as.matrix(data[-inTrain, i])
  rownames(test_label) <- 1:nrow(test_label)
  l = list("train" = train, "train_label" = train_label, "test" = test,
           "test_label" = test_label)
  return(l)
}

#' Fits an XGBoost model to the data
#'
#' This function fits an eXtreme Gradient Boosting model to the data
#' with the main parameters chosen through cross validation
#'
#' @param train The training data without the label.
#' @param label The training label.
#' @param n The number of folds to be used for cross validation.
#' @param k The number of complete sets of folds.
#' @param tune An integer number of default values for each parameter to be tested.
#' @param seed A single value.
#' @return A list containing the final model, the probabilities estimated by the model,
#' the AUC, the threshold selected through the ROC curve and the confusion matrix.
#' @export
XGboost.class = function(train, label, n = 10, k = 3, tune = 6, seed = 11){
  lb <- as.factor(label)
  #lb = factor(lb, levels = c("0", "1"))
  #names(lb) = "label"
  data = as.data.frame(cbind(as.data.frame(train), lb))
  tc = caret::trainControl(method = "repeatedcv", number = n, repeats = k)
  set.seed(seed)
  model = caret::train(lb~., data = data, 
                       method = "xgbTree", trControl = tc, tuneLength = tune)
  final.model = xgboost::xgboost(data = train, label = label,
                                 nrounds = model$bestTune$nrounds,
                                 max_depth = model$bestTune$max_depth,
                                 eta = model$bestTune$eta,
                                 gamma = model$bestTune$gamma,
                                 min_child_weight = model$bestTune$min_child_weight,
                                 colsample_bytree = model$bestTune$colsample_bytree,
                                 subsample = model$bestTune$subsample, 
                                 objective = "binary:logistic", verbose = 0)
  pred = predict(final.model, train)
  ROC = pROC::roc(lb, pred)
  roc.df <- data.frame(
    tpp=ROC$sensitivities*100,
    vnp=(ROC$specificities)*100,
    thresholds=ROC$thresholds)
  wmax = which.max(roc.df$tpp+roc.df$vnp)
  th = roc.df$thresholds[wmax]
  classif = ifelse(pred>=th, 1, 0)
  cm = caret::confusionMatrix(as.factor(classif), as.factor(label), positive = '1')
  l = list("Model" = final.model, "Prob" = pred, "Roc" = ROC, 
           "Threshold" = round(th,4), "Confusion.matrix" = cm)
  return(l)
}

#' Applies a fitted model to the test data
#'
#' This function applies a fitted model to predict probabilities to the test data.
#'
#' @param model The fitted model.
#' @param test The test data without the label.
#' @param label The test label.
#' @param threshold The threshold for classifying the probabilities
#' as the positive class.
#' @return A list containing the probabilities estimated by the model
#' and the confusion matrix.
#' @export
model_test = function(model, test, label, threshold){
  pred = predict(model, test)
  classif = ifelse(pred>=threshold, 1, 0)
  cm = caret::confusionMatrix(as.factor(classif), as.factor(label), positive = '1')
  l = list("Prob" = pred, "Confusion.matrix" = cm)
  return(l)
}

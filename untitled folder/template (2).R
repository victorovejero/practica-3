rm(list = ls())

create_formula <- function(variables, target="Target", data="") {
  if (data != "") {
    variables <- paste(data, "$", variables)
  }
  formula <- as.formula(paste(target, paste(variables, collapse=" + "), sep=" ~ "))
  return(formula)
} 

normalize.all <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

normalize <- function(x, series) {
  return ((x - min(series)) / (max(series) - min(series)))
}

denormalize <- function(x, series) {
  return(x*(max(series) - min(series))+min(series))
}

get_scr <- function(targets, predictions) {
  return (sum((targets - predictions) ^ 2))
}

get_sct <- function(targets, predictions) {
  mean_targets <- mean(targets)
  return(sum((targets - mean_targets) ^ 2))
}

get_sce <- function(targets, predictions) {
  mean_targets <- mean(targets)
  return(sum((predictions - mean_targets) ^ 2))
}

get_r2 <- function(targets, predictions) {
  scr <- get_scr(targets, predictions)
  sct <- get_sct(targets, predictions)
  
  r2 <- 1- scr/sct
  return(r2)
}

get_r2_adj <- function(targets, predictions, n, k) {
  r2 <- get_r2(targets, predictions)
  r2_adj <- 1 - (1 - r2) * (n-1)/(n-k-1)
  return(r2_adj)
}

get_regression_indicators <- function(target.denorm, predictions.denorm) {
  r2 <- get_r2(target.denorm, predictions.denorm)
  r2_adj <- get_r2_adj(target.denorm, 
                       predictions.denorm, 
                       nrow(test.factors),
                       ncol(test.factors) - 1)
  
  error <- target.denorm - predictions.denorm
  mse <- sum(error^2)/length(error)
  
  scr <- get_scr(target.denorm, predictions.denorm)
  sct <- get_sct(target.denorm, predictions.denorm)
  sce <- get_sce(target.denorm, predictions.denorm)
  
  cat("R2: ", r2, '\n')
  cat("R2 ajustado: ", r2_adj, '\n')
  cat("MSE: ", mse, '\n')
  cat("SCR: ", scr, '\n')
  cat("SCE: ", sce, '\n')
  cat("SCT: ", sct, '\n')
  
  plot(target.denorm, type = "p",col = "red", xlab = "Sample",
       ylab = "Target Value",
       main = "Target: Real (red) - Predicted (blue)")
  ## lines option for add_prediction 
  lines(predictions.denorm, type = "p", col = "blue")
  
  ggplot(data=as.data.frame(error), 
         mapping= aes(x=error)) + 
    geom_histogram(binwidth=0.5, col=c('blue'))
}

get_regression_performance <- function(model) {
  predictions.norm <- predict(model, test.factors)
  predictions.denorm <- denormalize(predictions.norm, data.numeric.denorm$Target)
  target.denorm <- denormalize(test.factors$Target, data.numeric.denorm$Target)
  
  get_regression_indicators(target.denorm, predictions.denorm)
}

get_regression_performance_nn <- function(model) {
  predictions.norm <- predict(model, test.dummies.inputs)
  predictions.denorm <- denormalize(predictions.norm, data.numeric.denorm$Target)
  target.denorm <- denormalize(test.dummies$Target, data.numeric.denorm$Target)
  
  get_regression_indicators(target.denorm, predictions.denorm)
}

get_class_performance <- function(predictions, targets) {
  predictions.table <- table(prediction = predictions, real = targets)
  print(caret::confusionMatrix(predictions, targets))
  
  mosaic(predictions.table,shade=T,colorize=T,gp=gpar(fill=matrix(c("green3","red2", "red2", "green3"),2,2)))
}

get_rf_variable_importance <- function(rf) {
  importance.pred <- as.data.frame(importance(rf, scale = TRUE))
  importance.pred <- rownames_to_column(importance.pred, var = "variable")
  
  print(importance.pred)
  
  p1 <- ggplot(data = importance.pred, 
               aes(x = reorder(variable, MeanDecreaseGini), 
                   y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
    labs(x = "variable", title = "MSE Reduction") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  p2 <- ggplot(data = importance.pred, 
               aes(x = reorder(variable, MeanDecreaseAccuracy), 
                   y = MeanDecreaseAccuracy, fill = MeanDecreaseAccuracy)) +
    labs(x = "variable", title = "Accuracy Reduction") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggarrange(p1,p2)
}

get_rf_variable_importance_reg <- function(rf) {
  importance.pred <- as.data.frame(importance(rf, scale = TRUE))
  importance.pred <- rownames_to_column(importance.pred, var = "variable")
  
  print(importance.pred)
  
  p1 <- ggplot(data = importance.pred, 
               aes(x = reorder(variable, `%IncMSE`), 
                   y = `%IncMSE`, fill = `%IncMSE`)) +
    labs(x = "variable", title = "MSE Reduction") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  
  p2 <- ggplot(data = importance.pred, 
               aes(x = reorder(variable, IncNodePurity), 
                   y = IncNodePurity, fill = IncNodePurity)) +
    labs(x = "variable", title = "Accuracy Reduction") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
  
  ggarrange(p1,p2)
}

get_obs_evolution <- function(rf) {
  oob_mse<-data.frame(oob_mse=rf$err.rate[,1],
                      trees=seq_along(rf$err.rate[,1]))
  
  ggplot(data=oob_mse, aes(x=trees, y=oob_mse))+
    geom_line()+
    labs(title = "Evolution of OOB vs number of trees", x="n. trees")+
    theme_bw()
}

get_gbm_variable_importance <- function(gbm.model) {
  # Plotting the influence of each predictor
  importance.pred <- summary(gbm.model, plotit = FALSE)
  ggplot(data = importance.pred, aes(x = reorder(var, rel.inf), y = rel.inf,
                                     fill = rel.inf)) +
    labs(x = "variable", title = "MSE reduction") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
}

get_neuralnet_predictions <- function(nn, data) {
  best.rep<-which.min(nn$result.matrix[1,])
  predictions.raw <- predict(nn, data, rep=best.rep, all.units=FALSE) 
  predictions.rounded <- as.data.frame(round(predictions.raw))
  predictions<-as.factor(max.col(predictions.rounded)) 
  return(predictions)
}

get_mlp_predictions <- function(nn, data) {
  best.rep<-which.min(nn$result.matrix[1,])
  predictions.raw <- predict(nn, data$inputsTest, rep=best.rep, all.units=FALSE)
  predictions<-as.factor(max.col(predictions.raw)) 
  targets <- as.factor(max.col(data$targetsTest))
  
  return(list(predictions=predictions, targets=targets))
}

get_kohmap_predictions <- function(kohmap, data) {
  predictions.raw <- predict(kohmap, newdata=data$inputsTest, whatmap=1)
  targets <- as.factor(max.col(data$targetsTest))
  
  predictions <- predictions.raw$predictions[[2]]
  
  return(list(predictions=predictions, targets=targets))
}

#Tunning the m number of predictors
tuning_rf_mtry <- function(df, y, ntree = 500){
  # This function returns the out-of-bag-MSE of a RandomForest model
  # in function of the number of predictors evaluated
  
  
  # Arguments:
  #   df = data frame with predictors and variable to predict
  #   y  = name of the variable to predict
  #   ntree = number of trees created by the randomForest algorithm
  
  require(dplyr)
  max_predictors <- ncol(df) - 1
  n_predictors   <- rep(NA, max_predictors)
  oob_mse         <- rep(NA, max_predictors)
  for (i in 1:max_predictors) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    model_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictors[i] <- i
    oob_mse[i] <- tail(model_rf$err.rate, n = 1)[1]
  }
  results <- data_frame(n_predictors, oob_mse)
  return(results)
}

get_mtry_by_oob <- function() {
  hiperparameter_mtry <-  tuning_rf_mtry(df = train.factors, y = "Target")
  hiperparameter_mtry %>% arrange(oob_mse)
  
  ggplot(data = hiperparameter_mtry, aes(x = n_predictors, y = oob_mse)) +
    scale_x_continuous(breaks = hiperparameter_mtry$n_predictors) +
    geom_line() +
    geom_point() +
    geom_point(data = hiperparameter_mtry %>% arrange(oob_mse) %>% head(1),
               color = "red") +
    labs(title = "Evolution of the out-of-bag-error vs m",
         x = "number of predictors used") +
    theme_bw()
}


#######################################################
#                     LIBRARIES
#######################################################

library("corrplot")
library(tidyverse)
library("psych") # Mult-hist
library("vcd") # Mosaic
library(ggpubr)

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library("car")  # For VIF
library(caret) # cv : SVM, NN
library(RSNNS) # NN
library(e1071) # SVM
library(randomForest) # RF
library(kohonen) # SOM
library(C50) # C4.5
library(rpart) # CART
library(rpart.plot) # CART (extra)
library(gbm) # Boosting
library(neuralnet) # NN classifier


######################################################
#           DATA LOADING AND PREPARATION
######################################################

# data.raw <- read.table("german.data")
# data.raw <- read_xlsx("<enter_you_file>")
data.raw <- read.csv("qsar_fish_toxicity.csv",sep=";",
                     stringsAsFactors = TRUE,
                     check.names = TRUE, na.strings = c("", "NA"), header=FALSE)
#library(ISLR)
#data.raw <- Hitters

data.initial <- data.raw

read.table("qsar_fish_toxicity.csv",sep=";",head=F)
# Remove NAs
data.initial <- na.omit(data.initial)

# Select target variable
data.initial$Target <- data.initial$V7
data.initial <- subset(data.initial, select=-V7)
str(data.initial)

# Remove here unwanted variables
data.initial <- subset(data.initial, select=-c(V5))
str(data.initial)

# HEY!!! STOP HERE AND CHECK IF TARGET SHOULD BE A FACTOR
# (obviously not if your doing regression)
# If target should be a factor
data.initial$Target <- as.factor(data.initial$Target)
str(data.initial)

# Normalize variables
data.numeric.denorm <- data.initial[sapply(data.initial, is.numeric)]
data.numeric.norm <- as.data.frame(normalizeData(data.numeric.denorm, type="0_1"))
colnames(data.numeric.norm) <- colnames(data.numeric.denorm)
summary(data.numeric.norm)

# If chr variables should be factors
data.chr <- data.initial[sapply(data.initial, is.character)]
data.factors.chr <- lapply(data.chr, as.factor)
data.initial[sapply(data.initial, is.character)] <- data.factors.chr
str(data.initial)

# Dataframe with factors
data.factors <- data.initial[sapply(data.initial, is.factor)]
data.dummies <- data.frame()
data.target <- data.frame(data.initial$Target)
colnames(data.target) <- c("Target")
data.dummies.target <- data.frame()

if ("Target" %in% colnames(data.factors)) {
  data.factors <- subset(data.factors, select=-Target)
  
  # Create dummy variables
  dummies.target <- dummyVars(" ~ . ", data=subset(data.initial, select=Target))
  data.dummies.target <- data.frame(predict(dummies.target, newdata=subset(data.initial, select=Target)))
  str(data.dummies.target)
} else {
  data.dummies.target <- data.frame(data.numeric.norm$Target)
  colnames(data.dummies.target) <- c("Target")
}

str(data.dummies.target)
# Create dummy variables
dummies <- dummyVars(" ~ . ", data=data.factors)
data.dummies.factors <- data.frame(predict(dummies, newdata=data.factors))
str(data.dummies.factors)

# IMPORTANT! FOR NN & SOM
data.nn.targets <- data.initial$Target
data.nn.factors <- data.factors

# Uncomment if TARGET is FACTOR
# data.nn.targets <- decodeClassLabels(data.initial$Target)

nn.dummies <- dummyVars(" ~ . ", data=data.nn.factors)
data.nn.dummies <- data.frame(predict(nn.dummies, newdata=data.nn.factors))
data.nn.inputs <- cbind(data.numeric.norm, data.nn.dummies)

data.nn.denorm <- splitForTrainingAndTest(
  data.nn.inputs,
  data.nn.targets,
  ratio=0.2
)
data.nn <- normTrainingAndTestSet(data.nn.denorm)


# Final dataframe with normalized variables and dummies
data.works.dummies.inputs <- cbind(data.dummies.factors, data.numeric.norm)
str(data.works.dummies.inputs)
data.works.dummies.labels <- data.dummies.target
str(data.works.dummies.labels)

# Final dataframe with normalized variables and factors
data.works.factors <- data.frame()
if ("Target" %in% colnames(data.factors)) {
  data.works.factors <- cbind(data.target, data.factors, data.numeric.norm)
} else {
  data.works.factors <- cbind(data.factors, data.numeric.norm)
}
str(data.works.factors)

######################################################
#                   DATA EXPLORATION
######################################################

data.exploration <- data.works.factors

str(data.exploration)
summary(data.exploration)

# Histogram
# Use this to select all numeric variables
histogram.selection <- colnames(data.numeric.norm)
# histogram.selection <- c("Target")
multi.hist(
  x=data.exploration[,histogram.selection],
  dcol=c("blue","red"),
  dlty=c("dotted", "solid"), 
  main=colnames(data.exploration[,histogram.selection]))

# Scatterplots
pairs.selection <- colnames(data.numeric.norm)
# pairs.selection <- histogram.selection
# NOTE: Change columns for dataset's columns
# pairs.selection <- c()
pairs.formula <- create_formula(pairs.selection, target="")
pairs(formula=pairs.formula, data=data.exploration, main='Data scatterplots', col=c('blue'))

# Corrplot
# NOTE: Change columns in select for the ones you want to see
corrplot.selection <- colnames(data.numeric.norm)
# corrplot.selection <- histogram.selection
# corrplot.selection <- c("Target")
corrplot.mixed(corr=cor(data.exploration[, corrplot.selection],method="pearson"), tl.pos="lt", tl.srt=45, addCoef.col = "black")

# Remove exploration variables to clear up space
rm(list = c("histogram.selection", "corrplot.selection", "pairs.selection", "pairs.formula", "data.exploration"))

######################################################
#                 TRAIN AND TEST
######################################################
set.seed(123)
train.selection <- sample(c(TRUE, FALSE), nrow(data.works.factors), prob=c(0.8, 0.2), replace=TRUE)

train.factors <- data.works.factors[train.selection, ]
train.factors.inputs <- subset(train.factors, select=-Target)
train.factors.labels <- subset(train.factors, select=Target)

train.dummies <- cbind(data.works.dummies.inputs, data.works.dummies.labels)[train.selection, ]
train.dummies.inputs <- as.matrix(data.works.dummies.inputs[train.selection,])
train.dummies.labels <- as.matrix(data.works.dummies.labels[train.selection,])

test.factors <- data.works.factors[!train.selection, ]
test.factors.inputs <- subset(test.factors, select=-Target)
test.factors.labels <- subset(test.factors, select=Target)

test.dummies <- cbind(data.works.dummies.inputs, data.works.dummies.labels)[!train.selection, ]
test.dummies.inputs <- as.matrix(data.works.dummies.inputs[!train.selection,])
test.dummies.labels <- as.matrix(data.works.dummies.inputs[!train.selection,])

train.numeric.denorm <- data.numeric.denorm[train.selection,]
test.numeric.denorm <- data.numeric.denorm[!train.selection,]

######################################################
#                       kMeans
######################################################

########################
#   CLASSIFICATION

# Elbow Method for optimum number of clusters
fviz_nbclust(train.dummies.inputs, kmeans, method = "wss", k.max = 20)
# Silhouette method
fviz_nbclust(train.dummies.inputs, kmeans, method = "silhouette")
# GAP method
fviz_nbclust(train.dummies.inputs, kmeans, method = "gap_stat")

kmeans.1 <- kmeans(
                train.dummies.inputs,
                centers=3,
                nstart=20)

# Clustering results
str(kmeans.1)

# Visualization of clusters
fviz_cluster(kmeans.1, data=train.dummies.inputs,
             choose.vars = colnames(train.dummies.inputs[, c(2,3)]))

fviz_cluster(kmeans.1, data=train.dummies.inputs,
             choose.vars = colnames(train.dummies.inputs[, c(1,7)]),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()

clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

clust <- clusters(test.dummies.inputs, kmeans.1[["centers"]])

######################################################
#                       Trees
######################################################

########################
#   CLASSIFICATION - c45

# NOTE: When using trees, Target must be a factor
tree.c45 <- C5.0(Target ~ ., data=train.factors, 
             control = C5.0Control(
                noGlobalPruning = FALSE,
                CF= 0.25)
             )
summary(tree.c45)

predictions.test <- predict(tree.c45, newdata = test.factors, type="class")
get_class_performance(predictions.test, test.factors$Target)

# Get rules
tree.c45.rules <- C5.0(Target ~ ., data=train.factors, 
                   control = C5.0Control(
                     noGlobalPruning = FALSE,
                     CF= 0.25), rules=TRUE)
summary(tree.c45.rules)

########################
#   CLASSIFICATION - CART

tree.cart <- rpart(Target ~ ., data=train.factors)
print(tree.cart)
summary(tree.cart)


# To find best CP
printcp(tree.cart, digits=4) # Look for lowest XERROR
plotcp(tree.cart, lty=2, col="red", upper="size" )

tree.cart <- rpart(Target ~ ., data=train.factors, cp=0.033)

# Get rules
rpart.rules(tree.cart, style = "tall", cover=TRUE, nn=TRUE, clip.facs = TRUE)

predictions.test <- predict(tree.cart, newdata = test.factors, type="class")
get_class_performance(predictions.test, test.factors$Target)

########################
#   REGRESSION - CART

tree.cart <- rpart(Target ~ ., data=train.factors)
print(tree.cart)
summary(tree.cart)

get_regression_performance(tree.cart)

######################################################
#                   Random Forests
######################################################

########################
#   CLASSIFICATION 

rf <- randomForest(
                formula=Target ~ ., 
                data=train.factors, 
                mtry=(ncol(train.factors)-1),
                importance=TRUE,
                norm.vote=TRUE)
print(rf)

predictions.test <- predict(rf, newdata = test.factors, type="class")
get_class_performance(predictions.test, test.factors$Target)

get_rf_variable_importance(rf)
get_obs_evolution(rf)
get_mtry_by_oob()

rf.mod <- randomForest(
      formula=Target ~ ., 
      data=train.factors, 
      mtry=5,
      ntree=200,
      importance=TRUE,
      norm.vote=TRUE)

print(rf.mod)

predictions.test <- predict(rf.mod, newdata = test.factors, type="class")
get_class_performance(predictions.test, test.factors$Target)

########################
#   REGRESSION

rf <- randomForest(
    formula=Target ~ ., 
    data=train.factors, 
    mtry=5,
    importance=TRUE,
    norm.vote=TRUE
)
print(rf)

get_rf_variable_importance_reg(rf)
get_regression_performance(rf)

######################################################
#                     BOOSTING
######################################################

########################
#   CLASSIFICATION

gbm.model <- gbm(Target ~ ., data = train.factors,
                 distribution = "gaussian",
                 n.trees = 2000,
                 interaction.depth = 5,
                 shrinkage = 0.01,
                 n.minobsinnode = 1,
                 bag.fraction = 0.5)

best.iter = gbm.perf(gbm.model, method="OOB")

predictions.test <- predict(gbm.model, newdata = test.factors)
# Boosting gives you numbers, not factors
# Transform numbers into factors
predictions.test.factors <- as.factor(ifelse(predictions.test >= 1.5, 2, 1))
get_class_performance(predictions.test.factors, test.factors$Target)

get_gbm_variable_importance(gbm.model)

########################
#   CLASSIFICATION (CV)

# NOTE: This section will teake a long time to run
# so it probably will not be necessary

set.seed(123)
validation <- trainControl(
  method = "cv",
  number = 10)

tuning.grid <- expand.grid(interaction.depth = c(1, 5, 9),
                           n.trees = c(100, 1000, 2000, 3000),
                           shrinkage = c(0.1, 0.01, 0.001),
                           n.minobsinnode = c(1, 10, 20))

set.seed(123)
gbm.best.model <- caret::train(
                    Target ~ ., data = train.factors,
                    method = "gbm",
                    trControl = validation,
                    verbose = FALSE,
                    tuneGrid = tuning.grid)

gbm.best.model$bestTune

########################
#   REGRESSION

gbm.model <- gbm(Target ~ ., data = train.factors,
                 distribution = "gaussian",
                 n.trees = 2000,
                 interaction.depth = 5,
                 shrinkage = 0.01,
                 n.minobsinnode = 1,
                 bag.fraction = 0.5)

best.iter = gbm.perf(gbm.model, method="OOB")

get_regression_performance(gbm.model)

######################################################
#                   Neural Nets
######################################################

########################
#   CLASSIFICATION

nn.1 <- neuralnet(Target.1+Target.2 ~.,
                  data=train.dummies,
                  hidden=c(5,5),
                  lifesign="minimal",
                  linear.output=FALSE,
                  rep=10)

predictions <- get_neuralnet_predictions(nn.1, test.dummies)
get_class_performance(predictions, test.factors$Target)

########################
#   CLASSIFICATION
# HEYYYY! THIS METHOD IS MUCH FASTER
nn.2 <- mlp(data.nn$inputsTrain, data.nn$targetsTrain,
            size=c(5,5),
            learnFuncParams = 0.1,
            maxit = 250,
            inputsTest = data.nn$inputsTest,
            targetsTest = data.nn$targetsTest)

info <- get_mlp_predictions(nn.2, data.nn)
get_class_performance(info$predictions, info$targets)

########################
#   REGRESSION

nn.1<-mlp(train.dummies.inputs, train.dummies.labels, 
               size=c(5,3),
               initFunc="Randomize_Weights",
               initFuncParams=c(-0.3, 0.3),
               learnFunc="Std_Backpropagation",
               learnFuncParams=c(0.2, 0.0),
               maxit = 350,
               updateFunc="Topological_Order",
               hiddenActFunc="Act_Logistic",
               linOut=TRUE,
               inputsTest = test.dummies.inputs, 
               targetsTest = test.dummies.labels)

get_regression_performance_nn(nn.1)

######################################################
#                       SOM
######################################################

########################
#   EXPLORATION

som.grid <-somgrid(xdim=5, ydim=4, topo="hexagonal")

som.1 <- som(data.nn$inputsTrain, grid=som.grid,
             rlen=100, alpha=c(0.05, 0.01),
             radius= 2, keep.data=TRUE)
names(som.1)
summary(som.1)

plot.def <- par()

# Evolution on mapping
plot(som.1, type="changes")
# Examples per neurons
plot(som.1, type="counts", main="Number of examples per neuron")
# Patterns (only works with few variables)
plot(som.1, type="codes", main="Patterns discovered")
# Plotting quality
plot(som.1, type="quality", main="Node Quality/Distance")
# Examples to neurons
plot(som.1, type="mapping", main="Data mapping to the neurons")

code <- 3
plot(som.1, 
     type="property", 
     property=getCodes(som.1, 1)[,code], 
     main=colnames(getCodes(som.1, 1))[code], 
     palette.name=heat.colors)

# Clustering patterns in the map
groups <- 2
# Applying hierarchical clustering for grouping patterns
som.hc=cutree(hclust(dist(som.1$codes[[1]])), groups) 
plot(som.1, type="codes", bgcol = rainbow(groups)[som.hc]) 
add.cluster.boundaries(som.1,som.hc)

########################
#   CLASSIFICATION

kohmap <- xyf(data.nn$inputsTrain, data.nn$targetsTrain,
              grid=som.grid,
              rlen=100, alpha=c(0.05, 0.01),
              radius= 2, keep.data=T)

# Patterns (only works with few variables)
plot(kohmap, type="codes", main=c("Patterns", "Target"))
# Classes per neuron
plot(kohmap, type="mapping", 
     labels=as.numeric(data.nn$targetsTrain), 
     col=as.numeric(data.nn$targetsTrain)+1, pch=1, main="Map of classes")

info <- get_kohmap_predictions(kohmap, data.nn)
get_class_performance(info$predictions, info$targets)

######################################################
#                       SVM
######################################################

########################
#   CLASSIFICATION SIMPLE

svm.1 <- svm(
  formula= Target ~ ., 
  data=train.factors, 
  kernel="radial", 
  cost=10, 
  scale=FALSE
)

predictions <- predict(svm.1, test.factors)
get_class_performance(predictions, test.factors$Target)

########################
#   CLASSIFICATION CV
# NOTE: This will take a long time to run

svm.test <- tune(
  "svm",            
  Target~.,
  data=train.factors,
  kernel="radial",
  ranges=list(
    epsilon=seq(0,1,0.1),
    cost=c(0.01, 0.1, 1, 5, 10, 20),
    gamma=c(0.1, 0.5, 1, 2, 5, 10))
)

ggplot(
  data=svm.test$performances, 
  aes(x=cost, y=error, color=factor(gamma)))+
  geom_line()+
  geom_point()+
  labs(title="Classification error vs C & gamma")+
  theme_bw()+theme(legend.position ="bottom")

svm.best <- svm.test$best.model

########################
#   REGRESSION

svm.1 <- svm(
  formula= Target ~ ., 
  data=train.factors, 
  kernel="radial", 
  cost=10, 
  scale=FALSE
)

get_regression_performance(svm.1)
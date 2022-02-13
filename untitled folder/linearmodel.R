raw_data <- read.csv("qsar_fish_toxicity.csv",sep=";",
                     stringsAsFactors = TRUE,
                     check.names = TRUE, na.strings = c("", "NA"), header=FALSE)

data_noNA <- na.omit(raw_data)
summary(data_noNA)


set.seed(263)
sample <- sample(c(TRUE, FALSE),
                 nrow(data_noNA), replace =
                   T, prob = c(0.8,0.2)) #70% & 30%
train <- data_noNA[sample, ]
test <- data_noNA[!sample, ]

# Ahora comenzamos con el modelo, empezaremos comprobando como de bien describen las 
# variables con mas correlación al Salary.

model <- lm(V7~.-V5,data=train)

summary(model)

vif(model)


predictions <- predict(model,new=train)
res <- train$V7-predictions

hist(res)
summary(res)



predictions2 <- predict(model,new=test)
res <- test$V7-predictions2

hist(res)
summary(res)



# PREDICCIONES INDIVIDUALES
predict1 <- model$coefficients %*% c(2.217,0.845,0.793,1,0,2.429)
predict1

predict2 <- model$coefficients %*% c(4.532,0.405,1.189,2,2,4.639)
predict2

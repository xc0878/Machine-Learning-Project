
library(ISLR)
library(glmnet)
data=read.csv("basement.csv")[,-1]
data=data[1:1460,]
obs=1460
#data=read.csv("train12_git.csv")[,-1]
#Need matrices for glmnet() function. Automatically conducts conversions as well
#for factor variables into dummy variables.
x = model.matrix(SalePrice ~ ., data)[, -1] #Dropping the intercept column.
y = data$SalePrice

grid = 10^seq(5, -2, length = 1000)


ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.models)) #20 different coefficients, estimated 100 times --
#once each per lambda value.
coef(ridge.models) #Inspecting the various coefficient estimates.

#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

#Creating training and testing sets. Here we decide to use a 70-30 split with
#approximately 70% of our data in the training set and 30% of our data in the
#test set.
set.seed(0)
train = sample(1:obs, 7*obs/10)
test = (-train)
#test=c(1:obs)
#final=c(1461:2919)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

#Let's attempt to fit a ridge regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])
mean((ridge.lambda5 - y.test)^2)


#Running 10-fold cross validation.
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)

#What is the test MSE associated with this best value of lambda?
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)

#Here the MSE is lower at approximately 113,173; a further improvement
#on that which we have seen above. With "cv.ridge.out", we can actually access
#the best model from the cross validation without calling "ridge.models.train"
#or "bestlambda.ridge":
ridge.bestlambdatrain = predict.cv.glmnet(cv.ridge.out, s ="lambda.min", newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)

### Alternative method with caret
library(caret)
set.seed(0)
train_control = trainControl(method = 'cv', number=10)
tune.grid = expand.grid(lambda = grid, alpha=c(0))
ridge.caret = train(x[train, ], y[train],
                    method = 'glmnet',
                    trControl = train_control, tuneGrid = tune.grid)

### Plot the tuning object:
plot(ridge.caret, xTrans=log)

### We see caret::train returns a different result from the
### one by cv.glmnet. By comparing the ridge.caret$results
### and cv.ridge.out$cvm, it's most likely to be rounding and 
### averaging.

### Predicting with the final model
pred = predict.train(ridge.caret , newdata = x[test,])
mean((pred - y[test])^2)
mean(100*abs(pred - y[test])/y[test])



##########################
#####Lasso Regression#####
##########################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)
#lasso.models = glmnet(x, y, alpha = 1, lambda = bestlambda.lasso)
 dim(coef(lasso.models)) #20 different coefficients, estimated 100 times --
#once each per lambda value.
coef(lasso.models) #Inspecting the various coefficient estimates.

#What do the estimates look like for a smaller value of lambda?
lasso.models$lambda[80] #Lambda = 0.2595.
coef(lasso.models)[, 80] #Most estimates not close to 0.
sum(abs(coef(lasso.models)[-1, 80])) #L1 norm is 228.1008.

#What do the estimates look like for a larger value of lambda?
lasso.models$lambda[15] #Lambda = 10,235.31.
coef(lasso.models)[, 15] #Estimates all 0.
sum(abs(coef(lasso.models)[-1, 15])) #L1 norm is essentially 0.

#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

#Can use the predict() function to obtain lasso regression coefficients for a
#new value of lambda, not necessarily one that was within our grid:
predict(lasso.models, s = 50, type = "coefficients")

#Let's attempt to fit a lasso regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
mean((lasso.lambda5 - y.test)^2)

#Here, the MSE is approximately 107,660.

#Instead of arbitrarily choosing random lambda values and calculating the MSE
#manually, it's a better idea to perform cross-validation in order to choose
#the best lambda over a slew of values.

#Running 10-fold cross validation.
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

#This time the MSE is actually higher at approximately 113,636. What happened?
x0=1965392410
set.seed(0)
### Exercise: Tune the same lasso model with caret!
for (i in 1:10){
  set.seed(0)
  cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = i/10, nfolds = 10)
  bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
x1=mean((lasso.bestlambdatrain - y.test)^2)
if (x1<=x0)
{x0=x1
  print(paste("alpha=",i, "lambda=",cv.lasso.out$lambda.min))
  print(paste("MSE=",x0))} 
}
#prices=data.frame(data$SalePrice)[final,]
library(dplyr)
#prices<- prices %>% mutate(., "predict"=lasso.bestlambdatrain)
#prices<-prices %>% mutate(., "residual"=data.SalePrice-predict)
mean(abs(y.test-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,]))*100/y.test)
data3=read.csv("attic.csv")
data3$SalePrice=data4

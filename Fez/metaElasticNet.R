


# Working with glmnet, importing file. Set variable "obs" to number of observations in the training data

library(glmnet)
data=read.csv("basementM.csv")[,-1]

#m1=read.csv("elasticnet.csv")[,-1]
#m2=read.csv("xingXG.csv")[,-1]
#m3=read.csv("xingRF.csv")[,-1]
#data['m1']=log(m1)
#data['m2']=log(m2)
#data['m3']=log(m3)
data['m1']=log(data['m1'])
data['m2']=log(data['m2'])
data['m3']=log(data['m3'])
data['SalePrice']=log(data['SalePrice'])
obs=1458
data=data[1:obs,]

# x puts the data into a form compatible with glmnet methods. y is shown to be skewed and therefore log transformed

x = model.matrix(SalePrice ~ ., data)[, -1] 
y = data$SalePrice
hist(y)
#y=log(y)

# range of lambda values for grid search

grid = 10^seq(5, -2, length = 100)
ridge.models = glmnet(x, y, alpha = 0, lambda = grid)

dim(coef(ridge.models)) #415 different coefficients, estimated 100 times --
#once each per lambda value.
coef(ridge.models) #Inspecting the various coefficient estimates.

#Visualizing the ridge regression shrinkage.
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

# Sampling 70% of the trainign data for training, the rest for testing the model  

set.seed(0)
train = sample(1:obs, 7*obs/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)

#Let's attempt to fit a ridge regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
ridge.models.train = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
ridge.lambda5 = predict(ridge.models.train, s = 5, newx = x[test, ])
sqrt(mean((ridge.lambda5 - y.test)^2))


#Running 10-fold cross validation on ridge model.
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 0, nfolds = 10)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)

#What is the test MSE associated with this best value of lambda?
ridge.bestlambdatrain = predict(ridge.models.train, s = bestlambda.ridge, newx = x[test, ])
sqrt(mean((ridge.bestlambdatrain - y.test)^2))


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
sqrt(mean((pred - y[test])^2))


##########################
#####Lasso Regression#####
##########################
#Fitting the lasso regression. Alpha = 1 for lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda = grid)



#Visualizing the lasso regression shrinkage.
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")


#Let's attempt to fit a lasso regression using some arbitrary value of lambda;
#we still have not yet figured out what the best value of lambda should be!
#We will arbitrarily choose 5. We will now use the training set exclusively.
lasso.models.train = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
lasso.lambda5 = predict(lasso.models.train, s = 5, newx = x[test, ])
sqrt(mean((lasso.lambda5 - y.test)^2))



#Running 10-fold cross validation for lambda of lasso model
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = 1, nfolds = 10)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)

#What is the test MSE associated with this best value of lambda?
lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lasso, newx = x[test, ])
sqrt(mean((lasso.bestlambdatrain - y.test)^2))

# Runnign 10-fold cross-validation for Elastic Net model parameters alpha, lambda

x0=1965392410
alph=0

for (i in 1:10){
  set.seed(0)
  cv.lasso.out = cv.glmnet(x[train, ], y[train],
                         lambda = grid, alpha = i/10, nfolds = 10)
  bestlambda.lassot = cv.lasso.out$lambda.min


  lasso.models.train = glmnet(x[train, ], y[train], alpha = i/10, lambda = grid)
  lasso.bestlambdatrain = predict(lasso.models.train, s = bestlambda.lassot, newx = x[test, ])
  x1=mean((lasso.bestlambdatrain - y.test)^2)
print(mean(abs(y.test-predict(lasso.models.train, s = bestlambda.lassot, newx = x[test,]))*100/y.test))
print(paste("alpha=",i, "lambda=",cv.lasso.out$lambda.min))
print(paste("rMSE=",sqrt(x1)))

if (x1<=x0)
  {x0=x1
  bestlambda.lasso=bestlambda.lassot
  alph=i/10
  lasso.models.train = glmnet(x[train, ], y[train], alpha = i/10, lambda =bestlambda.lasso)
  print(paste("Set alpha=",i, ",lambda=",cv.lasso.out$lambda.min))
 } 
}

lasso.models.train = glmnet(x[train, ], y[train], alpha = alph, lambda = grid)
mean(abs(y.test-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,]))*100/y.test)

#rmse for elastic net model

print(paste("rMSEl=",sqrt(mean((predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,]) - y.test)^2))))

# Boosting elastic net by adding predicted SalesPrice values to x data, using epsilon (residuals) as y

epsilon=y[train]-predict(lasso.models.train, s = bestlambda.lasso, newx = x[train,])
epsilon1=y[test]-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,])

data1=data[1:obs,]
data1["SalePrice"]=predict(lasso.models.train, s = bestlambda.lasso, newx = x)
data1[,'epsilon']=integer(obs)
for (j in row.names(epsilon)){
  data1[j,'epsilon']=epsilon[row.names(epsilon)==j,]
}
for (j in row.names(epsilon1)){
  data1[j,'epsilon']=epsilon1[row.names(epsilon1)==j,]
}
x1 = model.matrix(epsilon ~ ., data1)[, -1] #Dropping the intercept column.
y1 = data1$epsilon
write.csv(data1, "epsilon.csv")


set.seed(0)
train1 = sample(1:nrow(x1), 7*nrow(x1)/10)
test1 = (-train1)
#test=c(1:obs)
#final=c(1461:2919)
y.test1 = y1[test1]

length(train1)/nrow(x1)
length(y.test1)/nrow(x1)
grid = 10^seq(5, -2, length = 100)
lasso.models.train1 = glmnet(x1[train1, ], y1[train1], alpha = 1, lambda = grid)
x00=1965392410
alph2=0
### Grid search for elastic net parameters of epsilon model
for (i in 1:10){ 
  set.seed(0)
  cv.lasso.out = cv.glmnet(x1[train1, ], y1[train1],
                           lambda = grid, alpha = i/10, nfolds = 10)
  bestlambda.lasso1t = cv.lasso.out$lambda.min
  alph2=i/10
  lasso.models.train1 = glmnet(x1[train1, ], y1[train1], alpha = alph2, lambda = grid)
  lasso.bestlambdatrain = predict(lasso.models.train1, s = bestlambda.lasso1t, newx = x1[test1, ])
  x11=mean((lasso.bestlambdatrain - y.test1)^2)
  print(mean(abs((y.test1-predict(lasso.models.train1, s = bestlambda.lasso1t, newx = x1[test1,]))*100/y.test1)))
  print(paste("alpha=",i, "lambda=",cv.lasso.out$lambda.min))
  print(paste("MSE=",x11))
  print(paste("rMSE=",sqrt(x11)))
  
  if (x11<=x00)
  {x00=x11
  bestlambda.lasso1=bestlambda.lasso1t
  lasso.models.train1 = glmnet(x1[train1, ], y1[train1], alpha = i/10, lambda =bestlambda.lasso1)
  print(paste("alpha=",i, "lambda=",cv.lasso.out$lambda.min))
  print(paste("MSE=",x00))} 
}
data1["predictedE"]=predict(lasso.models.train1, s = 1202, newx = x1)
write.csv(data1, "epsilon.csv")
lasso.models.train1 = glmnet(x1[train1, ], y1[train1], alpha = alph2, lambda = grid)
mean(abs((y.test1-predict(lasso.models.train1, s = bestlambda.lasso1, newx = x1[test1,]))*100/y.test1))
mean(abs(y.test-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,])-predict(lasso.models.train1, s = bestlambda.lasso1, newx = x1[test,]))*100/y.test)
mean(abs(y.test-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,])-predict(lasso.models.train1, s = bestlambda.lasso1, newx = x1[test,]))*100/y.test)

# rmse of boosted model

sqrt(mean((y.test-predict(lasso.models.train, s = bestlambda.lasso, newx = x[test,])+predict(lasso.models.train1, s = bestlambda.lasso1, newx = x1[test,]))^2))

# applying the boosted elastic net to testing data for submission

data=read.csv("basementM.csv")[,-1]

#m1=read.csv("elasticnet.csv")[,-1]
#m2=read.csv("xingXG.csv")[,-1]
#m3=read.csv("xingRF.csv")[,-1]
#data['m1']=log(m1)
#data['m2']=log(m2)
#data['m3']=log(m3)
data['m1']=log(data['m1'])
data['m2']=log(data['m2'])
data['m3']=log(data['m3'])
data['SalePrice']=log(data['SalePrice'])
data=data[1459:2917, ]

x = model.matrix(SalePrice ~ ., data)[, -1] 
y = data$SalePrice

Prices=exp(predict(lasso.models.train, s = bestlambda.lasso, newx = x))

data["SalePrice"]=predict(lasso.models.train, s = bestlambda.lasso, newx = x)
data[,'epsilon']=integer(2917-obs)

epsilon=y-predict(lasso.models.train, s = bestlambda.lasso, newx = x)
epsilon1=y-predict(lasso.models.train, s = bestlambda.lasso, newx = x)

x = model.matrix(SalePrice ~ ., data)[, -1] 
y = data$SalePrice



for (j in row.names(epsilon)){
  data[j,'epsilon']=epsilon[row.names(epsilon)==j,]
}
for (j in row.names(epsilon1)){
  data[j,'epsilon']=epsilon1[row.names(epsilon1)==j,]
}
x1 = model.matrix(epsilon ~ ., data)[, -1] 
y1 = data1$epsilon


Prices=Prices-exp(predict(lasso.models.train1, s = bestlambda.lasso1, newx = x))



write.csv(data.frame(Prices), "submission.csv")

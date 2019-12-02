library(tidyverse)
library(ggplot2)
library(MICE)
library(corrr)
library(corrplot)
library(ggplot2)
library(ade4)
library(data.table)
library(knitr)

train = read.csv("train.csv")
test = read.csv("test.csv")

dim(train)
dim(test)


str(train)
str(test)

checkMiss(train)

train = read.csv("train.csv", stringsAsFactors = FALSE) 
test = read.csv("test.csv", stringsAsFactors = FALSE) 

SalePrice = train$SalePrice 
train$SalePrice = NULL



###---- DO NOT USE, TURNS TO NON_NUMERIC-----###

for (col in colnames(train)){
  if (typeof(train[,col]) == "character"){
    new_col = train[,col]
    new_col[is.na(new_col)] = "missing"
    train[col] = as.factor(new_col)
  }
}

##---- DO NOT USE, TURNS TO NON_NUMERIC-----###




#----- Fill in all N/A with -1 in test & train ----#

train[is.na(train)] = -1
test[is.na(test)] = -1

#----- Fill in all N/A with -1 in test & train ----#

head(train, 50)

str(train)

#-------------------------- Check for Missingness ---------------------------#


checkMiss <- function(tr){
  
  vars <- ""
  perc <- 0
  type <- ""
  
  for(i in 1: ncol(train)){
    if(anyNA(tr[,i])){
      vars <- c(vars, names(tr)[i])
      type <- c(type, "train")
      perc <- c(perc, length(which(is.na(tr[,i]))) / (nrow(tr) + nrow(tr)))
      
    }
  }
  
  if(length(vars) > 1){
    
    vars <- vars[-1]
    type <- type[-1]
    perc <- perc[-1]
    
    vars <- factor(vars)
    
    naData <- data.frame(variables = vars, type = type, percentage = perc)
    naData$variables <- factor(naData$variables, 
                               levels = naData$variables[order(naData$perc)])
    plot <- ggplot(naData,
                   aes(x=variables, y=percentage, 
                       fill = type)) + geom_bar(stat = "identity") + xlab("Variable") + ylab("Percent missing") + ggtitle("Percentage of Missing Values in Training/Test Sets") + coord_flip() + geom_hline(yintercept = 0.05)
    print("Checking NA process completed.")
    return(plot)
  }
  else
    print("Checking NA process completed.")
  
}
checkMiss(train)

# ----------------------- See plot for results -----------------------#


#-------------------------- Time to Find Correllation ---------------------------#

library(corrr)
library(corrplot)
library(ggplot2)

cor(hd$SalePrice, hd$LotArea)
cor(hd$SalePrice, hd$MSSubClass)
cor(hd$SalePrice, hd$GarageArea)
cor(hd$SalePrice, hd$GarageCars)
cor(hd$SalePrice, hd$GarageYrBlt) # Shoudl we return an age of garage? Instead of a year?
cor(hd$SalePrice, hd$MoSold)
cor(hd$SalePrice, hd$GarageType)
cor(hd$SalePrice, hd$MSSubClass)
cor(hd$SalePrice, hd$HouseStyle)


head(hd, 10)

#-------------------------- Time to Find Correllation on _train_ ---------------------------#


cor(train$SalePrice, train$LotArea)
cor(train$SalePrice, train$MSSubClass)
cor(train$SalePrice, train$GarageArea)
cor(train$SalePrice, train$GarageCars)
cor(train$SalePrice, train$GarageYrBlt) # Shoudl we return an age of garage? Instead of a year?
cor(train$SalePrice, train$MoSold)
cor(train$SalePrice, train$GarageType)
cor(train$SalePrice, train$Exterior)
cor(train$SalePrice, train$MSSubClass)
cor(train$SalePrice, train$HouseStyle)



#----------------------- Top 10 THAT CORRELLATE 50% or more to $SalePrice ------------------------#

cor(train$SalePrice, train$OverallQual)
cor(train$SalePrice, train$GrLivArea)
cor(train$SalePrice, train$GarageCars)
cor(train$SalePrice, train$GarageArea)
cor(train$SalePrice, train$TotalBsmtSF)
cor(train$SalePrice, train$X1stFlrSF)
cor(train$SalePrice, train$FullBath)
cor(train$SalePrice, train$YearBuilt)
cor(train$SalePrice, train$YearRemodAdd)

#----------------------- END Top 10 VARIABLES THAT CORRELLATE 50% or more to $SalePrice ------------------------#




#---- Total Correllation over 20% to SalePrice) - Top 22 Items by Correllation  -----#

for (col in colnames(train)){
  if(is.numeric(train[,col])){
    if( abs(cor(train[,col],train$SalePrice)) > 0.20){
      print(col)
      print( cor(train[,col],train$SalePrice) )
    }
  }
}

summary(train)

#--------- I Suggest keepting these - Top 22 (20%) Items by Correllation  -----------#




#--------- Total Correllation under 20% to SalePrice) - Drop these -----------#

for (col in colnames(train)){
  if(is.numeric(train[,col])){
    if( abs(cor(train[,col],train$SalePrice)) < 0.20){
      print(col)
      print( cor(train[,col],train$SalePrice) )
    }
  }
}

#--------- Total Correllation under 20% to SalePrice) - Drop these -----------#



#----------------------- Cross Correllation (Variable to Variable)  ------------------------#
# > 60% Correllation only #

cors = cor(train[ , sapply(train, is.numeric)]) 
high_cor = which(abs(cors) > 0.334 & (abs(cors) < 1))
rows = rownames(cors)[((high_cor-1) %/% 38)+1]
cols = colnames(cors)[ifelse(high_cor %% 38 == 0, 38, high_cor %% 38)]
vals = cors[high_cor]

cor_data = data.frame(cols=cols, rows=rows, correlation=vals)
cor_data

# Garage Cars & Garage Area have the closest correllation to one another, so I am dropping one of them
#----------------------- Cross Correllation (Variable to Variable)  ------------------------#



#------------- Density Plots ----------------#

for (col in colnames(train)){
  if(is.numeric(train[,col])){
    plot(density(train[,
                       col]), main=col)
  }
}



#----------- End Density Plots --------------#




#################################################################### 
########################## BEGIN DROPPING ##########################
#################################################################### 


# -------- Begin Numerical Variables to Drop (<20% Correllation is dropped) ----------#

select (train,-c(Id,MSSubClass,OverallCond,BsmtFinSF2,LowQualFinSF,
                 BsmtHalfBath,BedroomAbvGr,KitchenAbvGr,EnclosedPorch,
                 X3SsnPorch,ScreenPorch,PoolArea,MiscVal,MoSold,YrSold))

# --------- End Numerical Variables to Drop (<20% Correllation is dropped)   -----------#


# ---------- Cleaning the Non-Numerical Values -----------#

test$Exterior1st[is.na(test$Exterior1st)] = "VinylSd" # Replace all missing with the 'Mode' --> VinylSd. 


train <- train %>% 
  mutate(BsmtExposure = ifelse(is.na(BsmtExposure), "noBsmt", BsmtExposure),
         BsmtFinished = ifelse(BsmtUnfSF == 0 & BsmtExposure != "noBsmt", 1, 
                               ifelse(BsmtExposure == "noBsmt", "noBsmt", 0)))

train[train$BsmtExposure == "noBsmt" & is.na(train$BsmtFinType1) & is.na(train$BsmtFinType2),] <- 
  train %>%
  filter(BsmtExposure == "noBsmt" & is.na(BsmtFinType1) & is.na(BsmtFinType2)) %>% 
  mutate(BsmtQual = "noBsmt",
         BsmtCond  = "noBsmt",
         BsmtFinType1 = "noBsmt",
         BsmtFinSF1 = 0,
         BsmtFinType2 = "noBsmt",
         BsmtFinSF2 = 0,
         BsmtUnfSF = 0,
         TotalBsmtSF = 0,
         BsmtFullBath = 0,
         BsmtHalfBath = 0,
         BsmtFinished = 1)



str(train)


train %>% 
  select(contains("Bsmt")) %>% 
  filter(BsmtExposure == "noBsmt" & is.na(BsmtFinType1) & is.na(BsmtFinType2))


train[train$BsmtExposure == "noBsmt" & is.na(train$BsmtFinType1) & is.na(train$BsmtFinType2),] <- 
  train %>%
  filter(BsmtExposure == "noBsmt" & is.na(BsmtFinType1) & is.na(BsmtFinType2)) %>% 
  mutate(BsmtQual = "noBsmt",
         BsmtCond  = "noBsmt",
         BsmtFinType1 = "noBsmt",
         BsmtFinSF1 = 0,
         BsmtFinType2 = "noBsmt",
         BsmtFinSF2 = 0,
         BsmtUnfSF = 0,
         TotalBsmtSF = 0,
         BsmtFullBath = 0,
         BsmtHalfBath = 0,
         BsmtFinished = 1)


train %>% select(contains("Bsmt")) %>% 
  filter(is.na(BsmtCond))


train$BsmtCond[is.na(train$BsmtCond)] <- factor("TA")


train %>% select(contains("Bsmt"), YearBuilt) %>% 
  filter(is.na(BsmtQual))


train[is.na(train$BsmtQual), ]$BsmtQual <- factor("TA")


train %>% select(Id, contains("Bsmt")) %>% 
  filter(is.na(BsmtFinType2))


train[is.na(train$BsmtFinType2), ]$BsmtFinType2 <- factor("Unf")


# --- Garages ---#
train %>% 
  filter(is.na(GarageType)) %>%
  select(contains("garage")) %>% 
  map(function(x) summary(factor(x)))


train <- train %>% 
  mutate(GarageType = ifelse(is.na(GarageType), "noGarage", GarageType))

train %>% 
  select(contains("garage")) %>% 
  group_by(GarageType) %>% 
  summarise_all(.funs = list(na.count = function(x) sum(is.na(x))))

train %>% 
  ggplot() +
  geom_point(aes(x = GarageYrBlt, y = SalePrice/1000, color = GarageType)) +
  ylab("SalePrice in 1000 $")


train2 = subset(train, select = -c(GarageType,GarageYrBlt,GarageFinish,GarageArea))


# ------------- Train2 has only GarageCars Variable now ---------------#




# --------- Pools, Fences, Fireplaces & Alley ----------- #

summary(factor(train2$PoolQC))

train2 %>% 
  filter(PoolArea > 0) %>% 
  select(Id, PoolQC, PoolArea)

train2 %>% 
  group_by(PoolQC) %>% 
  summarise(mean(PoolArea),
            median(PoolArea))

train2 <- train2 %>% 
  mutate(PoolQC = ifelse(is.na(PoolQC), "noPool", PoolQC),
         PoolArea = ifelse(is.na(PoolQC), 0, PoolArea))


train2 <- train2 %>% mutate(Alley = ifelse(is.na(Alley), "none", Alley))

train2 <- train2 %>% mutate(Fence = ifelse(is.na(Fence), "none", Fence))



# Fireplaces

train %>% 
  filter(is.na(FireplaceQu)) %>% 
  select(Fireplaces) %>% summary

train2 <- train2 %>% mutate(FireplaceQu = ifelse(is.na(FireplaceQu), "none", FireplaceQu))




# Obvious Drops with no correllation to SP

train3 = subset(train, select = -c(MasVnrArea,MasVnrType,LotFrontage,Condition2,OverallCond,GarageFinish,GarageArea,MasVnrArea,Exterior2nd))


# ============ One Hot Encoding ============= #

library(ade4)
library(data.table)
one_hot = c('Neighborhood', 'HouseStyle', 'RoofStyle')
for (f in one_hot){
   train9_dummy = acm.disjonctif(train9[ ,f])
   train9[ ,f] = NULL
   train10 = cbind(train9, train9_dummy)
}

summarise(train10)


# --------- Not Running --------- #


# Check what I have now
write.csv(train3, file = "train3_check.csv",row.names=FALSE)



# ========= More Variable to Drop ================ #
train4 = subset(train3, select = -c(LandContour,Utilities,LotConfig,LandSlope,Condition1,BsmtFinType1,BsmtExposure))
train5 = subset(train4, select = -c(BsmtFinType2,BsmtUnfSF,X1stFlrSF,X2ndFlrSF,LowQualFinSF,GrLivArea,Functional))
train5 = subset(train4, select = -c(GarageType,X3SsnPorch,ScreenPorch,PoolArea,Fence,MiscFeature,SaleCondition))
train6 = subset(train5, select = -c(Alley,LotShape,KitchenAbvGr,TotRmsAbvGrd,Functional))
train7 = subset(train6, select = -c(Street,MSSubClass))
train8 = subset(train7, select = -c(MoSold,MiscVal,GarageCond,WoodDeckSF,OpenPorchSF,EnclosedPorch,PoolQC,GarageCond,FireplaceQu))
train9 = subset(train8, select = -c(BsmtFinSF2,RoofStyle))
train10 = subset(train9, select = -c(ExterCond))

# Check what I have now
train5
write.csv(train3, file = "train3_check.csv",row.names=FALSE)
write.csv(train6, file = "train6_check.csv",row.names=FALSE)
write.csv(train7, file = "train7.csv",row.names=FALSE)
write.csv(train8, file = "train8.csv",row.names=FALSE)
write.csv(train9, file = "train9.csv", row.names=FALSE)
write.csv(train10, file = "train10.csv", row.names=FALSE)

sum(train10)

train10




# ========== Begin 3D Maps ========== #

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(dplyr)
library(rvest)
library(magrittr)
library(ggthemes)


usa <- map_data("usa")
states <- map_data("state")
mn_df <- subset(states, region == "minnesota")
counties <- map_data("county")
mn_county <- subset(counties, region == "minnesota")
mn_county$pos <- 1:nrow(mn_county)

webpage <- read_html("https://en.wikipedia.org/wiki/List_of_counties_in_Minnesota")
tbls <- html_nodes(webpage, "table")
# Shows all tables
tbls

wiki <- html_table(tbls[1],fill = T)[[1]]
# Remove Citations in Column Names
names(wiki) <- gsub("\\[.*$","",names(wiki))
# Convert to Numeric and Remove Weird Characters
wiki$Population <- wiki$Population %>% 
  gsub("^.*♠","",.) %>%
  gsub("[^0-9\\.]","",.) %>%
  as.numeric
# Convert to Numeric and Remove Weird Characters
wiki$Area <- wiki$Area %>% 
  gsub("^[0-9]+♠","",.) %>%
  gsub("sq.*$","",.) %>%
  gsub("[^0-9\\.]","",.) %>%
  as.numeric
# Column not needed
wiki$Map <- NULL
# Remove " County" from County Names
# One off replacement for "saint louis"
wiki$County <- gsub("( County|\\.)","",wiki$County) %>% 
  tolower %>% gsub("saint louis","st louis",.)
# Just makes it easier to merge later
names(wiki)[1] <- "subregion"

# Append to mn_county
mn_county <- merge(mn_county,wiki,by="subregion",all.x=T)
mn_county$density <- mn_county$Population / mn_county$Area
# Unused
mn_county$bin <- NULL





#--------- UNUSED OR CORRUPTED CODE ~ DO NOT USE -----------#
# Try on DataSet
########## y = c(rep('a', 9990), rep('b', 10))
#set.seed(2)
#index = sample(1:10000, size= 7000) label_train = y[index]
#label_test = y[-index] print(mean(label_train=='b'))
#####################

# scatterplot(SalePrice ~ YrSold, data=hd,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)

# TCreatring Folds - try on DataSet
##########

#library(caret)
#folds = createFolds(credit$default, 5) str(folds)
#str(folds)

##########

# Train & evaluate Try on DataSet
##########
#n=5
#accuracy = numeric(n)
#for(i in 1:n){
#  index = -folds[[i]]
# logit = glm(default~., data = credit[index, ], family = 'binomial')
#  prob = predict(logit, credit[-index, ], type="response") accuracy[i] = mean(
#   (prob>=0.5) == (credit$default[-index]=='yes') )
# } 

# accuracy

# sapply(train, function(x) sum(is.na(x)))

# Train & evaluate:
#print(mean(accuracy)); print(sd(accuracy))
##########


### Cross Validation in Caret
### The package caret actually provides a convenient function to proceed with cross validation:
# ctrl = trainControl(method = "cv", number = 5) logit_cv = train(default ~ ., data=credit,
#              logit_cv$results



#ctrl = trainControl(method = "cv", number = 5)
#tune.grid = expand.grid(lambda = (0:10)*0.1, alpha=0) logit_shrinkage = train(default ~ ., data=credit, method = "glmnet",
##           reProc=c('center', 'scale'), tuneGrid = tune.grid) logit_shrinkage$results


#############################


# Check precision
# precision = cmat[2,2] / (cmat[1,2] + cmat[2,2]) precision





#-------------------------- Old_Clean (Do Not Use) ---------------------------#

# ag_clean <- read.csv('alex_clean_data.csv')
# head(ag_clean, 20)
# 
# 
# 
# ag_clean$Exterior <- paste(ag_clean$Exterior1st, ag_clean$Exterior2nd)
# ag_clean
# ?pwd
# write.csv(ag_clean, 'ag_clean_data.csv')
# 
# read.csv('ag_clean_data.csv')
# 
# select(ag_clean,-c(Exterior2nd))

# hd <- read.csv('train.csv')
# head(hd, 20)






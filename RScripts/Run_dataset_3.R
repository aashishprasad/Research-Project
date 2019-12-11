#install.packages("mltools")
library(data.table)
library(mltools)
library(caret)
library(dplyr)

data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_3.csv")
data <- data[,-c(1)]
#################################################################
data_age_ratings <- one_hot(as.data.table(data$age_rating))
data_month <- one_hot(as.data.table(data$Month))
data_publisher <- one_hot(as.data.table(data$publisher))
data_developer <- one_hot(as.data.table(data$developer))

#################################################################
library(caTools) 

df <- cbind(data,data_month)
df <- cbind(df,data_age_ratings)
df <- cbind(df,data_publisher)
df <- cbind(df,data_developer)
df <- df[,-c(1:3,5)]

#################################################################
#https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
library(Boruta)
set.seed(111)
boruta.game_train <- Boruta(game_rating~., data = df, doTrace = 2)

#take a call on tentative features
boruta.game <- TentativeRoughFix(boruta.game_train)

plot(boruta.game, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.game$ImpHistory),function(i)
  boruta.game$ImpHistory[is.finite(boruta.game$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.game$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.game$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(boruta.game, withTentative = F)
game_df <- attStats(boruta.game)
#################################################################
library(data.table)
setDT(game_df, keep.rownames = TRUE)[]
write.csv(game_df, file = "D:/DA/Semester_3/Research Project/Dataset/boruta_result_dataset_3.csv",fileEncoding = 'UTF-8')
selected_game_df <- df[,c(1,2,6:8,12:15,17,19,27,28,32,33,35:44,56:58,62,64,65,68,74,78,80,92,97,100,103,107,111,113,122,123,127,128,134,150,182,184,185,187:189,193,234)]

#Run if excluding Boruta
#selected_game_df <- df

#################################################################
set.seed(123)
spec = c(train = .8, test = .2)
g = sample(cut(
  seq(nrow(selected_game_df)),
  nrow(selected_game_df)*cumsum(c(0,spec)),
  labels = names(spec)
))
res = split(selected_game_df, g)
train <- res$train
test <- res$test

###################### Random Forest ########################
#install.packages("randomForest")
library(randomForest)

TrainSet <- train
TestSet <- test

#install.packages("janitor")
library(janitor)
TrainSet <- clean_names(TrainSet)#removing spaces and special characters from column names
TestSet <- clean_names(TestSet)

# Fine tuning parameters of Random Forest model
model2 <- randomForest(game_rating ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)

# Predicting on train set
predTrain <- predict(model2, TrainSet)

# Predicting on Test set
predTest <- predict(model2, TestSet)

# RMSE
library(caret)
#Model performance
data.frame(
  RMSE = RMSE(predTest,TestSet$game_rating),
  R2 = R2(predTest,TestSet$game_rating)
)
#RMSE 1.368504, R2 0.205113 Boruta
#RMSE 1.396012, R2 0.1783043
plot(predTest,TestSet$game_rating,col = c("blue") , pch = 19,xlab = 'Predicted', ylab = 'Actual', abline(a=1,b=1))

############################ SVM #####################################
library(caTools) 

training <- train
testing <- test

library(e1071)
model = svm(formula = game_rating ~ ., data = training, type = "eps-regression", kernel = 'radial')
y_pred = predict(model, newdata = testing[-2])

library(caret)
#Model performance
data.frame(
  RMSE = RMSE(y_pred,testing$game_rating),
  R2 = R2(y_pred,testing$game_rating)
)
#RMSE 1.349704, R2 0.2147621 Boruta
#RMSE 1.449982, R2 0.1117363
plot(y_pred,testing$game_rating,col = c("blue") , pch = 19,xlab = 'Predicted', ylab = 'Actual', abline(a=1,b=1))

############################ XGBoost #####################################
#https://datascienceplus.com/extreme-gradient-boosting-with-r
library(xgboost)

training <- train
testing <- test

X_train = xgb.DMatrix(as.matrix(training %>% dplyr::select(-game_rating)))
y_train = training$game_rating
X_test = xgb.DMatrix(as.matrix(testing %>% dplyr::select(-game_rating)))
y_test = testing$game_rating


xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)

set.seed(0) 
xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

#Model performance
data.frame(
  RMSE = RMSE,
  R2 = rsq
)
#RMSE 1.413788, R2 0.1330563 #Boruta
#RMSE 1.34178, R2 0.2191185

plot(predicted,y_test,col = c("blue") , pch = 19,xlab = 'Predicted', ylab = 'Actual', abline(a=1,b=1))

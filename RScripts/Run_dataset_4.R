#install.packages("mltools")
library(data.table)
library(mltools)
library(caret)
library(dplyr)

data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_4.csv")
data <- data[,-c(1)]
#################################################################
data_age_ratings <- one_hot(as.data.table(data$age_rating))
data_month <- one_hot(as.data.table(data$Month))
data_publisher <- one_hot(as.data.table(data$publisher))
data_developer <- one_hot(as.data.table(data$developer))

#################################################################
library(caTools) 

#data_month <- data_month[,-c(3:6,8:10,12,15,18:20,23:25,27:29)]
df <- cbind(data,data_month)
data_age_ratings <- data_age_ratings[,-c(1)]
df <- cbind(df,data_age_ratings)
df <- cbind(df,data_publisher)
#data_developer <- data_developer[,-c(5)]
df <- cbind(df,data_developer)
df <- df[,-c(1:3,5)]

#################################################################
library(Boruta)
set.seed(111)
boruta.game_train <- Boruta(game_rating~., data = df, doTrace = 2)
#print(boruta.game_train)

#take a call on tentative features
boruta.game <- TentativeRoughFix(boruta.game_train)
#print(boruta.game)

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
write.csv(game_df, file = "D:/DA/Semester_3/Research Project/Dataset/boruta_result_dataset_4.csv",fileEncoding = 'UTF-8')
selected_game_df <- df[,c(12,2,13,25,35:44,57,63,97,100,116,120:122,175,177,181,182,185,207,226)]

#Run if excluding Boruta
#selected_game_df <- df

###################### Random Forest ########################
#install.packages("randomForest")
library(randomForest)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(123)
train <- sample(nrow(selected_game_df[,-c(30)]), 0.7*nrow(selected_game_df[,-c(30)]), replace = FALSE)
TrainSet <- selected_game_df[train,]
ValidSet <- selected_game_df[-train,]

#install.packages("janitor")
library(janitor)
TrainSet <- clean_names(TrainSet)#removing spaces and special characters from column names
ValidSet <- clean_names(ValidSet)

# Fine tuning parameters of Random Forest model
model2 <- randomForest(game_rating ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)

# Predicting on train set
predTrain <- predict(model2, TrainSet)

#table(predTrain, TrainSet$game_rating)  

# Predicting on Validation set
predValid <- predict(model2, ValidSet)

# Checking accuracy
#mean(predValid == ValidSet$game_rating)                    
#table(predValid,ValidSet$game_rating)

# RMSE
#install.packages("Metrics")
library(caret)
#Model performance
data.frame(
  RMSE = RMSE(predValid,ValidSet$game_rating),
  R2 = R2(predValid,ValidSet$game_rating)
)

plot(predValid,ValidSet$game_rating,col = c("red","black") , pch = 19)

############################ SVM #####################################
library(caTools) 

set.seed(123) 
split = sample.split(selected_game_df$game_rating, SplitRatio = 0.70)

training = subset(selected_game_df, split == TRUE)
testing = subset(selected_game_df, split == FALSE)

# Scaling 
#training[-2] = scale(training[-2]) 
#testing[-2] = scale(testing[-2])

#training <- na.omit(training)
#training <- training[,-c(53)]
#testing <- testing[,-c(53)]

library(e1071)
model = svm(formula = game_rating ~ ., data = training, type = "eps-regression", kernel = 'radial')
y_pred = predict(model, newdata = testing[-2])

library(caret)
#Model performance
data.frame(
  RMSE = RMSE(y_pred,testing$game_rating),
  R2 = R2(y_pred,testing$game_rating)
)

plot(y_pred,testing$game_rating,col = c("red","black") , pch = 19)

############################ XGBoost #####################################
library(xgboost)
set.seed(123)
# Create index for testing and training data
inTrain <- createDataPartition(y = selected_game_df$game_rating, p = 0.8, list = FALSE)
# subset power_plant data to training
training <- selected_game_df[inTrain,]
# subset the rest to test
testing <- selected_game_df[-inTrain,]

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


#xgb_train$bestTune


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

#plot(predicted,y_test,col = c("red","black") , pch = 19)

options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))



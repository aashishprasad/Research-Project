library(xgboost)
library(caret)

#data <-read.csv('D://Downloads/data.csv',header=T,sep=',')
data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/data_with_nlp_sentiments.csv")
data <- data[,-c(1)]
summary(data)

class(data)


set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = data$game_rating, p = 0.8, list = FALSE)
# subset power_plant data to training
training <- data[inTrain,]
# subset the rest to test
testing <- data[-inTrain,]

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


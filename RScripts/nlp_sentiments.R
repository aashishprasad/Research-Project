library(rvest)
library(dplyr)
library(tidyr)

nlp_data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/wiki_text_nlp.csv")
nlp_data <- nlp_data[,-c(1,2,40:49)]

for(i in 1:nrow(nlp_data)){
  game_text <- as.character(nlp_data[i,38])
  temp_df <- get_nrc_sentiment(game_text, cl = NULL, language = "english")
  
  if(i>1){
    nlp_sentiment_table <- rbind(nlp_sentiment_table, temp_df)
    wiki_tbl <- rbind(wiki_tbl, wiki_text)
  }else{
    nlp_sentiment_table <- temp_df
    wiki_tbl <- wiki_text
  }
}

nlp_data <- cbind(nlp_data$game_rating ,nlp_sentiment_table)
colnames(nlp_data) <- c('game_rating', 'anger','anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive')

#SVM
library(caTools) 

set.seed(123) 
split = sample.split(nlp_data$game_rating, SplitRatio = 0.70)

training = subset(nlp_data, split == TRUE)
testing = subset(nlp_data, split == FALSE)

# Scaling 
#training[-2] = scale(training[-2]) 
#testing[-2] = scale(testing[-2])

#training <- na.omit(training)
#training <- training[,-c(53)]
#testing <- testing[,-c(53)]

library(e1071)
model = svm(formula = game_rating ~ ., data = training, type = "eps-regression", kernel = 'radial')
y_pred = predict(model, newdata = testing[-1])

library(caret)
#Model performance
data.frame(
  RMSE = RMSE(y_pred,testing$game_rating),
  R2 = R2(y_pred,testing$game_rating)
)

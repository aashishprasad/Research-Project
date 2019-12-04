#install.packages('syuzhet')
#install.packages("tidyr")
#install.packages("dplyr")
library(syuzhet)
library(rvest)
library(tidytext)
library(dplyr)
library(tidyr)

wiki_url <- read.csv("D:/DA/Semester_3/Research Project/Dataset/wiki_urls_video_games.csv")
wiki_url <- wiki_url[,-c(1)]

for(i in 1:nrow(wiki_url)){
  print(i)
  #i=33
  url <- wiki_url[i,2]
  if(is.na(url)==FALSE){
    
    #fix for #18 & #33
    if(i == 18){
      url <- paste(url,"_(video_game)",sep = '')
    }else if(i == 33){
      url<- "https://en.wikipedia.org/wiki/AO_Tennis_(video_game)"
    }
    page <- read_html(as.vector(url))
    game_text <- page%>%html_nodes("h2+ p,h3+p")%>%html_text()
    
    #text <- gsub("[\r\n]", "", game_text)
    tryCatch(
      temp_df <- get_nrc_sentiment(game_text, cl = NULL, language = "english"),error = function(e){NA}
    )
    
    #fix for mutiple paragraphs in game_text
    flag = 0
    for (k in 1:nrow(temp_df)) {
      flag=1
      if(k>1){
      text <- paste(text,game_text[k],sep = ' ',collapse = NULL)
      }else{
        text <- game_text[k]
      }
    }
    if(flag==1){
      tryCatch(
        temp_df <- get_nrc_sentiment(text, cl = NULL, language = "english"),error = function(e){NA}
      )
      wiki_text <- text
      #colnames(wiki_text) <- c("text")
    }
    #print(temp_df)
  }else{
    #print('in else') 
    temp_df <- data.frame('0','0','0','0','0','0','0','0','0','0')
    colnames(temp_df) <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
    wiki_text <- NA
    #colnames(wiki_text) <- c("text")
  }
  
  
  if(i>1){
    sentiment_table <- rbind(sentiment_table, temp_df)
    wiki_tbl <- rbind(wiki_tbl, wiki_text)
  }else{
    sentiment_table <- temp_df
    wiki_tbl <- wiki_text
  }
  
}

sentiment_table <- data.frame(sapply(sentiment_table, as.numeric))#converting characters to numeric
game_table_distinct <- read.csv("D:/DA/Semester_3/Research Project/Dataset/ign_video_games.csv")

#storing game texts
# tbl <- cbind(wiki_url,wiki_tbl)
# tbl <- tbl[,-c(2)]
# rownames(tbl) <- 1:nrow(tbl)
# tbl <- cbind(tbl,game_table_distinct$game_rating)
# #tbl$`game_table_distinct$game_rating` <- floor(tbl$`game_table_distinct$game_rating`)
# colnames(tbl) <- c("game_name","game_text","game_ratings")
# write.csv(tbl, file = "D:/DA/Semester_3/Research Project/Dataset/game_data.csv",fileEncoding = 'UTF-8')

#merging and saving game data with plots
# tbl <- cbind(game_table_distinct,wiki_tbl)
# rownames(tbl) <- 1:nrow(tbl)
# tbl <- tbl[,-c(1)]
# write.csv(tbl, file = "D:/DA/Semester_3/Research Project/Dataset/game_data.csv",fileEncoding = 'UTF-8')

#merging sentiments with descriptive game data
final_game_data <- cbind(game_table_distinct,sentiment_table)
final_game_data <- cbind(final_game_data,wiki_url$url)
final_game_data <- cbind(final_game_data,wiki_tbl)
final_game_data <- final_game_data[,-c(1)]
rownames(final_game_data) <- 1:nrow(final_game_data)#resetting row numbering
##########################

#checking valid rows
valid=0
for(i in 1:nrow(sentiment_table)){
  #print(i)
  flag=0
  for(j in 1:10){
    if(sentiment_table[i,j]!='0'){
      flag=1
    }
  }
  if(flag==1){
    valid= valid+1
    temp_df <- data.frame('1')
    colnames(temp_df) <- c("valid")
  }else{
    temp_df <- NA
  }
  if(i>1){
    dummy_df <- rbind(dummy_df,temp_df)
  }else{
    dummy_df <- temp_df
  }
}

#removing rows with 'valid' as NA
final_game_data <- cbind(final_game_data,dummy_df)
final_game_data <- final_game_data[complete.cases(final_game_data[ , 52]),]
final_game_data <- final_game_data[,-c(52)]

#rownames(final_game_data) <- 1:nrow(final_game_data)#resetting row numbering
#view_table <- cbind(wiki_url,sentiment_table)

#converting factors to characters
final_game_data$developer <- as.character(final_game_data$developer)
final_game_data$publisher <- as.character(final_game_data$publisher)

#converting factors to numeric
final_game_data$game_rating <- as.numeric(as.character(final_game_data$game_rating))
#final_game_data$developer <- data.frame(sapply(final_game_data$developer, as.character), stringsAsFactors=FALSE)#converting factors to characters
#final_game_data$publisher <- data.frame(sapply(final_game_data$publisher, as.character), stringsAsFactors=FALSE)#converting factors to characters

######################################
countifs<-function(x,v){
  ifelse(is.na(v),0,
  sum(ifelse(x==v,1,0)))}

test_df <- final_game_data

#feature selection for 'developer' and 'publisher'
for(d in 1:nrow(test_df)){
  
  if(countifs(test_df$publisher,test_df[d,3])<15){
    test_df[d,3] <- 'other'
    
  }
  
  if(countifs(test_df$developer,test_df[d,2])<5){
    test_df[d,2] <- 'other'
  }
}
######################################
#test_df <- data.frame(data_test)
####-------------####
#imputing missing values
for(i in 1:nrow(test_df)){
  print(i)
  #flag=0
  #i=22
  url <- test_df[i,50]
  #fix for #18 & #33
  if(i == 14){
    url <- paste(url,"_(video_game)",sep = '')
  }else if(i == 22){
    url<- "https://en.wikipedia.org/wiki/AO_Tennis_(video_game)"
  }
  page <- read_html(as.vector(url))
  
  for(j in 2:6){
    if(is.na(test_df[i,j])||test_df[i,j]=='N/A'||test_df[i,j]=='TBA'||test_df[i,j]=='released'||test_df[i,j]=='N'){
    #print('found')
      if(j==2){
        #print(i)
        game_text <- page%>%html_nodes("tr:nth-child(3) td")%>%html_text()
        test_df[i,j] <- game_text[1]
      }else if(j==3){
        #print(j)
        game_text <- page%>%html_nodes("tr:nth-child(4) td")%>%html_text()
        test_df[i,j] <- game_text[1]
      }else if(j==4){
        #print(j)
        #game_text <- page%>%html_nodes("li:nth-child(1)")%>%html_text()
      }else if(j==5){
        #print(j)
        #game_text <- page%>%html_nodes("li:nth-child(1)")%>%html_text()
      }else if(j==6){
        #print(j)
        #game_text <- page%>%html_nodes("h2+ p,h3+p")%>%html_text()
      }
    }
  }
}


#######################################################
#write.csv(test_df, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv",fileEncoding = 'UTF-8')
#xxxxxxxx-----xxxxxxxxxx#
# test_df$age_rating <- factor(test_df$age_rating)
# test_df$game_rating <- as.numeric(test_df$game_rating)
# test_df$Dreamcast <- factor(test_df$Dreamcast)
# test_df$Wireless <- factor(test_df$Wireless)
# test_df$Saturn <- factor(test_df$Saturn)
# test_df$Android <- factor(test_df$Android)
# test_df$iPad <- factor(test_df$iPad)
# test_df$iPhone <- factor(test_df$iPhone)
# test_df$Arcade <- factor(test_df$Arcade)
# test_df$GameCube <- factor(test_df$GameCube)
# test_df$Genesis <- factor(test_df$Genesis)
# test_df$Macintosh <- factor(test_df$Macintosh)
# test_df$PC <- factor(test_df$PC)
# test_df$Nintendo <- factor(test_df$Nintendo)
# test_df$GameBoy <-as.character(as.numeric(test_df$Gameboy))
# test_df$NES <- factor(test_df$NES)
# test_df$PlayStation <- factor(test_df$PlayStation)
# test_df$Xbox <- factor(test_df$Xbox)
# test_df$Wii <- factor(test_df$Wii)
# test_df$otherPlatform <- factor(test_df$otherPlatform)
# test_df$Flight <- factor(test_df$Flight)
# test_df$Music <- factor(test_df$Music)
# test_df$Simulation <- factor(test_df$Simulation)
# test_df$Fighting <- factor(test_df$Fighting)
# test_df$Platformer <- factor(test_df$Platformer)
##################################################
#data <- test_df
#test_df <- read.csv("D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv")
#test_df <- test_df[,-c(50)]
#data$game_rating <- floor(data$game_rating)

########### Re-run countifs
#feature selection for 'developer' and 'publisher'
for(d in 1:nrow(test_df)){
  
    if(countifs(na.omit(test_df$publisher),test_df[d,3])<15){
      test_df[d,3] <- 'other'
    }
  
    if(countifs(na.omit(test_df$developer),test_df[d,2])<10){
      test_df[d,2] <- 'other'
    }
}
############
write.csv(test_df[,-c(50)], file = "D:/DA/Semester_3/Research Project/Dataset/game_data.csv",fileEncoding = 'UTF-8')
############
write.csv(test_df[,-c(50,51)], file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv",fileEncoding = 'UTF-8')
############
data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv")
data <- data[,-c(1,2)]

#replacing sentiments with NLP sentiments
data <- data[,-c(39:48)]
data <- cbind(data,nlp_sentiment_table)
################################################
data$developer <- factor((data$developer))
data$publisher <- factor((data$publisher))
data$Month <- factor((data$Month))
data$Year <- as.integer(as.character(data$Year))
#data$age_rating <- factor((data$age_rating))
#################################################

# data$developer <- as.integer(as.factor(data$developer))
# data$publisher <- as.integer(as.factor(data$publisher))
# data$Month <- as.integer(as.factor(data$Month))
# data$Year <- as.integer(as.factor(data$Year))
# data$age_rating <- as.integer(as.factor(data$age_rating))
# 
data$Dreamcast <- as.numeric(as.character(data$Dreamcast))
data$Wireless <- as.numeric(as.character(data$Wireless))
data$Saturn <- as.numeric(as.character(data$Saturn))
data$Android <- as.numeric(as.character(data$Android))
data$iPad <- as.numeric(as.character(data$iPad))
data$iPhone <- as.numeric(as.character(data$iPhone))
data$Arcade <- as.numeric(as.character(data$Arcade))
data$GameCube <- as.numeric(as.character(data$GameCube))
data$Genesis <- as.numeric(as.character(data$Genesis))
data$Macintosh <- as.numeric(as.character(data$Macintosh))
data$PC <- as.numeric(as.character(data$PC))
data$Nintendo <- as.numeric(as.character(data$Nintendo))
data$GameBoy <- as.numeric(as.character(data$GameBoy))
data$NES <- as.numeric(as.character(data$NES))
data$PlayStation <- as.numeric(as.character(data$PlayStation))
data$Xbox <- as.numeric(as.character(data$Xbox))
data$Wii <- as.numeric(as.character(data$Wii))
data$otherPlatform <- as.numeric(as.character(data$otherPlatform))
data$Flight <- as.numeric(as.character(data$Flight))
data$Music <- as.numeric(as.character(data$Music))
data$Simulation <- as.numeric(as.character(data$Simulation))
data$Fighting <- as.numeric(as.character(data$Fighting))
data$Platformer <- as.numeric(as.character(data$Platformer))
data$Puzzle <- as.numeric(as.character(data$Puzzle))
data$Strategy <- as.numeric(as.character(data$Strategy))
data$Adventure <- as.numeric(as.character(data$Adventure))
data$RPG <- as.numeric(as.character(data$RPG))
data$Racing <- as.numeric(as.character(data$Racing))
data$Shooter <- as.numeric(as.character(data$Shooter))
data$Sports <- as.numeric(as.character(data$Sports))
data$Action <- as.numeric(as.character(data$Action))
data$otherGenre <- as.numeric(as.character(data$otherGenre))
#data$game_rating <- as.integer(as.factor(data$game_rating))

# data$Dreamcast <- factor(data$Dreamcast)
# data$Wireless <- factor(data$Wireless)
# data$Saturn <- factor(data$Saturn)
# data$Android <- factor(data$Android)
# data$iPad <- factor(data$iPad)
# data$iPhone <- factor(data$iPhone)
# data$Arcade <- factor(data$Arcade)
# data$GameCube <- factor(data$GameCube)
# data$Genesis <- factor(data$Genesis)
# data$Macintosh <- factor(data$Macintosh)
# data$PC <- factor(data$PC)
# data$Nintendo <- factor(data$Nintendo)
# data$GameBoy <- factor(data$GameBoy)
# data$NES <- factor(data$NES)
# data$PlayStation <- factor(data$PlayStation)
# data$Xbox <- factor(data$Xbox)
# data$Wii <- factor(data$Wii)
# data$otherPlatform <- factor(data$otherPlatform)
# data$Flight <- factor(data$Flight)
# data$Music <- factor(data$Music)
# data$Simulation <- factor(data$Simulation)
# data$Fighting <- factor(data$Fighting)
# data$Platformer <- factor(data$Platformer)
# data$Puzzle <- factor(data$Puzzle)
# data$Strategy <- factor(data$Strategy)
# data$Adventure <- factor(data$Adventure)
# data$RPG <- factor(data$RPG)
# data$Racing <- factor(data$Racing)
# data$Shooter <- factor(data$Shooter)
# data$Sports <- factor(data$Sports)
# data$Action <- factor(data$Action)
# data$otherGenre <- factor(data$otherGenre)
# data$game_rating <- factor(data$game_rating)

#output <- lm(data$game_rating ~ data$positive+data$negative+data$anger+data$anticipation+data$disgust+data$fear+
#               data$joy+data$sadness+data$surprise)
#output <- lm(data$game_rating ~ ., data = data)
data <- na.omit(data)

#eliminating rows with irrrelavent month values
data <- data %>% filter(data$Month == 'January' | data$Month=='Febuary' | data$Month == 'March' | data$Month == 'April' | data$Month == 'May' | data$Month == 'June' | data$Month == 'July' | data$Month == 'August' | data$Month == 'September' | data$Month == 'October' | data$Month == 'November' | data$Month == 'December')

write.csv(data, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_1.csv",fileEncoding = 'UTF-8')
##################################################
#selecting rows with Year > = '2000'
data <- data %>% filter(data$Year >= '2000')
write.csv(data, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_2.csv",fileEncoding = 'UTF-8')
##################################################
#remove 'other' from publisher
data <- data %>% filter(data$publisher != 'other')
write.csv(data, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_3.csv",fileEncoding = 'UTF-8')
##################################################
#remove 'other' from developer
data <- data %>% filter(data$developer != 'other')
write.csv(data, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_dataset_4.csv",fileEncoding = 'UTF-8')
##################################################
#install.packages("mltools")
library(data.table)
library(mltools)
data_age_ratings <- one_hot(as.data.table(data$age_rating))
data_month <- one_hot(as.data.table(data$Month))
data_publisher <- one_hot(as.data.table(data$publisher))
data_developer <- one_hot(as.data.table(data$developer))

#data_month[, colSums(data_month) != 0]
#data_month[,which(colSums(data_month) != 0)]

#data_month <- apply(data_month == 0, 2, all)

# i <- (colSums(data_month, na.rm=T) != 0) # T if colSum is not 0, F otherwise
# t <- 0
# i <- as.data.frame(i)
# for(k in 1:nrow(i)){
#   if(i[k,1]== FALSE){
#     k <- k-t
#     data_month <- data_month[,-c(k)]
#     t <- t+1
#   }
# }
#################################################
library(caTools) 
#data <- data[,-c(7:38)]
#data <- data[,-c(1:5)]

# data_month <- as.numeric(as.character(data_month))
# data_month <- as.data.frame(data_month)
# 
# newdata1 <- data_month[,c(1:20)!=0]
# newdata1 <- as.data.frame(newdata1)
data_month <- data_month[,-c(3:6,8:10,12,15,18:20,23:25,27:29)]
df <- cbind(data,data_month)
data_age_ratings <- data_age_ratings[,-c(1)]
df <- cbind(df,data_age_ratings)
df <- cbind(df,data_publisher)
#data_developer <- data_developer[,-c(5)]
df <- cbind(df,data_developer)
df <- df[,-c(1:3,5)]

#################################################
set.seed(123)
split = sample.split(df$game_rating, SplitRatio = 0.70)

training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# training_set = subset(data[,-c(7:38)], split == TRUE)
# test_set = subset(data[,-c(7:38)], split == FALSE)

# Scaling 
#training_set[-6] = scale(training_set[-6]) 
#test_set[-6] = scale(test_set[-6])

output <- lm(training_set$game_rating ~ ., data = training_set)
pred <- predict(output, newdata = test_set[-2])
#######################
# RMSE
#install.packages("Metrics")
library(caret)
#Model performance
data.frame(
  RMSE = RMSE(pred, test_set$game_rating),
  R2 = R2(pred, test_set$game_rating)
)

#######################
#Correlation
data.cor = cor(data[,-c(7:38)], method = c("spearman"),use="pairwise.complete.obs")
#install.packages("corrplot")
library(corrplot)
corrplot(data.cor)
#######################
T_log = log(data$game_rating)

#install.packages("rcompanion")
library(rcompanion)

plotNormalHistogram(T_log)
plotNormalHistogram(data$game_rating)
res <- cor(data.cor)
round(res, 2)
###################################
# 
# data <- read.csv("D:/DA/Semester_3/Research Project/Dataset/Video_Game_Sales_as_of_Jan_2017.csv")
# 
# data$Platform <- as.integer(as.factor(data$Platform))
# data$Year_of_Release <- as.integer(as.factor(data$Year_of_Release))
# data$Genre <- as.integer(as.factor(data$Genre))
# data$Publisher <- as.integer(as.factor(data$Publisher))
# data$Rating <- as.integer(as.factor(data$Rating))
# 
# data$Rating = log(data$Rating)
# 
# data.cor = cor(data[,-c(1)], method = c("spearman"),use="pairwise.complete.obs")
# #install.packages("corrplot")
# library(corrplot)
# corrplot(data.cor)
#######################################
pairs(game_rating ~ developer+publisher+age_rating+Month+Year+anger+anticipation+joy+disgust+sadness+fear+surprise+positive+negative
      ,data=data,panel=panel.smooth, 
      main="Simple Scatterplot Matrix")
######################################
#intrain <- createDataPartition(y = data$game_rating, p= 0.7, list = FALSE)
#training <- data[intrain,]
#testing <- data[-intrain,]

#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# svm_Linear <- train(game_rating ~., data = training, method = "svmLinear",
#                     trControl=trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 10)
#############################################################################
#df[45:241] <- lapply(df[45:241] , factor)
library(Boruta)
set.seed(111)
boruta.game_train <- Boruta(game_rating~., data = df, doTrace = 2)
print(boruta.game_train)

#take a call on tentative features
boruta.game <- TentativeRoughFix(boruta.game_train)
print(boruta.game)

plot(boruta.game, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.game$ImpHistory),function(i)
  boruta.game$ImpHistory[is.finite(boruta.game$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.game$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.game$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(boruta.game, withTentative = F)
game_df <- attStats(boruta.game)
#print(game_df)
######################################
library(data.table)
setDT(game_df, keep.rownames = TRUE)[]
write.csv(game_df, file = "D:/DA/Semester_3/Research Project/Dataset/boruta_result.csv",fileEncoding = 'UTF-8')
#selected_game_df <- game_df %>% filter(game_df$decision == 'Confirmed')
selected_game_df <- df[,-c(3,4,9,10,14,17,18,20:24,26,31,34,45:55,57,59:61,63,64,67:75,78:81,83:102,104,106,108,109,111,112,115,117:129,132:187,189,191,192,195:198,200:211,213:216,218:237,239:242)]
#selected_game_df$Year <- as.integer(selected_game_df$Year)
#############################################################################
#SVM
library(caTools) 

set.seed(123) 
split = sample.split(selected_game_df$game_rating, SplitRatio = 0.70)

training = subset(selected_game_df, split == TRUE)
testing = subset(selected_game_df, split == FALSE)

# Scaling 
training[-2] = scale(training[-2]) 
testing[-2] = scale(testing[-2])

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

# Confusion Matrix 
#cm = table(testing[, 2], y_pred)
#sum(diag(cm))/sum(cm)

#library(caret)
#caret::confusionMatrix(y_pred,testing$game_rating, positive = '1')

# test_pred <- predict(svm_Radial, newdata = testing)
# test_pred
# confusionMatrix(table(test_pred, testing$game_rating))
#############################################
# intrain <- createDataPartition(y = selected_game_df$game_rating, p= 0.7, list = FALSE)
# training <- data[intrain,]
# testing <- data[-intrain,]
# 
# trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
# svm_Linear <- train(game_rating ~., data = training, method = "svmLinear",
#                      trControl=trctrl,
#                      preProcess = c("center", "scale"),
#                      tuneLength = 10)
library(e1071)
dat = data.frame(selected_game_df[,-c(2)], y = as.factor(selected_game_df$game_rating))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
#############################################
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

# Create a Random Forest model with default parameters
#model1 <- randomForest(game_rating ~ ., data = TrainSet, importance = TRUE)

# Fine tuning parameters of Random Forest model
model2 <- randomForest(game_rating ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)

# Predicting on train set
#predTrain <- predict(model2, TrainSet, type = "class")
predTrain <- predict(model2, TrainSet)

#root mean square error with anti log
# rmse<-function(actual,pred)
# {
#   sqrt( sum( (actual - pred)^2 , na.rm = TRUE ) / length(actual) )
# }
# R2(ValidSet$game_rating,predTrain)






# Checking classification accuracy
#mean(predTrain == TrainSet$game_rating)
table(predTrain, TrainSet$game_rating)  

# Predicting on Validation set
#predValid <- predict(model2, ValidSet, type = "class")
predValid <- predict(model2, ValidSet)

# Checking accuracy
mean(predValid == ValidSet$game_rating)                    
table(predValid,ValidSet$game_rating)

# RMSE
#install.packages("Metrics")
library(caret)
#Model performance
data.frame(
  RMSE = RMSE(predValid,ValidSet$game_rating),
  R2 = R2(predValid,ValidSet$game_rating)
)

plot(predValid,ValidSet$game_rating,col = c("red","black") , pch = 19)
########################################################################
# library(caret)
# control_xgb <- trainControl(method="repeatedcv", number=10, repeats=5, search="random")
# set.seed(1337)
# 
# xgbm_random <- caret::train(game_rating ~ .,data=TrainSet,
#                             method="xgbTree",
#                             trControl=control_xgb)
##########################################

#install.packages('syuzhet')
library(syuzhet)
library(rvest)
library(tidytext)

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
    }
    #print(temp_df)
  }else{
    #print('in else') 
    temp_df <- data.frame('0','0','0','0','0','0','0','0','0','0')
    colnames(temp_df) <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
  }
  
  
  if(i>1){
    sentiment_table <- rbind(sentiment_table, temp_df)
  }else{
    sentiment_table <- temp_df
  }
  
}

sentiment_table <- data.frame(sapply(sentiment_table, as.numeric))#converting characters to numeric

#merging sentiments with descriptive game data
final_game_data <- cbind(game_table_distinct,sentiment_table)
final_game_data <- cbind(final_game_data,wiki_url$url)

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
final_game_data <- final_game_data[complete.cases(final_game_data[ , 51]),]
final_game_data <- final_game_data[,-c(51)]

rownames(final_game_data) <- 1:nrow(final_game_data)#resetting row numbering
#view_table <- cbind(wiki_url,sentiment_table)

#converting factors to characters
final_game_data$developer <- as.character(final_game_data$developer)
final_game_data$publisher <- as.character(final_game_data$publisher)

#converting factors to numeric
final_game_data$game_rating <- as.numeric(as.character(final_game_data$game_rating))
#final_game_data$developer <- data.frame(sapply(final_game_data$developer, as.character), stringsAsFactors=FALSE)#converting factors to characters
#final_game_data$publisher <- data.frame(sapply(final_game_data$publisher, as.character), stringsAsFactors=FALSE)#converting factors to characters

######################################
#feature selection for 'developer' and 'publisher'
for(d in 1:nrow(final_game_data)){
  
  if(countifs(final_game_data$publisher,final_game_data[d,3])<15){
    final_game_data[d,3] <- 'other'
    
  }
  
  if(countifs(final_game_data$developer,final_game_data[d,2])<5){
    final_game_data[d,2] <- 'other'
  }
}
######################################
test_df <- data.frame(final_game_data)
####-------------####
#imputing missing values
for(i in 1:nrow(final_game_data)){
  print(i)
  #flag=0
  #i=22
  url <- final_game_data[i,50]
  #fix for #18 & #33
  if(i == 14){
    url <- paste(url,"_(video_game)",sep = '')
  }else if(i == 22){
    url<- "https://en.wikipedia.org/wiki/AO_Tennis_(video_game)"
  }
  page <- read_html(as.vector(url))
  
  for(j in 2:6){
    if(is.na(final_game_data[i,j])||final_game_data[i,j]=='N/A'||final_game_data[i,j]=='TBA'||final_game_data[i,j]=='released'||final_game_data[i,j]=='N'){
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
write.csv(test_df, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv",fileEncoding = 'UTF-8')
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
data <- test_df
data <- data[,c(-1,-50)]

data$developer <- factor((data$developer))
data$publisher <- factor((data$publisher))
data$Month <- factor((data$Month))
data$Year <- factor((data$Year))
data$age_rating <- factor((data$age_rating))
#################################################

data$developer <- as.integer(as.factor(data$developer))
data$publisher <- as.integer(as.factor(data$publisher))
data$Month <- as.integer(as.factor(data$Month))
data$Year <- as.integer(as.factor(data$Year))
data$age_rating <- as.integer(as.factor(data$age_rating))

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


#output <- lm(data$game_rating ~ data$positive+data$negative+data$anger+data$anticipation+data$disgust+data$fear+
#               data$joy+data$sadness+data$surprise)
#output <- lm(data$game_rating ~ ., data = data)
data <- na.omit(data)
data$game_rating = log(data$game_rating)
#################################################
library(caTools) 

set.seed(123) 
split = sample.split(data$game_rating, SplitRatio = 0.70)

training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

# Scaling 
#training_set[-6] = scale(training_set[-6]) 
#test_set[-6] = scale(test_set[-6])

output <- lm(training_set$game_rating ~ ., data = training_set)
pred <- predict(output, newdata = test_set[-6])
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
summary(output)#######################
T_log = log(data$game_rating)

#install.packages("rcompanion")
library(rcompanion)

plotNormalHistogram(T_log)
plotNormalHistogram(data$game_rating)

###################################

countifs<-function(x,v){
  sum(ifelse(x==v,1,0))}
countifs(game_table_distinct$publisher,'Nintendo')
for (publisher in unique(game_table_distinct$publisher)){
  print(countifs(game_table_distinct$publisher,publisher))}
  
  for (publisher in unique(game_table_distinct$publisher)){}
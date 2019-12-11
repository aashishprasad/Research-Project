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
      
    }
    
  }else{
    temp_df <- data.frame('0','0','0','0','0','0','0','0','0','0')
    colnames(temp_df) <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
    wiki_text <- NA
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

#converting factors to characters
final_game_data$developer <- as.character(final_game_data$developer)
final_game_data$publisher <- as.character(final_game_data$publisher)

#converting factors to numeric
final_game_data$game_rating <- as.numeric(as.character(final_game_data$game_rating))
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

#imputing missing values
for(i in 1:nrow(test_df)){
  print(i)
  
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
#################################################

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

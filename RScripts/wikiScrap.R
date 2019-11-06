#install.packages('syuzhet')
library(syuzhet)
library(rvest)
library(tidytext)

wiki_url <- read.csv("D:/DA/Semester_3/Research Project/Dataset/wiki_urls_video_games.csv")
wiki_url <- wiki_url[,-c(1)]

for(i in 1:nrow(wiki_url)){
  print(i)
  #i=6
  url <- wiki_url[i,2]
  if(is.na(url)==FALSE){
    
    #fix for #25
    if(i == 18){
      url <- paste(url,"_(video_game)",sep = '')
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

#merging sentiments with descriptive game data
final_game_data <- cbind(game_table_distinct,sentiment_table)

##########################
#imputing missing values


##########################

#checking valid rows
valid=0
for(i in 1:nrow(sentiment_table)){
  print(i)
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

#removing rows with valid as NA
final_game_data <- cbind(final_game_data,dummy_df)
final_game_data <- final_game_data[complete.cases(final_game_data[ , 50]),]
final_game_data <- final_game_data[,-c(50)]

#view_table <- cbind(wiki_url,sentiment_table)

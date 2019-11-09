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

#merging sentiments with descriptive game data
final_game_data <- cbind(game_table_distinct,sentiment_table)
final_game_data <- cbind(final_game_data,wiki_url$url)

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

#removing rows with 'valid' as NA
final_game_data <- cbind(final_game_data,dummy_df)
final_game_data <- final_game_data[complete.cases(final_game_data[ , 51]),]
final_game_data <- final_game_data[,-c(51)]

rownames(final_game_data) <- 1:nrow(final_game_data)#resetting row numbering
#view_table <- cbind(wiki_url,sentiment_table)

final_game_data <- data.frame(lapply(final_game_data, as.character), stringsAsFactors=FALSE)#converting factors to characters
test_df <- data.frame(final_game_data)
##########################
#imputing missing values
for(i in 1:nrow(final_game_data)){
  print(i)
  #flag=0
  i=22
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
        print(i)
        game_text <- page%>%html_nodes("tr:nth-child(3) td")%>%html_text()
        test_df[i,j] <- game_text[1]
      }else if(j==3){
        print(j)
        game_text <- page%>%html_nodes("tr:nth-child(4) td")%>%html_text()
        test_df[i,j] <- game_text[1]
      }else if(j==4){
        print(j)
        #game_text <- page%>%html_nodes("li:nth-child(1)")%>%html_text()
      }else if(j==5){
        print(j)
        #game_text <- page%>%html_nodes("li:nth-child(1)")%>%html_text()
      }else if(j==6){
        print(j)
        #game_text <- page%>%html_nodes("h2+ p,h3+p")%>%html_text()
      }
    }
  }
}

#######################################################
write.csv(test_df, file = "D:/DA/Semester_3/Research Project/Dataset/final_video_game_data.csv",fileEncoding = 'UTF-8')
#xxxxxxxx-----xxxxxxxxxx#


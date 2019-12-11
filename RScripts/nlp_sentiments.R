library(rvest)
library(dplyr)
library(tidyr)
library(syuzhet)

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


library(rvest)

wiki_url <- read.csv("D:/DA/Semester_3/Research Project/Dataset/wiki_urls_video_games.csv")
wiki_url <- wiki_url[,-c(1)]

for(i in wiki_url){
  url <- wiki_url[i,2]
  if(is.na(url)==FALSE){
    print(url)
    page <- read_html(as.vector(url))
    game_text <- page%>%html_nodes("h2+ p")%>%html_text()
  }else{
    print('in else') 
  }
  
}
#Library for web scraping
#install.packages("rvest")
#install.packages("dplyr")
library(dplyr)
library(rvest)
#install.packages("purrr")

flag = 0 #default value

for(l in 1:26){
  val=LETTERS[l]
  print(val)

# initiallizing 
start = 0
n = 50

Sys.sleep(5)

for(k in 1:100){
if(start == 0){
  website <- read_html(paste("https://ie.ign.com/games?letter=",val,sep = ''))
  start = 1
}else{
  
  weburl <- paste("https://ie.ign.com/games?startIndex=",n,"&letter=",val,sep = '')
  website <- read_html(weburl)
  n = n+ 50
  print(weburl)
  #print(n)
}
  
#Finding End of Page.
  end_text <- website%>%html_nodes(".itemList")%>%html_text()
  if(identical(end_text, "\n    \nNo Results.        ")){
    print('##############EndOfPage#############')
    break()
  }
  
#scraping webpage data based on html paths taken from selector gadget

base_url <- "https://ie.ign.com"
titles <- website%>%html_nodes(".item-title a")%>%html_text()
titles_url <- paste(base_url, website%>%html_nodes(".item-title a")%>%html_attr('href'),sep = '')
titles_table <- data.frame(titles,titles_url)


for (i in 1:50) {
 print(i) 


  tryCatch(
game_page <- read_html(as.vector(titles_table$titles_url[i])),error = function(e){NA}
  )
  
  
game_name <- game_page%>%html_nodes("h1 span")%>%html_text()
developer <- substring(game_page%>%html_nodes(".developer")%>%html_text(),10)
publisher <- substring(game_page%>%html_nodes(".publisher")%>%html_text(),10)
release_date <- substring(game_page%>%html_nodes(".release-date")%>%html_text(),13)
platform <- substring(game_page%>%html_nodes(".platforms")%>%html_text(),10)
age_rating <- first(substring(game_page%>%html_nodes(".fake-link")%>%html_text(),6))
genre <- game_page%>%html_nodes(".about-object .jsx-1709341442 .jsx-1709341442:nth-child(2)")%>%html_text()
game_rating <- first(game_page%>%html_nodes(".hexagon-content")%>%html_text())

if(identical(age_rating, character(0))){
  age_rating <- NA
}else if(identical(developer, character(0))){
  developer <- NA
}else if(identical(publisher, character(0))){
  publisher <- NA
}else if(identical(release_date, character(0))){
  release_date <- NA
}else if(identical(platform, character(0))){
  platform <- NA
}else if(identical(genre, character(0))){
  genre <- NA
}

  if(game_rating!= "nr"){
    if(i == 1 && flag == 0){
      #print(TRUE)
      game_table <- data.frame(game_name,developer,publisher,genre,platform,release_date,age_rating,game_rating)
      flag <- 1
    }else{
      #print(FALSE)
        if(flag == 0){
          game_table <- data.frame(game_name,developer,publisher,genre,platform,release_date,age_rating,game_rating)
          flag <- 1
        }else{
          #print(game_name)
          dummy_table <- data.frame(game_name,developer,publisher,genre,platform,release_date,age_rating,game_rating)
          game_table <- rbind(game_table,dummy_table) 
          #remove(dummy_table)
        }
    }
  }
}

}
}


game_table_distinct  <- distinct(game_table)

#write.csv(game_table_distinct, file = "D:/DA/Semester_3/Research Project/Dataset/ign_video_games.csv")


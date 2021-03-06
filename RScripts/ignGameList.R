#Library for web scraping
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("purrr")
library(dplyr)
library(rvest)
library(tidyr)

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

#for loop :: feature selection for platforms
for(i in 1:nrow(game_table_distinct)){
  print(i)
  
  platform_df <- data.frame("")
  
  if(grepl('Wii',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Wii')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Wii")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Wii")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Xbox',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Xbox')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Xbox")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Xbox")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('PlayStation',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('PlayStation')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("PlayStation")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("PlayStation")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('NES',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('NES')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("NES")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("NES")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Game Boy',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Game Boy')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("GameBoy")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("GameBoy")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Nintendo',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Nintendo')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Nintendo")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Nintendo")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('PC',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('PC')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("PC")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("PC")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Macintosh',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Macintosh')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Macintosh")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Macintosh")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Amiga',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Amiga')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Amiga")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Amiga")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Genesis',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Genesis')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Genesis")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Genesis")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('GameCube',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('GameCube')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("GameCube")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("GameCube")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Lynx',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Lynx')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Lynx")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Lynx")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Windows Phone',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Windows Phone')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("WindowsPhone")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("WindowsPhone")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Arcade',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Arcade')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Arcade")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Arcade")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('iPhone',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('iPhone')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("iPhone")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("iPhone")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('iPad',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('iPad')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("iPad")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("iPad")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Android',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Android')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Android")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Android")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('digiBLAST',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('digiBLAST')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("digiBLAST")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("digiBLAST")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Saturn',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Saturn')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Saturn")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Saturn")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('M2',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('M2')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("M2")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("M2")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('TurboGrafx-16',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('TurboGrafx-16')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("TurboGrafx_16")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("TurboGrafx_16")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Wireless',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Wireless')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Wireless")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Wireless")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Dreamcast',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Dreamcast')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Dreamcast")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Dreamcast")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Gizmondo',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Gizmondo')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Gizmondo")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Gizmondo")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Web Games',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Web Games')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("WebGames")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("WebGames")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Master System',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Master System')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("MasterSystem")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("MasterSystem")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('3DO',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('3DO')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("3DO")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("3DO")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Atari',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Atari')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Atari")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Atari")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  if(grepl('Sega',game_table_distinct[i,5],ignore.case = TRUE)==TRUE){
    #print('Sega')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Sega")
    
    platform_df <- cbind(temp_df,platform_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Sega")
    
    platform_df <- cbind(temp_df,platform_df)
  }
  platform_df <- platform_df[,-30]
  if(i>1){
  platform_table <- rbind(platform_table,platform_df)
  }else{
    platform_table <- platform_df
  }
}

#test_df <- game_table_distinct
game_table_distinct <- cbind(game_table_distinct,platform_table)

#separating month and date
game_table_distinct <- game_table_distinct %>% separate(release_date, c("Month", "Date","Year"),extra='drop')
game_table_distinct <- game_table_distinct[,-5]
game_table_distinct <- game_table_distinct[,-6]

#creating 'otherPlatform' column for games where platform count < 100
#######################################################
valid = 0

for(i in 1:nrow(game_table_distinct)){
  print(i)
  
  for(j in 9:29){
    if(j<15 || j>16 && j<19 || j == 20 || j>24 && j <27 || j == 29){#specific platform columns
      
      if(game_table_distinct[i,j] == 1){
         valid = 1
        }
    }
  }
  if(valid == 1){
    
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("otherPlatform")
    
    if(i>1){
      otherPlatform_df <- rbind(otherPlatform_df,temp_df)
    }else{
      otherPlatform_df <- temp_df
    }
    
    valid = 0 #resetting valid as '0'
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("otherPlatform")
    
    if(i>1){
      otherPlatform_df <- rbind(otherPlatform_df,temp_df)
    }else{
      otherPlatform_df <- temp_df
    }
  }
}

game_table_distinct <- cbind(game_table_distinct,otherPlatform_df)
#######################################################

#removing irrelevant platform columns
game_table_distinct <- game_table_distinct[,-c(9:14,17,18,20,25,26,29)]

#changing age rating "K-A" as "E"
levels(game_table_distinct$age_rating)[2] <- "E"

#removeing quotes
game_table_distinct$age_rating <- gsub("\"","", game_table_distinct$age_rating)


#for loop :: feature selection for genre
for(i in 1:nrow(game_table_distinct)){
  print(i)
  
  genre_df <- data.frame("")
  
  if(grepl('Action',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    #print('Wii')
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Action")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Action")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Sports',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Sports")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Sports")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Shooter',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Shooter")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Shooter")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Racing',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Racing")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Racing")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('RPG',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("RPG")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("RPG")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Adventure',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Adventure")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Adventure")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Strategy',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Strategy")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Strategy")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Puzzle',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Puzzle")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Puzzle")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Platformer',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Platformer")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Platformer")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Fighting',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Fighting")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Fighting")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Simulation',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Simulation")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Simulation")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Music',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Music")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Music")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Flight',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Flight")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Flight")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Party',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Party")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Party")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Card',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Card")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Card")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Wrestling',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Wrestling")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Wrestling")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Hunting',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Hunting")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Hunting")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  if(grepl('Educational',game_table_distinct[i,4],ignore.case = TRUE)==TRUE){
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("Educational")
    
    genre_df <- cbind(temp_df,genre_df)
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("Educational")
    
    genre_df <- cbind(temp_df,genre_df)
  }
  genre_df <- genre_df[,-19]
  if(i>1){
    genre_table <- rbind(genre_table,genre_df)
  }else{
    genre_table <- genre_df
  }
}

game_table_distinct <- cbind(game_table_distinct,genre_table)

test_df <- game_table_distinct

#creating 'otherGenre' column for games where platform count < 100
#######################################################
valid = 0

for(i in 1:nrow(game_table_distinct)){
  print(i)
  #i=8
  for(j in 27:31){#specific genre columns
    #if(j<32){
      
      if(game_table_distinct[i,j] == 1){
        valid = 1
      }
    #}
  }
  if(valid == 1){
    
    temp_df <- data.frame("1")
    colnames(temp_df) <- c("otherGenre")
    
    if(i>1){
      otherGenre_df <- rbind(otherGenre_df,temp_df)
    }else{
      otherGenre_df <- temp_df
    }
    
    valid = 0 #resetting valid as '0'
  }else{
    temp_df <- data.frame("0")
    colnames(temp_df) <- c("otherGenre")
    
    if(i>1){
      otherGenre_df <- rbind(otherGenre_df,temp_df)
    }else{
      otherGenre_df <- temp_df
    }
  }
}

game_table_distinct <- cbind(game_table_distinct,otherGenre_df)
#######################################################

#removing irrelevant genre columns
game_table_distinct <- game_table_distinct[,-c(27:31)]

#removing old genre column
game_table_distinct <- game_table_distinct[,-c(4)]

#######################################################
write.csv(game_table_distinct, file = "D:/DA/Semester_3/Research Project/Dataset/ign_video_games.csv",fileEncoding = 'UTF-8')
#xxxxxxxx-----xxxxxxxxxx#

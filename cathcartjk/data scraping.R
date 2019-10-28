#Loading the rvest package
library('rvest')


#Specifying the url for desired website to be scrapped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section

rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text

rank_data <- html_text(rank_data_html)

#look at and make sure no NAs and make sure it doesnt need reprocessing 
head(rank_data)


title_data_html <- html_nodes(webpage,'.lister-item-header a')
title_data <- html_text(title_data_html)
head(title_data)

description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)

# removes /n in the description  

description_data<-gsub("\n","",description_data)

head(description_data)




runtime_data_html <- html_nodes(webpage, '.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)
#removes min
runtime_data<-gsub("min","",runtime_data)
runtime_data<-as.numeric(runtime_data)
head(runtime_data)




genre_data_html <- html_nodes(webpage, '.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)

#removes /n

genre_data <- gsub("\n","", genre_data)

#only takes the first genre 

genre_data <- gsub(",.*","", genre_data)
genre_data <- as.factor(genre_data)




rating_data_html <- html_nodes(webpage, '.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
head(rating_data)





votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
votes_data<-gsub(",","",votes_data)
votes_data<-as.numeric(votes_data)
head(votes_data)




directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
directors_data<-as.factor(directors_data)
head(directors_data)



actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
actors_data<-as.factor(actors_data)
head(actors_data)



#Combining all the lists to form a data frame
movies_df <- data.frame(Rank = rank_data, Title = title_data,
                      
                      Description = description_data, Runtime = runtime_data,
                      
                      Genre = genre_data, Rating = rating_data,
                      
                      Votes = votes_data, Director = directors_data, Actor = actors_data)

View(movies_df)

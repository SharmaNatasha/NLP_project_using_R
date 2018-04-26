#Load necessary libraries
library(itunesr)
library(tidytext)
library(udpipe)
library(ggplot2)
library(dplyr)
library(tm)
library(topicmodels)
library(wordcloud)
library(lattice)
library(tmap)
library(widyr)
library(stringr)    

#Get the app id from itunes link - https://itunes.apple.com/in/app/voot/id1011777157?mt=8
#Collect the reviews from first four pages

reviews_page1 <- getReviews('1011777157','in',1)
reviews_page2 <- getReviews('1011777157','in',2)
reviews_page3 <- getReviews('1011777157','in',3)
reviews_page4 <- getReviews('1011777157','in',4)

#Stack the collected reviews 
all_reviews <- rbind(reviews_page1,reviews_page2,reviews_page3,reviews_page4)
head(all_reviews)


### Cleaning - Format the Date and numeric fields

all_reviews$Date <- as.Date(all_reviews$Date)

all_reviews$Rating <- as.numeric(all_reviews$Rating)

### Average Rating by App Version

AvgRating_AppVersion <- all_reviews %>% 
  group_by(App_Version) %>% 
  summarise(avg_rating = mean(Rating),
            count = n()) %>% 
  arrange(desc(count))

### plot for average rating by app version

windows(10,5)
ggplot(AvgRating_AppVersion, aes(count, avg_rating, colour = App_Version)) + 
  geom_point()

### Daily Average Rating

Daily_Avg_Rating <- all_reviews %>% 
  select(Date,Rating) %>% 
  group_by(Date) %>% 
  summarise(Rating = round(mean(Rating),2),
            Count = n())

### plot for daily average rating 
windows(10,5)
ggplot(Daily_Avg_Rating, aes(Date, Rating, colour = Count)) + 
  geom_line()

### function to analyse one start reviews for latest app version/ all app version
one_star_cleaning <- function(all_reviews) {
  
  sss <- all_reviews
  
  files <- sss$Review[sss$Rating=='1' & sss$App_Version=='2.8.12' ]
  #files <- sss$Review[sss$Rating=='1']
  files
  
  files <- iconv(files, "latin1", "ASCII", sub="")
  
  #create corpus from vector
  docs <- Corpus(VectorSource(files))
  
  #start preprocessing
  
  #Transform to lower case
  docs <-tm_map(docs,content_transformer(tolower))
  #remove punctuation
  docs <- tm_map(docs, removePunctuation)
  #Strip digits
  docs <- tm_map(docs, removeNumbers)
  #remove stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  #remove whitespace
  docs <- tm_map(docs, stripWhitespace)
  #Stem document
  docs <- tm_map(docs,stemDocument)
  
  #return document matrix
  docs
  
}


### Create wordcloud to retrieve most used words by cutomers

docs <- one_star_cleaning(all_reviews)

tdm <- TermDocumentMatrix(docs)
tdm

Document_matrix <- as.matrix(tdm)
Document_matrix

sorted_Document_matrix <- sort(rowSums(Document_matrix),decreasing=TRUE)
sorted_Document_matrix

final_mat <- data.frame(word = names(sorted_Document_matrix),freq=sorted_Document_matrix)
final_mat

windows(10,5)
pal <- RColorBrewer::brewer.pal(9,"BrBG")
wordcloud(iconv(final_mat$word, "UTF-8", "ASCII", sub = ""),
          final_mat$freq,c(8,.3),min.freq=2,random.color=TRUE,colors=pal)



### NLP from Udpipe
windows(10,5)
one_star_reviews <- all_reviews$Review[all_reviews$Rating == '1' 
                                              & all_reviews$App_Version =='2.8.12']
one_star_reviews <- as.character(one_star_reviews)

#Execute the below line during first time model download 
#model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

s <- udpipe_annotate(udmodel_english, one_star_reviews)
x <- data.frame(s)

#part of speech
stats <- txt_freq(x$upos)
stats
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "yellow", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

## NOUNS
windows(10,5)
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
#stats$key 
barchart(key ~ freq, data = head(stats, 15), col = "cadetblue", 
         main = "Most occurring nouns", xlab = "Freq")

## ADJECTIVES
windows(10,5)
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
stats$key
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")


## VERBS
windows(10,5)
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
stats$key
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")




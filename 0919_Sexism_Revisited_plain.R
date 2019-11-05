####################
#
# Tim Schleicher
# Sexism Revisited Or Why Our Kids Should Listen To Jazz
# Plain R Script [Please use .Rmd file in /doc instead!]
#
####################


# Load relevant libraries 
library(tidytext)
library(ggplot2)
library(tm)
library(wordcloud)
library(wordcloud2)
library(plyr)
library(dplyr)
library(stringr)
library(SentimentAnalysis)
library(topicmodels)
library(lda)
library(RColorBrewer)
library(imager)
library(tidyr)


# The pre-processing was done along the the starter code given to us. There is one 
# exception though: I did excluded female and male third person singular pronouns 
# from the stop words. Like this I can later look at which songs are talking heavily
# about women (and men). The pre-processing can be retraced in a separate doc in 
# this folder.

load('../output/processed_lyrics_gender.RData') 
lyrics_gender <- dt_lyrics


#### In this step I identify songs that are most about women across all genres

# Put frequencies of "she", etc. from the lyrics in new column.
# Usually names in songs are not mentioned, that's why I use pronouns as 
# identifiers for songs about women
keywords_w <- c(" she ", " her ", " shed ", " shell ", " shes ", " hers ")
keywords_temp <- paste(keywords_w, collapse = '|', sep = '')
lyrics_gender$freq_w <- str_count(lyrics_gender$stemmedwords, keywords_temp) + 1

keywords_m <- c(" he ", " him ", " hed ", " hell ", " hes ", " his ")
keywords_temp <- paste(keywords_m, collapse = '|', sep = '')
lyrics_gender$freq_m <- str_count(lyrics_gender$stemmedwords, keywords_temp) + 1

# Create new column for genre per year 
lyrics_gender <- lyrics_gender %>% mutate(genre_year = paste(genre, year, sep = '_'))

# Remove "she", etc. from the stemmed words
stop_words <- paste(c(keywords_w, keywords_m), collapse = '|', sep = '')
lyrics_gender$stemmedwords <- gsub(stop_words, ' ', lyrics_gender$stemmedwords)
lyrics_gender$stemmedwords <- gsub('  ', ' ', lyrics_gender$stemmedwords)

# Select the 15,000 songs talking about women (and men) most intense

women_songs <- lyrics_gender[order(lyrics_gender$freq_w, decreasing = TRUE),] %>%
  slice(., 1:15000)

men_songs <- lyrics_gender[order(lyrics_gender$freq_m, decreasing = TRUE),] %>%
  slice(., 1:15000)


#### Now I want to know the sentiments present in the songs per genre

# Write function to get sentiments per genre
sentiment_genre <- function(x, y){
  
  text_genre <- filter(x, genre == y)
  sentiment_women_genre <- analyzeSentiment(text_genre$stemmedwords)
  
  sentiment_genre <- convertToBinaryResponse(sentiment_women_genre) %>% 
    select(., SentimentGI)
  
  result <- table(sentiment_genre)
  return(result)
}

#sentiment_genre(women_songs, 'Pop')
#sentiment_genre(women_songs, 'Hip-Hop')

# Create dataframe with sentiment results per genre based on function
genres <- c('Country', 'Electronic', 'Folk', 'Hip-Hop', 'Indie', 'Jazz', 'Metal', 
            'Pop', 'R&B', 'Rock')
sentiments <- data.frame(stringsAsFactors = F)

for(i in genres){
  result <- sentiment_genre(women_songs, i) %>% as.data.frame() %>% mutate(genre = i)
  sentiments <- rbind.fill(sentiments, result)
}

sentiments_w <- sentiments %>% group_by(genre) %>% mutate(sum = sum(Freq)) %>%
  mutate(share = Freq / sum) %>% subset(., sentiment_genre=='negative')

# Visualize results
ggplot() +
  geom_bar(data = sentiments_w, aes(x = reorder(genre, -share), y = share), 
           stat = 'identity', fill = "blue1") + 
  theme(legend.position = "none") +
  labs(x = "Genre", y = "Share of negative sentiments", 
       title = "Negative sentiments in songs about women")

```

To Metal fans, the results might be breathtaking. The plot clearly shows that Metal as well as Hip-Hop songs about women are by far the songs with the highest shares of negative sentiments. Jazz and Folk songs, however, seem to talk the most positive about women. 

### A positive note: What makes Jazz songs about women so nice?

The word cloud with dominant terms in Jazz songs about women is full of "love". Probably, this does not come surprisingly to you since many songs we know are about love. 

However, some of you might argue that certain dominant terms here could even be perceived as sexist. It should be noted though that this word cloud gives an overall positive impression.

```{r echo=FALSE, message=FALSE, warning=FALSE}

#### Here, I want to visualize dominant words of a respective genre

# Write function to get wordcloud from most frequent words
lyrics_shape <- function(x, y){
  
  songs <- filter(x, genre == y)
  
  corpus <- Corpus(VectorSource(songs$stemmedwords))
  m <- as.matrix(TermDocumentMatrix(corpus))
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  return(wordcloud2(d, color = "random-dark"))
}

lyrics_shape(women_songs, "Jazz")

# Apply previously defined function again
lyrics_shape(women_songs, "Metal")

#### Now, I want to model the topics of a respective genre

# Write a function to get topics of a genre
topics_genre <- function(x, y){
  
  text_genre <- filter(x, genre == y)
  
  corpus <- Corpus(VectorSource(text_genre$stemmedwords))
  genre_dtm <- DocumentTermMatrix(corpus)
  
  genre_lda <- LDA(genre_dtm, 3)
  genre_topics <- tidy(genre_lda, matrix = "beta")
  genre_topics
  
  genre_terms <- genre_topics %>%
    group_by(topic) %>%
    top_n(20, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  result <- genre_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = "Term", y = "Beta value", 
         title = "Modeled topics for Hip-Hop songs about women")
  
  print(result)
}

topics_genre(women_songs, 'Hip-Hop') 

#### Finally, I want to look at the development in specific genres over time

# Create function for sentiment of genre in a specific year
sentiment_genre_years <- function(x, y){
  
  text_genre <- filter(x, genre_year == y)
  sentiment_text_genre <- analyzeSentiment(text_genre$stemmedwords)
  
  sentiment_genre <- convertToBinaryResponse(sentiment_text_genre) %>% select(., SentimentGI)
  
  result <- table(sentiment_genre)
  return(result)
}

# Select songs from 1990 on and create dataframe with aggregated results
women_songs_1990 <- women_songs %>% filter(year >= 2000 & 
                                             genre != 'Not Available')

genres <- unique(women_songs_1990$genre_year)

sentiments <- data.frame(stringsAsFactors = F)

for(i in genres){
  result <- sentiment_genre_years(women_songs_1990, i) %>% as.data.frame() %>% mutate(genre = i)
  sentiments <- rbind.fill(sentiments, result)
}


sentiments_w <- sentiments %>% group_by(genre) %>% mutate(sum = sum(Freq)) %>%
  mutate(share = Freq / sum) %>% subset(., sentiment_genre=='negative') %>%
  rename(genre_year = genre) %>% separate(., genre_year, into = c('genre', 'year'), sep = "_") %>%
  mutate(year = as.numeric(year)) %>% arrange(year)

# Visualize Hip-Hop, Jazz, and Metal genre from 2006 to 2016
sentiments_final <- sentiments_w %>% filter(year > 2005) %>%
  filter(genre %in% c("Metal", "Hip-Hop", "Jazz"))

ggplot() +
  geom_line(data = sentiments_final, aes(x = year, y = share, color = genre), size = 1, 
            stat = 'identity') +
  theme_minimal() +
  labs(x = "Time", y = "Share of negative sentiments", 
       title = "Negative sentiments in songs about women over time")

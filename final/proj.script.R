## loading packages
library(LaF); library(dplyr); library(tidyr); library(ggplot2); library(tm);
library(tokenizers); library(stringi); library(quanteda); library(data.table); 
library(stringr); library(knitr); library(caret); library(tidytext)

## Set the path in our directory where the data is stored.
setwd <- (' ')

# Get whole data set after downloading from here https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip and unzip it
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)

# Convert to data frame to better treat them and merge them
twitter <- data_frame(text = twitter)
blogs <- data_frame(text = blogs)
news <- data_frame(text = news)

# Getting a sample from our data
set.seed(122814)
s_twitter <- sample_n(twitter, nrow(twitter) * .10)
s_twitter <- mutate(s_twitter, source = "twtitter")

s_blogs <- sample_n(blogs, nrow(blogs) * .10)
s_blogs <- mutate(s_blogs, source = "blogs")

s_news <- sample_n(news, nrow(news) * .10)
s_news <- mutate(s_news, source = "news")

# Merging the dataset samples
sample <- bind_rows(s_twitter, s_blogs, s_news)

# We're going to work from now on only with the merged sample, so let's clear everything else.
rm(twitter, blogs, news, s_twitter, s_blogs, s_news)

# Convert to ASCII to avoid special characters
sample$text <- iconv(sample$text, from = "UTF-8", to = "ASCII//TRANSLIT")

# Get only lower caps
sample$text <- tolower(sample$text)

# Since numbers are difficult to predict and add little to no information for 

# predicting, we'll also get rid of those
sample$text <- gsub("[0-9]", "", sample$text)

# I had a dilema here of what to do with apostrophes,... at the end, I figured 

# I will just treat them as empty characters
sample$text <- gsub("'", "", sample$text)

# Remove special characters
sample$text <- str_replace_all(sample$text, "[^[:alnum:]]", " ")

# Remove double spacing
sample$text <- str_squish(sample$text)

# Unigrams (Or, you know, as common people know them, words)
ngram1 <- sample %>% 
     unnest_tokens(unigram, text)
# Bigrams
ngram2 <- sample %>% 
     unnest_tokens(bigram, text, token = "ngrams", n = 2)
# Trigrams
ngram3 <- sample %>% 
     unnest_tokens(trigram, text, token = "ngrams", n = 3)
# Quadgrams
ngram4 <- sample %>% 
     unnest_tokens(quadgram, text, token = "ngrams", n = 4)

ngram1 <- ngram1 %>%
     count(unigram) %>%  
     mutate(proportion = n / sum(n)) %>%
     arrange(desc(proportion)) %>%  
     mutate(coverage = cumsum(proportion))
ngram2 <- ngram2 %>%
     count(bigram) %>%  
     mutate(proportion = n / sum(n)) %>%
     arrange(desc(proportion)) %>%  
     mutate(coverage = cumsum(proportion))
ngram3 <- ngram3 %>%
     count(trigram) %>%  
     mutate(proportion = n / sum(n)) %>%
     arrange(desc(proportion)) %>%  
     mutate(coverage = cumsum(proportion))
ngram4 <- ngram4 %>%
     count(quadgram) %>%  
     mutate(proportion = n / sum(n)) %>%
     arrange(desc(proportion)) %>%  
     mutate(coverage = cumsum(proportion))

# The unigram needs no separation, but let's just rename the column
names(ngram1)[1] <- "word"

# Now, we separate an name the rest of the ngrams
ngram2 <- ngram2 %>% 
     separate(bigram, 
              c("word_1", "word"), 
              sep = " ")
ngram3 <- ngram3 %>% 
     separate(trigram, 
              c("word_2", "word_1", "word"), 
              sep = " ")
ngram4 <- ngram4 %>% 
     separate(quadgram, 
              c("word_3", "word_2", "word_1", "word"), 
              sep = " ")

# save database 

saveRDS(ngram1, "ngram1.rds")
saveRDS(ngram2, "ngram2.rds")
saveRDS(ngram3, "ngram3.rds")
saveRDS(ngram4, "ngram4.rds")

# I want my app to give three suggestions.
# In the worst case scenario where no prediction can be made, our program is 
# goint to suggest the three most common words.
top3 <- c(as.character(ngram1[1, 1]), 
            as.character(ngram1[2, 1]), 
            as.character(ngram1[3, 1]))

# Predicting from the last word, with bigrams
pred2 <- function(textInput){
     # Getting the last word
          firstWord <- tail(strsplit(textInput, split = " ")[[1]], 1)[1]
     Filtered <- filter(ngram2, word_1 == firstWord)[1:3, ]
     Suggestion <- Filtered[1:3, 2]
     if(is.na(Suggestion[1, 1])){
          print(top3)
     } else { 
          c(as.character(Suggestion[1, 1]), 
            as.character(Suggestion[2, 1]), 
            as.character(Suggestion[3, 1])
          )
     }
}

# Predicting from the last two words, with trigrams
pred3 <- function(textInput){
     # Getting the last two words
          firstWord <- tail(strsplit(textInput, split = " ")[[1]], 2)[2]
          secondWord <- tail(strsplit(textInput, split = " ")[[1]], 2)[1]
     Filtered <- filter(ngram3, word_1 == firstWord & 
                             word_2 == secondWord)[1:3, ]
     Suggestion <- Filtered[1:3, 3]
     if(is.na(Suggestion[1, 1])){
          pred2(textInput)
     } else { 
          c(as.character(Suggestion[1, 1]), 
            as.character(Suggestion[2, 1]), 
            as.character(Suggestion[3, 1])
          )
     }
}

# Predicting from the last three words, with quadgrams
pred4 <- function(textInput){
     # Getting the last three words words
     firstWord <- tail(strsplit(textInput, split = " ")[[1]], 3)[3]
     secondWord <- tail(strsplit(textInput, split = " ")[[1]], 3)[2]
     thirdWord <- tail(strsplit(textInput, split = " ")[[1]], 3)[1]
     Filtered <- filter(ngram4, word_1 == firstWord & 
                             word_2 == secondWord & 
                             word_3 == thirdWord)[1:3, ]
     Suggestion <- Filtered[1:3, 4]
     if(is.na(Suggestion[1, 1])){
          pred3(textInput)
     } else { 
          c(as.character(Suggestion[1, 1]), 
            as.character(Suggestion[2, 1]), 
            as.character(Suggestion[3, 1])
          )
     }
}

##  that’s the function for getting a prediction for the next word from our text prediction.
predictText <- function(Input){
     # First, clean the text
     textInput <- tolower(Input)
     textInput <- gsub("[0-9]", "", textInput)
     textInput <- gsub("'", "", textInput)
     textInput <- str_replace_all(textInput, "[^[:alnum:]]", " ")
     textInput <- str_squish(textInput)
     # Now, count the number of words in the phrase
     inputLength <- str_count(textInput, '\\w+')
     if(inputLength == 0){
          top3
     } else if(inputLength == 1){
          pred2(textInput)
     } else if(inputLength == 2){
          pred3(textInput)
     } else {
          pred4(textInput)
     }
}

# building a shiny app
if (file.exists("ngrams.R")) {
        source("ngrams.R")
} else {
        print("ngramous.R not found, proceeding without it.") # Optional message
}
library(shiny)
suppressPackageStartupMessages({
        library(tidyverse)
        library(stringr)
})



# Define UI for the application
ui <- fluidPage(
        titlePanel("Text Prediction App"),
        p("This (rather simple) App predicts the next word from a phrase."), 
        p("It was developed as a part of the capstone project in the JHU-Coursera Data Cience Specialization"), 
        
        sidebarLayout(
                sidebarPanel(
                        h2("Instructions"),
                        h5("1. Enter a word of a phrase in the text bar at the side"), 
                        h5("2. Below will be shown a list of 3 possible follow up words"), 
                        h5("3. In case of misspelling, probably no only few words will appear")
                ), 
                mainPanel(
                        textInput("phraseInput", "User Input", width = 600), 
                        h2("Suggested predicted words"), 
                        h3(span(textOutput("suggested_outputs"), style = "color:blue"))
                )
        )
        
)

# Define server logic
server <- function(input, output) {
        # Nothing needs to be done here for a simple static message
        
        output$suggested_outputs <- renderText({
                predictText(input$phraseInput)
        })
}

# Run the application
shinyApp(ui = ui, server = server)

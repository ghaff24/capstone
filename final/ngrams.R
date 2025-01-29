ngram1 <- readRDS("ngram1.rds")
ngram2 <- readRDS("ngram2.rds")
ngram3 <- readRDS("ngram3.rds")
ngram4 <- readRDS("ngram4.rds")

top3 <- c(as.character(ngram1[1, 1]), 
          as.character(ngram1[2, 1]), 
          as.character(ngram1[3, 1]))


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

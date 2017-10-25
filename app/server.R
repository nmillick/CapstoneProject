library(shiny)
library(stringr)
library(tm)

##data
bg <- readRDS("C:/R/CourseraCap/Capstone/app/data/bigram.RData");
tg <- readRDS("C:/R/CourseraCap/Capstone/app/data/trigram.RData"); 
qd <- readRDS("C:/R/CourseraCap/Capstone/app/data/quadgram.RData")

###bigram
names(bg)[names(bg) == 'word1'] <- 'w1'
names(bg)[names(bg) == 'word2'] <- 'w2'

##trigram
names(tg)[names(tg) == 'word1'] <- 'w1'
names(tg)[names(tg) == 'word2'] <- 'w2'
names(tg)[names(tg) == 'word3'] <- 'w3'

##quadgram
names(qd)[names(qd) == 'word1'] <- 'w1'
names(qd)[names(qd) == 'word2'] <- 'w2'
names(qd)[names(qd) == 'word3'] <- 'w3'
names(qd)[names(qd) == 'word4'] <- 'w4'

##prediction function
predictWord <- function(the_word) {
  word_add <- stripWhitespace(removeNumbers(removePunctuation(tolower(the_word),preserve_intra_word_dashes = TRUE)))
  the_word <- strsplit(word_add, " ")[[1]]
  n <- length(the_word)
  
  if (n == 1) {the_word <- as.character(tail(the_word,1)); biFun(the_word)}
  else if (n == 2) {the_word <- as.character(tail(the_word,2)); triFun(the_word)}
  else if (n >= 3) {the_word <- as.character(tail(the_word,3)); quadFun(the_word)}
}

biFun <- function(the_word) {
  if (identical(character(0),as.character(head(bg[bg$w1 == the_word[1], 2], 1)))) {
    as.character(head("it",1))
  }
  else {
    as.character(head(bg[bg$w1 == the_word[1],2], 1))
  }
}

triFun <- function(the_word) {
  if (identical(character(0),as.character(head(tg[tg$w1 == the_word[1]
                                                  & tg$w2 == the_word[2], 3], 1)))) {
    as.character(predictWord(the_word[2]))
  }
  else {
    as.character(head(tg[tg$w1 == the_word[1]
                         & tg$w2 == the_word[2], 3], 1))
  }
}

quadFun <- function(the_word) {
  if (identical(character(0),as.character(head(qd[qd$w1 == the_word[1]
                                                  & qd$w2 == the_word[2]
                                                  & qd$w3 == the_word[3], 4], 1)))) {
    as.character(predictWord(paste(the_word[2],the_word[3],sep=" ")))
  }
  else {
    as.character(head(qd[qd$w1 == the_word[1] 
                         & qd$w2 == the_word[2]
                         & qd$w3 == the_word[3], 4], 1))
  }       
}


##servercall
shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    result <- predictWord(input$inputText)
    result
  });
  output$sentence1 <- renderText({
    input$inputText});
}
)
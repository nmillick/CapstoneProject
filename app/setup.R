library(tm) 
library(NLP) 
library(openNLP) 
library(RWeka)
library(qdap) 


set.seed(1234)

twitfile <- file("//atlas.local/storage/users/nmillick/My Documents/R/Coursera/Capstone/final/en_US/en_US.twitter.txt")
newfile <- file("//atlas.local/storage/users/nmillick/My Documents/R/Coursera/Capstone/final/en_US/en_US.news.txt")
blogfile <- file("//atlas.local/storage/users/nmillick/My Documents/R/Coursera/Capstone/final/en_US/en_US.blogs.txt")

enBlog <- readLines(blogfile, skipNul = TRUE)
enNew <- readLines(newfile, skipNul = TRUE)
enTwit <- readLines(twitfile, skipNul = TRUE)

sampleBlog <- sample(enBlog,5000, replace = TRUE)
sampleNew <- sample(enNew,5000, replace = TRUE)
sampleTwit <- sample(enTwit,5000, replace = TRUE)

sample <- c(sampleBlog,sampleNew,sampleTwit)
txt <- sent_detect(sample)

##remove(sampleBlogs,sampleNews,sampleTwitter,enBlogs,enNews,enTwitter,sample)

txt <- removeNumbers(txt)
txt <- removePunctuation(txt)
txt <- stripWhitespace(txt)
txt <- tolower(txt)
txt <- txt[which(txt!="")]

##Profanity check
profanity <- readLines("ProfanityFilter.txt", skipNul = TRUE)
txt <- Corpus(VectorSource(txt))
txt <- tm_map(txt, removeWords, profanity)

##Bigram
bigram <- NGramTokenizer(txt, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram,
                    one = sapply(str2,"[[",1),
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")

##trigram
trigram <- NGramTokenizer(txt, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
names(trigram) <- c("words","freq")
trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))
trigram <- data.frame(word1 = trigram$one,word2 = trigram$two,
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)

write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")

#quadgram
quadgram <- NGramTokenizer(txt, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]
names(quadgram) <- c("words","freq")
quadgram$words <- as.character(quadgram$words)
str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))
quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)

write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")





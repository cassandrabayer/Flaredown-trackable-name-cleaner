#install.packages("data.table")  
# install.packages("tm")  
#install.packages("spelling")  
#install.packages("rHealthDataGov")  
# install.packages("RISmed")
# install.packages("ngram")
# install.packages("shiny")
#install.packages("stringdist")
#install.packages("dplyr")
setwd('/Users/cassiemerriam/Desktop/FD/FD')
library(data.table)
library(tm)
library(spelling)
library(rHealthDataGov)
library(RISmed)
library(ngram)
library(shiny)
library(stringdist)
library(dplyr)

Get_PubMed_Data <- function(topic, start_date, end_date, return_count) {
  require(RISmed)
  
  search_query <- EUtilsSummary(topic, retmax=return_count, mindate=start_date,maxdate=end_date)
  summary(search_query)
  # see the ids of our returned query
  QueryId(search_query)
  # get actual data from PubMed
  records<- EUtilsGet(search_query)
  class(records)
  # store it
  pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))
  head(pubmed_data,1)
  pubmed_data$Title <- as.character(pubmed_data$Title)
  pubmed_data$Abstract <- as.character(pubmed_data$Abstract)
  pubmed_data$Abstract <- gsub(",", " ", pubmed_data$Abstract, fixed = TRUE)
  
  return (pubmed_data)
}

Text_To_Clean_Sentences <- function(text_blob) {
  # swap all sentence ends with code 'ootoo'
  text_blob <- gsub(pattern=';|\\.|!|\\?', x=text_blob, replacement='ootoo')
  
  # remove all non-alpha text (numbers etc)
  text_blob <- gsub(pattern="[^[:alpha:]]", x=text_blob, replacement = ' ')
  
  # force all characters to lower case
  text_blob <- tolower(text_blob)
  
  # remove any small words {size} or {min,max}
  text_blob <- gsub(pattern="\\W*\\b\\w{1,2}\\b", x=text_blob, replacement=' ')
  
  # remove contiguous spaces
  text_blob <- gsub(pattern="\\s+", x=text_blob, replacement=' ')
  
  # split sentences by split code
  sentence_vector <- unlist(strsplit(x=text_blob, split='ootoo',fixed = TRUE))
  return (sentence_vector)
}

Trim <- function( x ) {
  # http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}


Get_Ngrams <- function(sentence_splits, ngram_size=2) {
  ngrams <- c()
  for (sentence in sentence_splits) {
    sentence <- Trim(sentence)
    if ((nchar(sentence) > 0) && (sapply(gregexpr("\\W+", sentence), length) >= ngram_size)) {
      ngs <- ngram(sentence , n=ngram_size)
      ngrams <- c(ngrams, get.ngrams(ngs))
    }
  }
  return (ngrams)
}

best_match <- function(word){
  matches <- c()
  for (sentence in n_all) {
    # find exact match with double backslash and escape
    if (grepl(paste0('\\<',word), sentence)) {
      print(sentence)
      matches <- c(matches, sentence)
    }
  }
  
  # find highest probability word
  precision_match <- c()
  for (a_match in matches) {
    # how many spaces in from of search word
    precision_match <- c(precision_match,nchar(strsplit(x = a_match, split = word)[[1]][[1]]))
  }
  
  # use highest number and a random of highest for multiples
  best_matched_sentence <- sample(matches[precision_match == max(precision_match)],size = 1)
  print(best_matched_sentence)
  
  # This shows the longest match for our sentence. Now we need to extract the next word after our sentence, in this case it is the word treated:
  
  # split the best matching sentence by the search word
  best_match <- strsplit(x = best_matched_sentence, split = word)[[1]]
  # split second part by spaces and pick first word
  best_match <-  strsplit(x = best_match[[2]], split = " ")[[1]]
  best_match <- best_match[[1]]
  
  print(best_match)
}

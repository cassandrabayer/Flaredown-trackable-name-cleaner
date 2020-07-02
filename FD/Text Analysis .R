#### Run Controller####
source("functions.R")

fd_full <- data.table(read.csv("export.csv"), stringsAsFactors = F)

#make copy
fd_clean <- fd_full

# refactor the older than 90s and drop random stuff
fd_clean <- fd_clean[age > 0]
fd_clean[age >= 90, age := 90]
# write.csv2(fd_clean, "fd_clean_20200417.csv")

#TODO
# scrub punctuation & make uniform capitalization
fd_clean[ , trackable_name := as.character(trackable_name)]
fd_clean[, trackable_name := removePunctuation(trackable_name)]
fd_clean[, trackable_name := tolower(trackable_name)]

# get number of trackable names and pct
fd_clean[, count := .N, by = .(trackable_name, trackable_type)][order(-count)]
fd_clean[, trackable_type_count := .N, by = trackable_type] # gets count of each trackable_type
fd_clean[, pct := (count/trackable_type_count), by = trackable_name][order(-count)]
fd_clean[, pct :=round(pct, 2)]

# find the cut off for number of words
# fd_clean[, num_words := strsplit(trackable_name, " ")]

# get top hits

# Clean up trackable names
# get unique list of trackable names and order them by prevalence
# drop any records that only have 10 matchs in condiiton
fd_clean <- fd_clean[!trackable_name %in% fd_clean[trackable_type == 'Condition', .N, by = trackable_name][N <=10]$trackable_name]
unique_tns <- unique(fd_clean[trackable_type == 'Condition', .(trackable_name, pct)]$trackable_name)
fd_clean[, common_trackable_name := '']

#local func to simplify conditions
namer <- function(term, type){
  tns <- unique(fd_clean[trackable_type == type &
                           common_trackable_name == '' &
                           agrepl(term, fd_clean$trackable_name, max.distance = 1), trackable_name])
  top_hit <- fd_clean[trackable_name %in% tns, .N, by = trackable_name][order(-N)][1]$trackable_name
  # if(nrow(fd_clean[trackable_name %in% tns, .N, by = trackable_name])< 2 ){
  #   fd_clean[, common_trackable_name := 'unique term']
  # }
  fd_clean[trackable_name %in% tns
           & trackable_type == type
           & common_trackable_name == '', common_trackable_name := top_hit]
  list_return = unique(fd_clean[trackable_type == type
                                & !common_trackable_name == top_hit
                                & common_trackable_name == '', .(trackable_name, pct)]$trackable_name)
  return(list_return)
}

for (i in 1:length(unique_tns))
{
  #debug(namer)
  term_current = unique_tns[i]
  if(is.na(term_current)) next
  length_start <- length(unique_tns)
  unique_tns_2 <- namer(term = term_current, type = 'Condition')
  length_end <- length(unique_tns_2)
  num_rows = data.table(length_start = length_start, length_end = length_end)
  #unique_tns <- namer(term = term_current, type = 'Condition')
  print(paste0('Completed iteration number ',
               i,
               ' in namer function for term ',
               term_current,
               ', which has a total of ',
               round(nrow(fd_clean[trackable_name == term_current & trackable_type == "Condition"])
                     /nrow(fd_clean[trackable_type == 'Condition']),2),
               '% of the Condition records. It has dropped ',
               num_rows$length_start - num_rows$length_end,
               " of the records in unique_tns, which is now ",
               num_rows$length_end,
               " records long"))
}





## fuzzy string matching

# spell check
#no run spell_check_text(fd_clean[, trackable_name])


#### Test #1: Medical Word Prediction ####
# lump together like terms with difference names
# medical_corpus <- Get_PubMed_Data("crohn's", 2018, 2020, 5)
# corpus_sentences <- Text_To_Clean_Sentences(paste(medical_corpus$Abstract, collapse=" "))
#
# # split into n-grams of various lengths then bind
# n2 <- Get_Ngrams(corpus_sentences, ngram_size=2)
# n3 <- Get_Ngrams(corpus_sentences, ngram_size=3)
# n4 <- Get_Ngrams(corpus_sentences, ngram_size=4)
# n5 <- Get_Ngrams(corpus_sentences, ngram_size=5)
#
# # consolidate all n-gram vectors into one and save then check
# n_all <- c(n2, n3, n4, n5)
# write.csv(n_all, 'test_crohns_ngrams.csv', row.names=FALSE)
# head(n_all)
# length(n_all)
#
# # Test
# best_match("ibd ")
#
# #### Test #2: Text Clustering ####
# # clean
# # corpus_cleaned <- Text_To_Clean_Sentences(paste(fd_clean$trackable_name, collapse=" "))
# # corpus = tm::Corpus(tm::VectorSource(corpus_cleaned))
# # corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
# # corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words
# # corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words
# # corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace)
#
#
# # vectorize and represent numerically then weight them
# # tdm <- tm::DocumentTermMatrix(corpus_sentences)
# # tdm.tfidf <- tm::weightTfIdf(tdm)
# #
# # # Run distance and store in a matrix
# # tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)
# # tfidf.matrix <- as.matrix(tdm.tfidf)
# # # Cosine distance matrix (useful for specific clustering algorithms)
# # dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")
#
#
# # # Cluster in 3 different ways
# # clustering.kmeans <- kmeans(tfidf.matrix, truth.K)
# # clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2")
# # clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

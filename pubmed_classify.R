##### Libraries #####
library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(readr)
library(stringr)
library(SnowballC)
library(reshape2)
library(syuzhet)
library(tm)
library(RISmed)
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
library(mlr)

##### Load #####
all.abstracts <- readRDS('2017-10-14_all_abstracts_class.rda')
#abstracts.cancer.all <- readRDS('2017-10-14_all_abstracts_cancer_class.rda')

all.abstracts$pmid <-rownames(all.abstracts)
####### tf-idf #########

txt.process <- Corpus(VectorSource(all.abstracts$abstract))
txt.process <- tm_map(txt.process, stripWhitespace)
txt.process <- tm_map(txt.process, removePunctuation, preserve_intra_word_dashes = TRUE)
txt.process <- tm_map(txt.process, content_transformer(tolower))
txt.process <- tm_map(txt.process, removeNumbers)
txt.process.cleaned <- data.frame(text=sapply(txt.process, identity), stringsAsFactors=F)

all.abstracts$cleaned_abstract <- txt.process.cleaned$text

# Generate the bigrams
all.bigrams <- all.abstracts %>%
  select(pmid, cleaned_abstract) %>%
  unnest_tokens(bigram, cleaned_abstract, token = 'ngrams', n = 2) %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

# Drop the bigrams with stopwords
# Then stem the word
eng.stopwords <- stopwords()
all.bigrams.filtered <- all.bigrams %>%
  filter(!word1 %in% eng.stopwords) %>%
  filter(!word1 == '') %>%
  filter(!word2 %in% eng.stopwords) %>%
  filter(!word2 == '') %>% 
  mutate(word1 = wordStem(word1)) %>%
  mutate(word2 = wordStem(word2)) %>%
  unite(bigram, word1, word2, sep = ' ')

# Count and convert to TF_IDF
# Keep bigram counts > 2 
# Possibly drop terms if they appear than 3 times 
all.bigrams.tokeep <- all.bigrams.filtered %>%
  count(pmid, bigram) %>%
  filter(n > 1) %>%
  count(bigram) %>% 
  filter(nn > 2)

# Filter and convert to TF-IDF
all.bigrams.tfidf <- all.bigrams.filtered %>%
  count(pmid, bigram) %>%
  filter(bigram %in% all.bigrams.tokeep$bigram) %>%
  bind_tf_idf(bigram, pmid, n) 

# Make matrix
all.bigrams.matrix <- dcast(all.bigrams.tfidf, pmid ~ bigram, value.var = 'tf_idf', fill=0)

# Join data frames
all.abstracts.full <- left_join(all.abstracts, all.bigrams.matrix, by='pmid')



#saveRDS(all.abstracts.full, '2017-10-14_all_tfidf.rda')


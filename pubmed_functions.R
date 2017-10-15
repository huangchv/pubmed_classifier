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

######### NOTES############
#`I have ~1000 from 2015, can get more from another year`

######################
set.seed(1234)

get.abstracts <- function(
  year_start = 2016, 
  year_end = 2016,
  query= '"journal article"[Publication Type] AND "english"[Language] AND hasabstract[text]', 
  n = 10, 
  output.name = 'random',
  rand.start = FALSE,
  ...) {
  # choose a value between 1 and 5000 for a start point
  rand_start <- 1
  if (rand.start == TRUE) {
    rand_start <- sample(1, 1:5000)
  }
  
  
  # Expand search, but randomly select n number of PMIDS 
  #QueryId(search_query)
  search_query <- EUtilsSummary(query, retmax=(n*10), mindate=year_start, maxdate=year_end, retstart=rand_start)
  all.ids <- search_query@PMID
  
  # Check if retrieved IDs is fewer than n, if so, grab them all
  # We'll deal with this later
  if (length(all.ids) < n) {
    retrieve.these <- all.ids
    warning(paste0("There were only ", length(all.ids) , " abstracts found for this query between " , year_start, ' and ', year_end))
  } else {
    take.these <- sample(1:(length(all.ids)), n, replace = FALSE)
    retrieve.these <- all.ids[take.these]
  }
  
  
  records<- EUtilsGet(retrieve.these)
  
  journal_data <- data.frame(
    'title'=ArticleTitle(records),
    'abstract'=AbstractText(records),
    'journal'=Title(records),
    'citations' = Cited(records),
    'year_pub' = YearPpublish(records),
    'month_pub' = MonthPubmed(records),
    'month_sub' = MonthReceived(records),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  journal_data$abstract <- as.character(journal_data$abstract)
  
  # get number of authors
  journal_authors <- Author(records)
  author_counts <- lapply(journal_authors, function(x) return(nrow(x[4])))
  
  journal_data$n_author <- unlist(author_counts)
  
  
  # Drop samples without abstract 
  journal_data <- journal_data[journal_data$abstract != '', ]
  
  abstract_sentiments <- get_nrc_sentiment(journal_data$abstract)
  
  journal_data_merged <- cbind(journal_data, abstract_sentiments)
  
  journal_data_merged <- journal_data_merged[order(journal_data_merged$citations, decreasing=TRUE),]
  
  # Save RDA file
  saveRDS(journal_data_merged, file = paste0(output.name, '_', year_start, '-', year_end , '_' , n ,'.rda'))
  
  # Return object
  return(journal_data_merged)
}


# Function to piece together query and return the abstract stuff
retrieve.byjournal <- function(some.journal) {
  journal.query <- paste0('(("', some.journal, '"[Journal]) AND ')
  query.tograb <- paste0( journal.query, '"journal article"[Publication Type]) AND "english"[Language] AND hasabstract[text]')
  some.abstracts <- get.abstracts(query.tograb, n=1000, year_start = 2000, year_end = 2017, output.name = some.journal)
  return(some.abstracts)
}

# Same as previous function but searches for cancer
retrieve.byjournal.cancer <- function(some.journal) {
  journal.query <- paste0('("cancer"[Title/Abstract]) (("', some.journal, '"[Journal]) AND ')
  query.tograb <- paste0( journal.query, '"journal article"[Publication Type]) AND "english"[Language] AND hasabstract[text]')
  some.abstracts <- get.abstracts(query.tograb, n=1000, year_start = 2000, year_end = 2017, output.name = paste0(some.journal, '_cancer'))
  return(some.abstracts)
}


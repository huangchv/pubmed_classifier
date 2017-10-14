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

# Set Year
year <- 2016

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
  
  take.these <- sample(1:(length(all.ids)), n, replace = FALSE)
  retrieve.these <- all.ids[take.these]
  
  
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



#### 

#head(journal_data_merged[,c(1,3)])


j.subset <- journal_data_merged[journal_data_merged$citations!=0,]

cor(journal_data_merged[, 4:15],method = 'spearman')

#scatterplot per group
df_melt <- melt(journal_data_merged[,4:15],"citations")
ggplot(df_melt,aes(value,citations)) +
  geom_point() +
  facet_grid(.~variable) +
  xlim(0,50)

#journal_data_merged <- readRDS(file='random_2015.rda')

#nature_2015_1000<- journal_data_merged
#saveRDS(nature_2015_1000, file = 'nature_2015_1k.rda')
#saveRDS(journal_data_merged, file = paste0('random_', year ,'.rda'))

j_data2015 <- readRDS(file='random_2015_5000.rda')
j_data2014 <- readRDS(file='random_2014_5000.rda')
j_data2013 <- readRDS(file='random_2013_5000.rda')
j_data2012 <- readRDS(file='random_2012_5000.rda')
j_data2011 <- readRDS(file='random_2011_5000.rda')
j_data2010 <- readRDS(file='random_2010_5000.rda')

# Add years
j_data2015$year <- 2015
j_data2014$year <- 2014
j_data2013$year <- 2013
j_data2012$year <- 2012
j_data2011$year <- 2011
j_data2010$year <- 2010

j_data_all <- rbind(j_data2015, j_data2014, j_data2013, j_data2012, j_data2011, j_data2010)
dim(j_data_all)

saveRDS(j_data_all, "2017-09-09_2010-2015_abstracts.rda")

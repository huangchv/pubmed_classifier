
journal.target <- 'lancet'
journal.query <- paste0('(("', journal.target, '"[Journal]) AND ')
query.tograb <- paste0( journal.query, '"journal article"[Publication Type]) AND "english"[Language] AND hasabstract[text]')
abstracts.nature <- get.abstracts(query.tograb, n=10, year_start = 2010, year_end = 2017, output.name = journal.target)

# Current plan 
# Grab 2k abstracts per journal 
# journal will consist of nature, cell, science, plos one, scientific reports, nejm, cell reports, lancet, and nature communications
# 18k in total
# Repeat but using cancer as a keyword in query
# Another 18k, for 36k in total


# Function to piece together query and return the abstract stuff
retrieve.byjournal <- function(some.journal) {
  journal.query <- paste0('(("', some.journal, '"[Journal]) AND ')
  query.tograb <- paste0( journal.query, '"journal article"[Publication Type]) AND "english"[Language] AND hasabstract[text]')
  some.abstracts <- get.abstracts(query.tograb, n=1000, year_start = 2000, year_end = 2017, output.name = some.journal)
  return(some.abstracts)
}

# Retrieve
all.journals <- c('nature', 'Scientific reports', 'Cell',
                  'Science', 'PloS one', 'The New England journal of medicine',
                  'lancet', 'cell reports', 'Nature Communications')
abstracts.lancet <- retrieve.byjournal('lancet')
abstracts.nature <- retrieve.byjournal('nature')
abstracts.sreports <- retrieve.byjournal('Scientific reports')
abstracts.cell <- retrieve.byjournal('Cell')
abstracts.science <- retrieve.byjournal('Science')
abstracts.pone <- retrieve.byjournal('PloS one')
abstracts.nejm <- retrieve.byjournal('The New England journal of medicine')
abstracts.creports <- retrieve.byjournal('cell reports')
abstracts.ncomm <- retrieve.byjournal('Nature Communications')
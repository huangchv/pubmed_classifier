# Current plan 
# Grab 1k abstracts per journal 
# journal will consist of nature, cell, science, plos one, scientific reports, nejm, cell reports, lancet, and nature communications
# 9kk in total
# Repeat but using cancer as a keyword in query
# Grab whatever you can get 

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

abstracts.cancer.lancet <- retrieve.byjournal.cancer('lancet')
abstracts.cancer.nature <- retrieve.byjournal.cancer('nature')
abstracts.cancer.sreports <- retrieve.byjournal.cancer('Scientific reports')
abstracts.cancer.cell <- retrieve.byjournal.cancer('Cell')
abstracts.cancer.science <- retrieve.byjournal.cancer('Science')
abstracts.cancer.pone <- retrieve.byjournal.cancer('PloS one')
abstracts.cancer.nejm <- retrieve.byjournal.cancer('The New England journal of medicine')
abstracts.cancer.creports <- retrieve.byjournal.cancer('cell reports')
abstracts.cancer.ncomm <- retrieve.byjournal.cancer('Nature Communications')

# Merge abstract data
abstracts.all <- rbind(abstracts.lancet, abstracts.nature, abstracts.sreports, abstracts.cell, abstracts.science,
                       abstracts.pone, abstracts.nejm, abstracts.creports, abstracts.ncomm)

abstracts.cancer.all <- rbind(abstracts.cancer.lancet, abstracts.cancer.nature, abstracts.cancer.sreports, abstracts.cancer.cell, abstracts.cancer.science, abstracts.cancer.pone, abstracts.cancer.nejm, abstracts.cancer.creports, abstracts.cancer.ncomm)

saveRDS(abstracts.all, '2017-10-14_all_abstracts_class.rda')
saveRDS(abstracts.cancer.all, '2017-10-14_all_abstracts_cancer_class.rda')

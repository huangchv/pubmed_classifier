library(dplyr)
library(tibble)
library(reshape2)
library(tm)
library(reshape2)
library(BoutrosLab.plotting.general)

all.tfidf.full <- readRDS('2017-10-14_all_tfidf.rda')

# Journals to factors 
all.tfidf.full$journal <- factor(all.tfidf.full$journal, labels = 1:9)
colnames(all.tfidf.full) <- make.names(colnames(all.tfidf.full))

journal.lookup <- data.frame(labels = 1:9, 
  journals = c('Cell', 'Cell Reports','Lancet', 'Nature', 'Nat Comms', 'PloS One', 'Science', 'Scientific Reports', 'NEJM'))

##### PCA on sentiments #####
colnames(all.tfidf.full)[1:20]
all.pca <- prcomp(all.tfidf.full[3:12], center = TRUE, scale = TRUE)

all.comps <- data.frame(journal = all.tfidf.full$journal, all.pca$x)
rownames(all.comps) <- all.tfidf.full$
all.comps$pmid <- rownames(all.comps)

comps.toplot <- melt(all.comps, id.vars = c('pmid', 'journal'), measure.vars = c('PC1', 'PC2'))
create.scatterplot(
  data = all.comps,
  formula =  PC4 ~ PC1,
  groups = journal,
  col = default.colours(9)
)

#### Citation Distributions ####
create.boxplot(
  all.tfidf.full,
  formula = journal ~ citations,
  outliers = FALSE,
  xlimits = c(-20,350),
  add.stripplot = TRUE,
  xlab.cex = 1.5,
  ylab.cex = 1.5,
  xlab.label = 'Citations',
  ylab.label = '',
  yaxis.lab = journal.lookup$journals,
  yaxis.cex = 1.25,
  xaxis.cex = 1.25,
  style = 'Nature',
  points.col = 'darkorange',
  points.alpha = 1
  )

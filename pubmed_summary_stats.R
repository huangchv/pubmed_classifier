library(plyr)
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
  width = 6,
  height = 3, 
  resolution = 300,
  filename = generate.filename('journal','citations','png'),
  outliers = FALSE,
  xlimits = c(-20,350),
  add.stripplot = TRUE,
  xlab.cex = 1.25,
  ylab.cex = 1.25,
  xlab.label = 'Citations',
  ylab.label = '',
  yaxis.lab = journal.lookup$journals,
  yaxis.cex = 1,
  xaxis.cex = 1,
  style = 'Nature',
  points.col = 'darkorange',
  points.alpha = 0.75,
  left.padding = 1,
  right.padding = 1
  )

##### Months submitted with more data #####
months.submitted <- aggregate(all.tfidf.full$month_sub, list(all.tfidf.full$journal), unlist(table))
sapply(months.submitted$x, unlist)


months.sub.toplot <- data.frame(matrix(c(unlist(months.submitted$x), rep(0,12)) , ncol=12, byrow = TRUE))
colnames(months.sub.toplot) <- month.abb[1:12]
rownames(months.sub.toplot) <- journal.lookup$journals

months.sub.toplot$sum <- apply(months.sub.toplot, 1, sum)
months.sub.percent.toplot <- months.sub.toplot[,1:12] / months.sub.toplot[,13]
months.sub.percent.toplot$journal <- rownames(months.sub.percent.toplot)
months.sub.percent.melt <- melt(months.sub.percent.toplot[1:8,])
months.sub.percent.melt$variable <- as.numeric(months.sub.percent.melt$variable)

months.sub.percent.melt$journal <- as.factor(months.sub.percent.melt$journal)

line.lty <- rep(1,8)
line.lty[c(2,5)] <- 2 
line.lty[8] <- 3 

journal.cols <- c('darkorange', 'darkorange', 'chartreuse4', 'darkorchid4', 'darkorchid4', "dodgerblue", "firebrick3", 'darkorchid4')

create.scatterplot(
  data = months.sub.percent.melt,
  formula = value ~ variable,
  groups = journal,
  col = journal.cols,
  type = c('l', 'g'),
  width = 8,
  height = 5,
  filename = generate.filename('papers','month_sub','png'),
  style = 'Nature',
  resolution = 300,
  lwd = 2,
  lty = line.lty,
  xlimits = c(0.5, 12.5),
  xat = 1:12,
  ylimits = c(-0.02, 0.22),
  xaxis.lab = month.abb,
  xlab.label = '',
  xaxis.cex = 1,
  yaxis.cex = 1,
  ylab.label = 'Papers (%)',
  ylab.cex = 1.15,
  key = list(
    text = list(
      lab = as.vector(journal.lookup$journals[1:8]),
      cex = 1,
      col = 'black'
    ),
    points = list(
      pch = 19,
      col = journal.cols,
      cex = 1
    ),
    x = 0.70,
    y = 0.95,
    padding.text = 2
  ),
  left.padding = 1,
  right.padding = 1
)

library(dplyr)
library(tibble)
library(SnowballC)
library(reshape2)
library(tm)
library(lattice)
library(ggplot2)
library(reshape2)
library(caret)
library(mlr)

all.tfidf.full <- readRDS('2017-10-14_all_tfidf.rda')

# Journals to factors 
all.tfidf.full$journal <- factor(all.tfidf.full$journal, labels = 1:9)

# Drop columns
#colnames(all.abstracts.full)[1:21]
omit.cols <- c(1,2,4,5,6,7,19,20)

all.tfidf.full <- all.tfidf.full[,-omit.cols]
colnames(all.tfidf.full) <- make.names(colnames(all.tfidf.full))

# Split into train test
data.part <- createDataPartition(all.tfidf.full$journal, p = 0.75, list=FALSE)
train.data <- all.tfidf.full[data.part,]
test.data <- all.tfidf.full[-data.part,]


# Task
classif.task = makeClassifTask(id = "pubmed_xgb_class", data = train.data, target = 'journal')

# Define learner
xgboost.lrn <- makeLearner(cl = 'classif.xgboost', predict.type = 'prob')

getParamSet(xgboost.lrn)
xgboost.lrn <- setHyperPars(
  xgboost.lrn, 
  eta = .03333, 
  subsample = .7, 
  max_depth = 4, 
  nrounds = 200,
  objective = 'multi:softprob', 
  eval_metric = 'mlogloss', 
  nthread = 4, 
  gamma=1,
  alpha=1,
  colsample_bytree = 0.7)

xgboost.lrn$par.vals


# Tune nrounds
rounds.params <- makeParamSet(
  makeDiscreteParam('nrounds', values = seq(250,600,50))
)


# CV strat
re.strat <- makeResampleDesc("CV", iters=4)

# tuning strat - random fun
ctrl <- makeTuneControlGrid(resolution=5)

# Tune =| 
mytune <- tuneParams(
  learner = xgboost.lrn, 
  task = classif.task, 
  resampling = re.strat, 
  measures = list(logloss, multiclass.au1p ),
  par.set = rounds.params, 
  control = ctrl, 
  show.info = T)

# 200 Was the best

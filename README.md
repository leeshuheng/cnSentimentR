# cnSentimentR
Analyse sentiment of Chinese text.

R package.

devtools::install_github("leeshuheng/cnSentimentR")

train.set <- read.csv("./train_set.csv", header = T, sep = ",", stringsAsFactors = F)

train.set <- train.set[,c("sentiment", "content")]

library(cnSentimentR)

train.set <- cnsr.prepare(train.set)

fit <- cnsr.train(train.set)

# test.set <- read.csv("./test_set.csv", header = T, sep = ",", stringsAsFactors = F)

test.set <- train.set[sample(1:nrow(train.set), 100, replace = F),]

test.set <- cnsr.predict(fit, test.set)

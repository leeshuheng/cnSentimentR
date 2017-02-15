# cnSentimentR
Analyse sentiment of Chinese text.

An R package.

```

devtools::install_github("leeshuheng/cnSentimentR")

train.set <- read.csv("./train_set.csv", header = T, sep = ",", stringsAsFactors = F)

train.set <- train.set[,c("sentiment", "content")]

library(cnSentimentR)

train.set <- cnsr.prepare(train.set)

fit <- cnsr.train(train.set)

# test.set <- read.csv("./test_set.csv", header = T, sep = ",", stringsAsFactors = F)

# test.set <- cnsr.prepare(test.set)

test.set <- train.set[sample(1:nrow(train.set), 100, replace = F),]

test.set <- cnsr.predict(fit, test.set)

cnsr.topic.word(test.set)
cnsr.topic.word(test.set, topicn = 5, topn = 15)

cnsr.keyword(test.set)
cnsr.keyword(test.set, topn = 20)

```

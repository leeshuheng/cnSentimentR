### 2015年 03月 11日 星期三 08:55:50 CST
### modify 2015年 04月 07日 星期二 10:20:51 CST
### modify 2015年 04月 20日 星期一 13:19:28 CST
### modify 2017年 02月 07日 星期二 11:52:51 CST
### author: 李小丹(Li Shao Dan) 字 殊恒(shuheng)
### K.I.S.S
### S.P.O.T

### TODO: stop words

library(jiebaR)
library(e1071)
library(tm)
library(SparseM)
library(lda)
library(reshape2)
#library(stringr)


.do.segment <- function(seg.fun, sz) {
	tryCatch({
		res <- seg.fun(sz)
	}, error = {
		res <- c()
	})
	if(length(res) > 0) {
		res <- tolower(res)
		#res <- rm.stop_word(res, sw)
	}
	if(length(res) > 0)
		res <- paste(res, collapse = " ")
	else
		res <- ""
	return(res)
}

## ugly
## FIXME
.cal.sentiment <- function(mod, text) {
	#text <- do.segment(mixseg, stop.words, text)
	#print(text)
	mod.row <- mod[["nrow"]]
	svm.mod <- mod[["mod"]]
	terms <- mod[["terms"]]

	test.txt <- scan_tokenizer(text)
	test.txt <- table(test.txt)

	test <- rep(0, mod.row)

	w <- names(test.txt)
	for(i in 1:length(w))
		test[terms %in% names(test.txt)[i]] <- test.txt[i]

	pred <- predict(svm.mod, as.matrix.csr(test, nrow = 1), probability = T)
	prob <- attr(pred, "probabilities")

	val <- max(prob)
	label <- dimnames(prob)[[2]][which(prob == val)]

	return(list(value = val[1],
				label = label[1]))
}

.create.svm.mod <- function(train, control) {
	cost <- control[["cost"]]
	cross <- control[["cross"]]
	tolerance <- control[["tolerance"]]
	gamma <- control[["gamma"]]

	txt <- Corpus(VectorSource(train$seg))
	dtm <- DocumentTermMatrix(txt,
					 control = list(wordLengths = c(4, Inf),
									removeNumbers = T))
	mod.row <- ncol(dtm)
	mod <- svm(dtm, as.character(train$sentiment),
			   probability = T,
			   type = "C-classification", cost = cost,
			   cross = cross, tolerance = tolerance, method = "SVM")
	return(list(terms = Terms(dtm), nrow = mod.row, mod = mod))
}

.lda.topic.word <- function(data, topicn, topn) {
	corpus <- lexicalize(data$seg)
	res <- lda.collapsed.gibbs.sampler(corpus$documents, topicn,
									   corpus$vocab, 25, 0.1, 0.1)
	return(top.topic.words(res$topics, topn, by.score = T))
}

.tfidf.keyword <- function(data, topn) {
	txt <- Corpus(VectorSource(data$seg))
	tdm <- TermDocumentMatrix(txt, control = list(wordLengths = c(4, Inf),
												  removeNumbers = T))
	tfidf <- weightTfIdf(tdm)

	#term <- Terms(tfidf)
	tfidf <- data.matrix(tfidf)
	#rownames(tfidf) <- term
	tfidf <- melt(tfidf)

	tfidf <- aggregate(tfidf$value, by = list(tfidf$Terms), FUN = sum)
	names(tfidf) <- c("Terms", "value")
	tfidf <- tfidf[order(tfidf$value, decreasing = T),]

	return(tfidf[1:topn,])
}

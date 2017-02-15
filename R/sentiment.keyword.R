### 2017年 02月 15日 星期三 11:39:35 CST
### 2017年 02月 15日 星期三 10:26:13 CST ##########
### author: 李小丹(Li Shao Dan) 字 殊恒(shuheng)
### K.I.S.S
### S.P.O.T

cnsr.keyword <- function(data, topn = 10) {
	pos <- subset(data, sentiment == 1)
	neg <- subset(data, sentiment == 0)

	pos.kw <- .tfidf.keyword(pos, topn)
	neg.kw <- .tfidf.keyword(neg, topn)

	return(list(pos = pos.kw, neg = neg.kw))
}

### 2017年 02月 15日 星期三 10:26:13 CST ##########
### author: 李小丹(Li Shao Dan) 字 殊恒(shuheng)
### K.I.S.S
### S.P.O.T


cnsr.topic.word <- function(data, topicn = 3, topn = 10) {
	pos <- subset(data, sentiment == 1)
	neg <- subset(data, sentiment == 0)

	pos.res <- .lda.topic.word(pos, topicn, topn)
	neg.res <- .lda.topic.word(neg, topicn, topn)

	return(list(pos = pos.res, neg = neg.res))
}

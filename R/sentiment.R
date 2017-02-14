### 2017年 02月 07日 星期二 10:33:43 CST
### author: 李小丹(Li Shao Dan) 字 殊恒(shuheng)
### K.I.S.S
### S.P.O.T

cnsr.prepare <- function(data, seg.fun = NULL) {
	if(is.null(seg.fun)) {
		seg <- worker()
		seg.fun <- function(sz) {
			return(seg <= sz)
		}
	}
	data$seg <- ""
	for(i in 1:nrow(data))
		data[i,]$seg <- .do.segment(seg.fun, data[i,]$content)
	data$content <- NULL
	return(data)
}

cnsr.train <- function(data, control = list(cost = 100, cross = 0,
											tolerance = 0.001)) {
	return(.create.svm.mod(data, control))
}

cnsr.predict <- function(fit, text) {
	text$sentiment <- 0
	text$value <- 0.0
	for(i in 1:nrow(text)) {
		res <- .cal.sentiment(fit, text[i,]$seg)
		text[i,]$sentiment <- as.numeric(res[["label"]])
		text[i,]$value <- res[["value"]]
	}
	return(text)
}

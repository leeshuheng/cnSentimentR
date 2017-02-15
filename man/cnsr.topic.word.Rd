\name{cnsr.topic.word}
\alias{cnsr.topic.word}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Extract topic words
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cnsr.topic.word(data, topicn = 3, topn = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  data.frame
}
  \item{topicn}{
	  Number of topic.
}
  \item{topn}{
	  Number of word.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Return positive and negative topic matrix.
}
\references{
	URL: https://github.com/leeshuheng/cnSentimentR
}
\author{
	\packageAuthor{cnSentimentR}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	cnsr.predict,cnsr.train,cnsr.prepare,cnsr.keyword
}
\examples{
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

cnsr.keyword(test.set)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

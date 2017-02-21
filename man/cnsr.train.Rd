\name{cnsr.train}
\alias{cnsr.train}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	cnsr.train
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cnsr.train(data, control = list(cost = 100, cross = 0, tolerance = 0.001, gamma = 1/ncol(train)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data.frame,result of cnsr.prepare. It must contain column 'sentiment', This column is the label of context,
	  '0' is negative, '1' is positive.
}
  \item{control}{
	  Some parameters of svm.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
	cnsr.predict,cnsr.prepare,cnsr.topic.word,cnsr.keyword
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

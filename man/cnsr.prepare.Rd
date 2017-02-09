\name{cnsr.prepare}
\alias{cnsr.prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	cnsr.prepare
}
\description{
	Preprocess text.
}
\usage{
cnsr.prepare(data, seg = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	  A data.frame. It must contain a column 'content' that is chinese context.
}
  \item{seg}{
	  A segment word machine. If it is 'NULL', function use 'jiebaR'
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	return a data.frame.
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
	cnsr.predict,cnsr.train
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

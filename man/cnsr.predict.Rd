\name{cnsr.predict}
\alias{cnsr.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	cnsr.predict
}
\description{
	Calculate sentiment of a Chinese sentence
}
\usage{
cnsr.predict(fit, text)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
	  reture from cnsr.train
}
  \item{text}{
	  a data.frame, return from cnsr.prepare
}
}
\details{
	Calculate sentiment of a Chinese sentence
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	cnsr.train,cnsr.prepare
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fit, text) 
{
    res <- cal.sentiment(fit, text)
    return(list(sentiment = as.numeric(res[["label"]]), value = res[["value"]]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

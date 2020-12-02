\name{run_ordinal_regression_effect}
\alias{run_ordinal_regression_effect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ordered Logistic Regression
}
\description{
This function runs Ordered Logistic Regression to find how attribute (and level) influences the overall likeliness to download.
}
\usage{
run_ordinal_regression_effect(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
chracter vector consisting of the attribute parameters.
}
}
\details{
The output is a summary table for the regression statistics as well as plot of influence effect of the attributes levels.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
run_ordinal_regression_effect(c("price","duration"))
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


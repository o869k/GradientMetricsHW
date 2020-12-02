\name{generate_optimal_message}
\alias{generate_optimal_message}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generating the optimal message to be presented and its probability of success per individual
}
\description{
This function present the best combination of attributes to be displayed in the message, based on the indivudal parameters of the responder.It also present the probability to reach Very or Somewhat likely to answer.
}
\usage{
generate_optimal_message(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a data frame consist of the individual paramers as in survey data
}
}
\details{
The probability is being estimated using a Random Forest model which was trained over the whole population with an MAE of 0.35
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
generate_optimal_message(unique(dataset[dataset$response_id=="R_10HSe2bbKGbKZ2h",-c(1:9)]))

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.



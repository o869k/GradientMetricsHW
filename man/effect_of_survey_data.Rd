\name{effect_of_survey_data}
\alias{effect_of_survey_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Effects of survey data}
\description{
This function attempts to describe the responders with the effect of their additional survey data and its influence on the overall likeliness to download
}
\usage{
effect_of_survey_data(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a string represent the survey information to be tested
}
}
\details{
The function plots a ggplot with the influence of each additonal data level and its probability spread on the 4 different levels of likeliness
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
effect_of_survey_data("d_education")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.


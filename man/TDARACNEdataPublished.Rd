\name{TDARACNEdataPublished}
\Rdversion{1.1}
\alias{TDARACNEdataPublished}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TDARACNEdataPublished
}
\description{ main function with reference paper data. This reproduce the paper results.
Simply run the function with no arguments to obtain the paper results. }
\usage{
TDARACNEdataPublished()
}
%- maybe also 'usage' for other objects documented here.
\examples{
	## take the paper data
	library(TDARACNE)
	data(dataYeast)
	data(dataSOSmean)
	data(dataIRMAon)
	data(threshIRMAon)
	data(threshSOSmean)
	data(threshYeast)
	## paper results
     	TDARACNEdataPublished()
	## see in your working directory for .dot files
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
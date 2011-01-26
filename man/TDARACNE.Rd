\name{TDARACNE}
\Rdversion{1.1}
\alias{TDARACNE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TDARACNE
}
\description{Main function, see P. Zoppoli, S. Morganella, M. Ceccarelli. TimeDelay-ARACNE: Reverse engineering of gene networks from time-course data by an information theoretic approach. BMC Bioinformatics 2010, 11:154.}
\usage{
TDARACNE(eSet,N,delta=3,likehood=1.2,norm=2,logarithm=1,thresh=0,ksd=1,tolerance=0.15,plot=FALSE,dot=FALSE,name="youHaveForgottenIt",adj=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eSet}{
%%     ~~Describe \code{eSet} here~~
eSet is the ExpressionSet object
}
  \item{N}{
%%     ~~Describe \code{N} here~~
N is respectively the number of bins in percentile normalization or in rank normalization

}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
delta is the maximum time delay allowed to infer connections
}
  \item{likehood}{
%%     ~~Describe \code{likehood} here~~
likehood is the fold change used as threshold to state the initial change expression (IcE) 
}
  \item{norm}{
%%     ~~Describe \code{norm} here~~
if you want column percentile normalization put norm == 1;
if you want Rank normalization put norm == 2;

}
  \item{logarithm}{
%%     ~~Describe \code{logarithm} here~~
if z is log put logarithm == 0;
}
  \item{thresh}{
%%     ~~Describe \code{thresh} here~~
the Influence threshold.
if you have a threshold and a SD put them here in this format: c(thresh,SD)
if you don't have threshold put 0 in thresh;

}
  \item{ksd}{
%%     ~~Describe \code{ksd} here~~
ksd is the standard deviation multiplier;
}
  \item{tolerance}{
%%     ~~Describe \code{tolerance} here~~
tolerance is the DPI tolerance;
0 means no tolerance
1 means no DPI
0.15 is the default ARACNE tolerance as it is for TDARACNE
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
plot must be TRUE to obtain automatically the graph
}
  \item{dot}{
%%     ~~Describe \code{dot} here~~
dot must be TRUE to obtain a .dot file 
}
  \item{name}{
%%     ~~Describe \code{name} here~~
name must be written with quotation marks(like this:'examplename') and is the name of the .dot file produced;
}
  \item{adj}{
%%     ~~Describe \code{adj} here~~
adj must be TRUE to obtain an adjacent matrix
}
}

\examples{
	## take paper data
	library(TDARACNE)
	data(dataIRMAon)
	data(threshIRMAon)
	## main function; in output gives to you and adj matrix and a .dot file
# eSet is the ExpressionSet object
# N is respectively the number of bins in percentile normalization or in rank normalization
# delta is the maximum time delay allowed to infer connections
# likehood is the fold change used as threshold to state the initial change expression (IcE) 
# if you want column percentile normalization put norm == 1;
# if you want Rank normalization put norm == 2;
# if z is log put logarithm == 0;
# if you don't have threshold put 0 in thresh;
# ksd is the standard deviation multiplier;
# tolerance is the DPI tolerance;
# plot must be TRUE to obtain automatically the graph
# dot must be TRUE to obtain a .dot file 
# name must be written with quotation marks(like this:'examplename') and is the name of the .dot file produced;
# adj must be TRUE to obtain an adjacent matrix

TDARACNE(dataIRMAon,11,"netIRMAon",delta=3,likehood=1.2,norm=2,logarithm=1,thresh=threshIRMAon,ksd=0,tolerance=0.15);
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
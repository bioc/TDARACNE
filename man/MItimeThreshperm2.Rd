\name{MItimeThreshperm2}
\Rdversion{1.1}
\alias{MItimeThreshperm2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MItimeThreshperm2
}
\description{Compute the threshold of the d-delayed Mutual information }
\usage{
MItimeThreshperm2(z, N, delta, norm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
z is the data matrix
}
  \item{N}{
N is respectively the number of bins in percentile normalization or in rank normalization
}
  \item{delta}{
delta is the maximum time delay allowed to infer connections
}
  \item{norm}{
if you want column percentile normalization put norm == 1;
if you want Rank normalization put norm == 2;
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
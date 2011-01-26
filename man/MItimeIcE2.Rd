\name{MItimeIcE2}
\Rdversion{1.1}
\alias{MItimeIcE2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MItimeIcE2
}
\description{Compute the d-delayed Mutual information all over the whole set of genes}
\usage{
MItimeIcE2(z, N, delta, norm, threshold, ksd, IcE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{
z is the data matrix}
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
  \item{threshold}{
the Influence threshold.
if you have a threshold and a SD put them here in this format: c(thresh,SD)
if you don't have threshold put 0 in thresh;
}
  \item{ksd}{
ksd is the standard deviation multiplier;
}
  \item{IcE}{
the IcE value
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
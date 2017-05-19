\name{CATT}
\alias{CATT}
\title{The Cochran-Armitage Trend Test}
\description{
This function conducts the Cochran-Armitage trend test to a 2 by k contingency table. It will report the test statistic (Z) and p-value. A linear trend in the frequencies will be calculated, because the weights (0,1,2) will be used by default.
}
\keyword{Cochran-Armitage; Trend Test}
\usage{
CATT(binomial,ordinal,table)
}
\arguments{
  \item{ordinal}{the vector of the ordinal variable}
  \item{binomial}{the vector of the binomial variable}
  \item{table}{option, the contingency table of table(binomial,ordinal)}
}
\value{
  \item{Z}{the test statistic}
  \item{p.value}{the p value of the hypothesis test}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Reference:

1. Cochran, WG (1954). Some methods for strengthening the common chi-squared tests. Biometrics. International Biometric Society. 10 (4): 417-451.

2. Armitage, P (1955). Tests for Linear Trends in Proportions and Frequencies. Biometrics. International Biometric Society. 11 (3): 375-386.

Update:

Version 0.2.0: The p value of two side was specified.

}
\examples{
# type of data is variable
binomial=c(rep(0,20),rep(1,10),rep(0,20),rep(1,20),rep(0,20),rep(1,30))
ordinal=c(rep(0,30),rep(1,40),rep(2,50))
CATT(binomial=binomial,ordinal=ordinal)

# type of data is table
tbl=matrix(c(20,10,20,20,20,30),nrow=2)
CATT(table=tbl)
}

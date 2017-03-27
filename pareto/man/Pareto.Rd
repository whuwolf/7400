\name{Pareto}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}
\alias{p.dpareto}
\alias{p.ppareto}
\alias{p.qpareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Pareto Distribution}
\description{
  Density, distribution function and quantile function for the Pareto distribution with scale equal to \code{alpha} and shape equal to \code{beta}.
}
\usage{
dpareto(x, alpha, beta, log = FALSE)
p.dpareto(x, alpha, beta, log = FALSE, P = 1)
ppareto(q, alpha, beta, lower.tail = TRUE, log.p = FALSE)
p.ppareto(q, alpha, beta, lower.tail = TRUE, log.p = FALSE, P = 1)
qpareto(p, alpha, beta, lower.tail = TRUE, log.p = FALSE)
p.qpareto(p, alpha, beta, lower.tail = TRUE, log.p = FALSE, P = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities.
}
  \item{alpha}{
  scale parameters. Must be positive.
}
  \item{beta}{
  shape parameters. Must be positive.
}
  \item{log, log.p}{
  logical; if TRUE (default), probabilities/densities \code{p} are returned as \code{log(p)}.
}
  \item{lower.tail}{
  logical; if TRUE (default), probabilities are \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.
}
  \item{P}{
  the number of computational threads to use for the loop.
}

}
\details{
  The pareto distribution has density
    \deqn{f(x) = \frac{\beta \alpha^{\beta}}{x^{\beta+1}}}{%
    f(x) = \beta \alpha^\beta/x^(\beta+1)}
  where \eqn{0 < \alpha < x} and \eqn{\beta > 0}.
}
\value{
  \code{dpareto}, \code{p.dpareto} gives the density, \code{ppareto}, \code{p.ppareto} gives the distribution function and \code{qpareto}, \code{p.qpareto} gives the quantile function. \cr

  Invalid arguments will result in return value NaN, with a warning.\cr

  The length of the result is the maximum of the lengths of the numerical arguments.\cr

  The numerical arguments are recycled to the length of the result. Only the first element of the logical argument is used.
}

\author{Yiheng Liu}

\examples{
dpareto(4, 2, 1)
dpareto(1:5, 2, 1)
dpareto(1:5, 2, 1, log = TRUE)
p.dpareto(1:10^6, 2, 1, P = 2)

ppareto(4, 2, 1)
ppareto(1:5, 2, 1)
ppareto(1:5, 2, 1, lower.tail = FALSE)
ppareto(1:5, 2, 1, lower.tail = FALSE, log.p = TRUE)
p.ppareto(1:10^6, 2, 1, lower.tail = FALSE, log.p = TRUE, P = 2)

qpareto(0.5, 2, 1)
qpareto(seq(0,1,0.1), 2, 1)
qpareto(seq(0,1,0.1), 2, 1, lower.tail = FALSE)
qpareto(log(seq(0,1,0.1)), 2, 1, lower.tail = FALSE, log.p = TRUE)
p.qpareto(log(seq(0,1,10^(-6))), 2, 1, lower.tail = FALSE, log.p = TRUE, P = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

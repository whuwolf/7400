\name{Pareto}
\alias{dpareto}
\alias{ppareto}
\alias{qpareto}
\alias{p.dpareto}
\alias{p.ppareto}
\alias{p.qpareto}
\alias{rpareto}
\alias{rcpareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Pareto Distribution}
\description{
  Density, distribution function, quantile function and random generation for the Pareto distribution with scale equal to \code{alpha} and shape equal to \code{beta}. Parallel computation is supported Open MP.
}
\usage{
dpareto(x, alpha, beta, log = FALSE)
p.dpareto(x, alpha, beta, log = FALSE, P = 1)
ppareto(q, alpha, beta, lower.tail = TRUE, log.p = FALSE)
p.ppareto(q, alpha, beta, lower.tail = TRUE, log.p = FALSE, P = 1)
qpareto(p, alpha, beta, lower.tail = TRUE, log.p = FALSE)
p.qpareto(p, alpha, beta, lower.tail = TRUE, log.p = FALSE, P = 1)
rpareto(n, alpha, beta)
rcpareto(n, alpha, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, q}{
  vector of quantiles.
}
  \item{p}{
  vector of probabilities.
}
  \item{n}{
  number of observations. If length(n) > 1, the length is taken to be the number required.
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
  \code{dpareto}, \code{p.dpareto} give the density, \code{ppareto}, \code{p.ppareto} give the distribution function, \code{qpareto}, \code{p.qpareto} give the quantile function, and \code{rpareto}, \code{rcpareto} generate random deviates.\cr

  Invalid arguments will result in return value NaN, with a warning.\cr

  The length of the result is determined by \code{n} for \code{rpareto} and \code{rcpareto}, and is the maximum of the lengths of the numerical arguments for the other functions.\cr

  The numerical arguments other than \code{n} are recycled to the length of the result. Only the first elements of the logical arguments are used.
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

qqplot(ppareto(rpareto(10^4,3,2),3,2), seq(0,1, length.out = 10^4), type = "l")
qqplot(ppareto(rcpareto(10^4,3,2),3,2), seq(0,1, length.out = 10^4), type = "l")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

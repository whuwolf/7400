\name{dpareto}
\alias{dpareto}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The Pareto Distribution}
\description{
  Density function for the pareto distribution with scale equal to \code{alpha} and shape equal to \code{beta}.
}
\usage{
dpareto(x, alpha, beta, log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  vector of quantiles.
}
  \item{alpha}{
  scale parameters. Must be positive.
}
  \item{beta}{
  shape parameters. Must be positive.
}
  \item{log}{
  logical; if TRUE, probabilities \code{p} are given as \code{log(p)}.
}
}
\details{
  The pareto distribution has density
    \deqn{f(x) = \frac{\beta \alpha^{\beta}}{x^{\beta+1}}}{%
    f(x) = \beta \alpha^\beta/x^(\beta+1)}
  where \eqn{0 < \alpha < x} and \eqn{\beta > 0}.
}
\value{
  \code{dpareto} gives the density.\cr

  Invalid arguments will result in return value NaN, with a warning.\cr

  The length of the result is the maximum of the lengths of the numerical arguments.\cr

  The numerical arguments are recycled to the length of the result. Only the first element of the logical argument is used.
}

\author{Yiheng Liu}

\examples{
dpareto(4, 2, 1)
dpareto(1:5, 2, 1)
dpareto(1:5, 2, 1, log = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

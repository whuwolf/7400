\name{class2}
\alias{classify}
\alias{classify.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Classification}
\description{
  Logistic Regression, LDA and Naive Bayes Classifier.
}
\usage{
classify(y, X, method = c("logistic", "lda", "naiveBayes"), intercept = TRUE, indCont = NULL)
classify.predict(model, X)
%- maybe also 'usage' for other objects documented here.
}
\arguments{
  \item{y}{
  labels of classes.
}
  \item{X}{
  observation of predictors.
}
  \item{method}{
  specify the classifier to use.
}
  \item{intercept}{
  logical; if TRUE (default), intercept will appear in the LR model.
}
  \item{indCont}{
  the index of continuous variables in X.
}
  \item{model}{
  An object produced by classify
}
}

\author{Yiheng Liu}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}

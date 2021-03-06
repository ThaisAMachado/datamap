% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{model_test}
\alias{model_test}
\title{Applies a fitted model to the test data}
\usage{
model_test(model, test, label, threshold)
}
\arguments{
\item{model}{The fitted model.}

\item{test}{The test data without the label.}

\item{label}{The test label.}

\item{threshold}{The threshold for classifying the probabilities
as the positive class.}
}
\value{
A list containing the probabilities estimated by the model
and the confusion matrix.
}
\description{
This function applies a fitted model to predict probabilities to the test data.
}

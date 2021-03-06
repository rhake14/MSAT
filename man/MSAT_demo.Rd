% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demo.R
\name{MSAT_demo}
\alias{MSAT_demo}
\title{Demo MSAT}
\usage{
MSAT_demo(
  num_items = 3L,
  feedback = MSAT::MSAT_feedback_with_score(),
  admin_password = "test123",
  researcher_email = "robin.hake@uni-oldenburg.de",
  dict = MSAT::MSAT_dict,
  language = "en",
  ...
)
}
\arguments{
\item{num_items}{(Integer scalar) Number of items in the test.}

\item{feedback}{(Function) Defines the feedback to give the participant
at the end of the test. Defaults to a graph-based feedback page.}

\item{admin_password}{(Scalar character) Password for accessing the admin panel.
Defaults to \code{"demo"}.}

\item{researcher_email}{(Scalar character)
If not \code{NULL}, this researcher's email address is displayed
at the bottom of the screen so that online participants can ask for help.
Defaults to \email{longgold@gold.uc.ak},
the email address of this package's developer.}

\item{dict}{The psychTestR dictionary used for internationalisation.}

\item{language}{The language you want to run your demo in.
Possible languages include English (\code{"en"}), German (\code{"de"})).
The first language is selected by default}

\item{...}{Further arguments to be passed to \code{\link{MSAT}()}.}
}
\description{
This function launches a demo for the RAT.
}

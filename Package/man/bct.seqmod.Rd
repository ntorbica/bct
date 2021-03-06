% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bct_seqmod.R
\name{bct.seqmod}
\alias{bct.seqmod}
\title{Injection sequence modification}
\usage{
bct.seqmod(info, bn)
}
\arguments{
\item{info}{Information table generated by bct.infogen}

\item{bn}{Batch name vector, containing batch names as in information table}
}
\value{
Returns a numeric vector containing the modified injection sequence.
}
\description{
Modifies the present injection sequence and produces a continuing sequence.
  Uses batch labels to decide the new injection sequence.
}

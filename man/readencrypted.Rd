% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{readencrypted}
\alias{readencrypted}
\title{Read encrypted SPSS file}
\usage{
readencrypted(filePath, debug, encStr, ownEnc, pass)
}
\arguments{
\item{filePath}{The full systempath to the dta file you want to import.}

\item{debug}{print debug information}

\item{encStr}{encoding string}

\item{ownEnc}{encoding provided by localeToCharset}

\item{pass}{passkey required for encoding}
}
\description{
Read encrypted SPSS file
}

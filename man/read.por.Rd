% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readpor.R
\name{read.por}
\alias{read.por}
\title{read.por}
\usage{
read.por(
  file,
  convert.factors = TRUE,
  generate.factors = TRUE,
  encoding = TRUE,
  fromEncoding = NULL,
  use.missings = TRUE,
  debug = FALSE,
  override = FALSE,
  convert.dates = TRUE,
  add.rownames = FALSE
)
}
\arguments{
\item{file}{\emph{string} a por-file to import. can be a file on a computer
or an url. in this case the file will be downloaded and read before it is
used.}

\item{convert.factors}{\emph{logical} if true numeric or character variables
will be converted into a factor in R.}

\item{generate.factors}{\emph{logical} function to convert variables with
partial labels into factors. e.g. 1 - low and 5 - high are provided, labels
2, 3 and 4 will be created. especially useful in combination with
\code{use.missings=TRUE}.}

\item{encoding}{\emph{logical} shall values be converted? If true, \code{read.por()}
will try the charcode stored inside the por-file. If this value is 2 or not
available, \code{fromEncoding} can be used to change encoding.}

\item{fromEncoding}{\emph{character} encoding of the imported file. This
information is stored inside the por-file, but is currently unused. Still
this option can be used to define the initial encoding by hand.}

\item{use.missings}{\emph{logical} should missing values be converted.
Defaults to TRUE.}

\item{debug}{\emph{logical} provides additional debug information. Most
likely not useful to any user.}

\item{override}{\emph{logical} The filename provided in \code{file} is
checked for the ending por. If the file ending is different, nothing is read.
This option can be used to override this behavior.}

\item{convert.dates}{\emph{logical} Should dates be converted on the fly?}

\item{add.rownames}{\emph{logical} If \code{TRUE}, the first column will be
used as rownames. Variable will be dropped afterwards.}
}
\description{
Function to read a SPSS por file into a data.frame().
}
\details{
SPSS files are widely available, though for R long time only foreign
and memisc provided functions to import por-files. Lately haven joined.
This package is an approach to offer another alternative, to document the
por-format and provide additional options to import the data.
}
\note{
Information to decrypt the por-format was provided by tda
\href{http://www.stat.ruhr-uni-bochum.de/tda.html}{www.stat.rub.de/tda.html} and
pspp \href{http://www.gnu.org/software/pspp/}{www.gnu.org/software/pspp/}
}
\seealso{
\code{\link[foreign]{read.spss}}, \code{memisc}.
}

#
# Copyright (C) 2015 Jan Marvin Garbuszus
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.


#' read.sav
#'
#' Function to read a SPSS sav file into a data.frame().
#' @param file \emph{string} a sav-file to import. can be a file on a computer
#' or an url. in this case the file will be downloaded and read befor it is
#' used.
#' @param convert.factors \emph{logical} if true numeric variables will be converted
#' into a factor in R. SPSS can have labeld strings, currently this is not im-
#' plemented.
#' @param generate.factors \emph{logical} function to convert variables with only
#' partial labels in to factors. e.g. 1 - low and 5 - high are provided, labels
#' 2, 3 and 4 will be created. especially useful in combination with
#' \code{use.missings=TRUE}.
#' @param encoding \emph{string} locale to convert to.
#' @param fromEncoding \emph{character.} encoding of the imported file. this
#' information is stored inside the sav-file, information is currently unused.
#'
#'@details SPSS files are widely available, though for R long time only foreign
#'and memisc provided functions to import sav-files. currently haven joined in.
#' this package is an approach to join in, document the sav-format and provide
#'additional options to import the data.
#'
#'@return \code{readspss} returns a data.frame with additional objects
#'\describe{
#'\item{names}{colnames}
#'\item{row.names}{rownames}
#'\item{class}{data.frame}
#'\item{val.label}{value labels}
#'\item{datalabel}{datalabel}
#'\item{datestamp}{datestamp}
#'\item{timestamp}{timestamp}
#'\item{varmatrix}{a matrix with information how the data is stored}
#'\item{labnames}{list with the information which variable uses which label}
#'\item{missings}{a list containg information about the missing variables. if
#'\code{use.missings=TRUE} this Information will be used to generate missings.}
#'\item{vartype}{informations about a variable. is it numeric or string.}
#'}
#'
#'@note information to decrypt the sav-format was provided by tda
#' \url{http://www.stat.ruhr-uni-bochum.de/tda.html} and pspp
#'  \url{http://www.gnu.org/software/pspp/}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
#'
#'@seealso \code{\link{foreign::read.sav}}, \code{memisc} and
#'\code{\link{haven::read_sav}}.

#' @useDynLib readspss
#' @importFrom tools file_ext
#' @export
read.sav <- function(file, convert.factors = TRUE, generate.factors = TRUE,
                     encoding = NULL, fromEncoding = NULL, use.missings =
                       FALSE, debug = FALSE, override = FALSE) {

  # Check if path is a url
  if (length(grep("^(http|ftp|https)://", file))) {
    tmp <- tempfile()
    download.file(file, tmp, quiet = TRUE, mode = "wb")
    filepath <- tmp
    on.exit(unlink(filepath))
  } else {
    # construct filepath and read file
    filepath <- get.filepath(file)
  }
  if (!file.exists(filepath))
    return(message("File not found."))

  file <- file_ext(basename(filepath))

  if ((file != "sav" & file != "SAV") & !isTRUE(override) ){
    warning ("Filending is not sav.
             Use Override if this check should be ignored.")
    return( NULL )
  }

  data <- spss(filePath = filepath, debug)

  names(data) <- trimws(names(data), "right")

  label <- attr(data, "label")
  val.labels <- attr(data, "vallabels")
  vartypes <- attr(data, "vartype")

  # print(label)

  attribs <- attributes(data)

  # convert NAs by missing information provided by SPSS.
  # these are just different missing values in Stata and NA in R.
  if (use.missings) {
    mvtab <- attr(data, "missings")
    varmat <- attr(data, "unkmat")
    missinfo <- varmat[,3]
    missinfo <- which(missinfo %in% missinfo[missinfo != 0])


    for (i in seq_along(mvtab)) {
      mvtabi <- mvtab[[i]]
      missinf <- missinfo[i]

      if (mvtabi[1] == 1) {
        naval <- mvtabi[2]
        # print(naval)

        data[missinf][data[missinf] == naval] <- NA
      }

      if (mvtabi[1] == 2) {
        naval1 <- mvtabi[2]
        naval2 <- mvtabi[3]
        # print(naval)

        data[missinf][data[missinf] == naval1 |
                        data[missinf] == naval2] <- NA
      }

      if (mvtabi[1] == 3) {
        naval1 <- mvtabi[2]
        naval2 <- mvtabi[3]
        naval3 <- mvtabi[4]
        # print(naval)

        data[missinf][data[missinf] == naval1 |
                        data[missinf] == naval2 | data[missinf] == naval3] <- NA
      }


      if (mvtabi[1] == -2) {
        # range

        minval <- mvtabi[2]
        maxval <- mvtabi[3]

        data[missinf][data[missinf] >= minval &
                        data[missinf] <= maxval] <- NA

      }

      if (mvtabi[1] == -3) {
        # range + descrete

        minval <- mvtabi[2]
        maxval <- mvtabi[3]
        naval  <- mvtabi[4]

        data[missinf][(data[missinf] >= minval &
                         data[missinf] <= maxval) | data[missinf] == naval] <- NA

      }
    }

  }

  ## Encoding
  if (!is.null(encoding)) {
    # set from encoding
    if (is.null(fromEncoding)) {
      fromEncoding <- "CP1252"
    }

    # varnames
    names(data) <-
      read.encoding(names(data), fromEncoding, encoding)

    # label
    for (i in seq_along(label))
      names(label[i]) <- read.encoding(names(label[[i]]),
                                       fromEncoding = fromEncoding, encoding =
                                         encoding)

    # var.labels
    val.labels <- read.encoding(val.labels, fromEncoding, encoding)
    # print(val.labels)
  }

  labnames <- attr(data, "EoHUnks")

  vallabels <<- val.labels
  label <<- label
  labnames <<- labnames

  if (convert.factors) {
    # vnames <- names(data)
    for (i in seq_along(label)) {
      # print(seq_along(val.labels))
      labname <- unlist(labnames[[i]])
      # print(labname)
      # vartype <- types[i]
      labtable <- unlist(label[[i]])
      # print(vartypes)

      # if(!is.null(labtable))
      #   labtable <- trimws(labtable, "right")

      names(labtable) <- trimws(names(labtable), "right")

      for (j in labname) {
        vartype <- vartypes[j]

        if (vartype == 0) {
          # get unique values / omit NA)
          varunique <- na.omit(unique(data[, j]))
          # print(varunique)

          # assign label if label set is complete
          if (all(varunique %in% labtable)) {
            data[, j] <- factor(data[, j], levels=labtable,
                                labels=labtable)

            # else generate labels from codes
          } else {
            if (generate.factors) {
              names(varunique) <- as.character(varunique)
              gen.lab  <-
                sort(c(varunique[!varunique %in% labtable], labtable))

              data[, j] <- factor(data[, j], levels = gen.lab,
                                  labels = names(gen.lab))
            } else {
              warning(
                paste(
                  vnames[i], "Missing factor labels - no labels assigned.
                  Set option generate.factors=T to generate labels."
                )
                )
            }
            }
          }
        }
      }
    }



  #### ToDo: For additional speed this could/should have been done during the
  #### Rcpp loop.
  # if unkmat > 0 it is a string.
  # trim strings of size < 8

  dfstr <- which(attribs$unkmat[,1] > 0 & attribs$unkmat[,1] < 8)

  # print(dfstr)

  if (any(dfstr)) {
    for (j in 1:length(dfstr)) {
      var <- dfstr[j]
      data[var] <- trimws(c(data[, var]), "right")
    }
  }

  # Identify strings longer than 8. divide the
  # size to see how many cells need to be merged.
  spssstr <- attribs$unkmat[,1]
  longstr <- which(spssstr > 8)
  spssstr <- ceiling(spssstr[longstr] / 8)

  #   print(spssstr)
  #   print(longstr)

  # strings that are longer than 8 bit. works only for
  if (any(longstr)) {
    for (j in length(longstr):1) {
      var <- longstr[j]
      varrange <- var:(var + spssstr[j] - 1)

      paststr <- c(do.call(paste0, data[varrange]))

      if (!is.null(encoding))
        paststr <- read.encoding(paststr, fromEncoding, encoding)

      data[var] <- trimws(paststr, "right")
      data <- data[-varrange[-1]]
    }
    attr(data, "val.label") <- val.labels

  }


  # longstrnam <- names(data[longstr])[1]
  # print(longstr)


  attr(data, "datalabel") <- attribs$datalabel
  attr(data, "datestamp") <- attribs$datestamp
  attr(data, "timestamp") <- attribs$timestamp

  attr(data, "varmatrix") <- attribs$unkmat
  attr(data, "val.label") <- val.labels
  attr(data, "labnames") <- labnames
  attr(data, "missings") <- attribs$missings
  attr(data, "vartype") <- attribs$vartype

  return(data)
  }

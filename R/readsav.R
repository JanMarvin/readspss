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
#' @param convert.factors \emph{logical} if true numeric or character variables
#' will be converted into a factor in R.
#' @param generate.factors \emph{logical} function to convert variables with
#' partial labels into factors. e.g. 1 - low and 5 - high are provided, labels
#' 2, 3 and 4 will be created. especially useful in combination with
#' \code{use.missings=TRUE}.
#' @param encoding \emph{string} locale to convert to.
#' @param fromEncoding \emph{character.} encoding of the imported file. This
#' information is stored inside the sav-file, but is currently unused. Still
#' this option can be used to define the inital encoding by hand.
#' @param use.missings \emph{logical} should missing values be converted.
#' Defaults to TRUE.
#' @param debug \emph{logical} provides additional debug information. Most
#' likely not usefull to any user.
#' @param override \emph{logical}. The filename provided in \code{file} is
#' checked for the ending sav. If the fileending is different, nothing is read.
#' This option can be used to override this behavior.
#'
#' @details SPSS files are widely available, though for R long time only foreign
#' and memisc provided functions to import sav-files. Lately haven joined.
#' This package is an approach to offer another alternative, to document the
#' sav-format and provide additional options to import the data.
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
#'\item{labnames}{list containing the information which variable uses which
#' label}
#'\item{missings}{a list containg information about the missing variables. if
#'\code{use.missings=TRUE} this Information will be used to generate missings.}
#'\item{vartype}{informations about a variable. is it numeric or string.}
#'}
#'
#'@note information to decrypt the sav-format was provided by tda
#' \url{http://www.stat.ruhr-uni-bochum.de/tda.html} and pspp
#'  \url{http://www.gnu.org/software/pspp/}
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#'
#'@seealso \code{\link[foreign]{read.spss}}, \code{memisc} and
#'\code{\link[haven]{read_sav}}.

#' @useDynLib readspss
#' @importFrom magrittr "%>%"
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @importFrom utils download.file
#' @export
read.sav <- function(file, convert.factors = TRUE, generate.factors = TRUE,
                     encoding = NULL, fromEncoding = NULL, use.missings =
                       TRUE, debug = FALSE, override = FALSE) {

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


  # import data using an rcpp routine
  data <- sav(filePath = filepath, debug)

  attribs <- attributes(data)

  label      <- attribs$label
  val.labels <- attribs$vallabels
  vartypes   <- attribs$vartypes
  unkmat     <- do.call("rbind", attribs$unkmat)


  # convert NAs by missing information provided by SPSS.
  # these are just different missing values in Stata and NA in R.
  if (use.missings) {
    mvtab <- attribs$missings
    varmat <- unkmat
    varmat <- varmat[varmat[,1]>=0,]
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
    names(data) <- read.encoding(names(data), fromEncoding, encoding)


    # label
    for (i in seq_along(label))
      names(label[[i]][[1]]) <- read.encoding(names(label[[i]][[1]]),
                                              fromEncoding = fromEncoding,
                                              encoding = encoding)

    # var.labels
    val.labels <- read.encoding(val.labels, fromEncoding, encoding)
    # print(val.labels)
  }

  labnames <- attr(data, "haslabel")
  varnames <- attr(data, "varnames")

  if (convert.factors) {
    # vnames <- names(data)
    for (i in seq_along(label)) {
      # print(seq_along(val.labels))
      labname <- labnames[[i]]
      # vartype <- types[i]
      labtable <- label[[i]]

      for (j in labname) {
        vartype <- vartypes[j]
        varname <- varnames[j]
        isNum   <- is.numeric(data[,varname])
        anyNA   <- any(is.na(labtable))

        # get unique values / omit NA unless NA already in labtable
        if (anyNA) {
          varunique <- unique(data[,varname])
        } else {
          varunique <- na.omit(unique(data[,varname]))
        }

        if (isNum & all(is.na(labtable))) {
          nam<- names(labtable)
          labtable <- as.numeric(labtable)
          names(labtable) <- nam
        }

        # assign label if label set is complete
        if (all(varunique %in% labtable)) {
          data[, varname] <- fast_factor(data[, varname], y=labtable)

          # else generate labels from codes
        } else {
          if (generate.factors) {

            names(varunique) <- as.character(varunique)

            gen.lab  <-
              sort(c(varunique[!varunique %in% labtable], labtable),
                   na.last = TRUE)

            if (isNum) {
              nam <- names(gen.lab)
              gen.lab <- as.numeric(gen.lab)
              names(gen.lab) <- nam
            }

            data[, varname] <- fast_factor(data[, varname], y = gen.lab)
          } else {
            warning(
              paste(
                names(data)[i], "Missing factor labels - no labels assigned.
                Set option generate.factors=T to generate labels."
              )
              )
          }
          }
        }
      }
    }

  # some SPSS files contain long varnames.
  # longvarname is created as empty list
  longvarname <- attr(data, "longvarname")

  if (!identical(longvarname, list())) {

    longname <- longvarname %>% unlist %>%
      strsplit("\t") %>% unlist %>% strsplit("=")


    # If the imported data contains strings longer than nchar(255) the data is
    # scrambled at this point. SPSS separates longer strings in different pieces
    # of size 255. The rcpp import already sorted the data in variables. These
    # variables are now combined. Variable names are split after a few letters
    # used for identification. Since SPSS can use variable names of 8 characters
    # they trim the name down to max of 5. They add three digits identifying the
    # order of the long strings. E.g. "Var1, Var1001, Var1002".
    # Unsure if 999 is the limit.

    nams <- names(data)

    replvec <- lapply(
      longname,
      function(x){
        # grep for identical variable names (not sure if SPSS considers the
        # possiblilty of similar cases. are Value1 and Value2 the same?)
        nams[
          grep(pattern = strtrim(x[[1]], 5), nams)
          ]
      })

    # print(replvec)

    for (i in length(replvec):1) {

      pat <- replvec[[i]]

      # any variables to combine?
      if (length(pat) > 1 & grepl("001", pat[2])) {
        sel <- data[,names(data) %in% pat]

        if (all(sapply(sel, is.character))) {
          pp <- pat[-1]; p1 <- pat[1]

          # remove columns pat[2:n]
          data <- data[,!names(data) %in% pp]

          data[p1] <- do.call(paste0, sel)
        }
      }
    }

    # assign names stored in spss
    # Previously the dataset used some different internal names usefull for
    # combining different long strings. Now everything is cleaned up and we
    # can apply the correct variable names
    nams <- names(data)

    new_nams <- do.call(rbind, longname)

    for (i in 1:NROW(new_nams)) {
      nams[nams == new_nams[i,1] ] <- new_nams[i,2]
    }

    names(data) <- nams

  }


  # prepare for return
  attr(data, "datalabel") <- attribs$datalabel
  attr(data, "datestamp") <- attribs$datestamp
  attr(data, "timestamp") <- attribs$timestamp

  attr(data, "varmatrix") <- unkmat
  attr(data, "val.label") <- val.labels
  attr(data, "labnames")  <- labnames
  attr(data, "missings")  <- attribs$missings
  attr(data, "vartype")   <- attribs$vtype
  attr(data, "lstring")   <- attribs$longstring
  attr(data, "lvarname")  <- attribs$longvarname

  # return
  return(data)

  }

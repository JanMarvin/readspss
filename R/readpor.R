#' read.por
#'
#' Function to read a SPSS por file into a data.frame().
#'@param file _string_ a por-file to import. can be a file on a computer
#' or an url. in this case the file will be downloaded and read before it is
#' used.
#'@param convert.factors _logical_ if true numeric or character variables
#' will be converted into a factor in R.
#'@param generate.factors _logical_ function to convert variables with
#' partial labels into factors. e.g. 1 - low and 5 - high are provided, labels
#' 2, 3 and 4 will be created. especially useful in combination with
#' `use.missings=TRUE`.
#'@param encoding _logical_ shall values be converted? If true, `read.por()`
#' will try the charcode stored inside the por-file. If this value is 2 or not
#' available, `fromEncoding` can be used to change encoding.
#'@param fromEncoding _character_ encoding of the imported file. This
#' information is stored inside the por-file, but is currently unused. Still
#' this option can be used to define the initial encoding by hand.
#'@param use.missings _logical_ should missing values be converted.
#' Defaults to TRUE.
#' @param debug _logical_ provides additional debug information. Most
#' likely not useful to any user.
#'@param override _logical_ The filename provided in `file` is
#' checked for the ending por. If the file ending is different, nothing is read.
#' This option can be used to override this behavior.
#'@param convert.dates _logical_ Should dates be converted on the fly?
#'@param add.rownames _logical_ If `TRUE`, the first column will be
#'  used as rownames. Variable will be dropped afterwards.
#'
#'@details SPSS files are widely available, though for R long time only foreign
#' and memisc provided functions to import por-files. Lately haven joined.
#' This package is an approach to offer another alternative, to document the
#' por-format and provide additional options to import the data.
#'
#'@note Information to decrypt the por-format was provided by tda
#' [www.stat.rub.de/tda.html](http://www.stat.ruhr-uni-bochum.de/tda.html) and
#' pspp [www.gnu.org/software/pspp/](http://www.gnu.org/software/pspp/)
#'
#'@seealso \code{\link[foreign]{read.spss}}, \code{memisc}.
#'
#' @useDynLib readspss, .registration=TRUE
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @importFrom utils download.file localeToCharset
#' @export
read.por <- function(file, convert.factors = TRUE, generate.factors = TRUE,
                     encoding = TRUE, fromEncoding = NULL, use.missings = TRUE,
                     debug = FALSE, override = FALSE, convert.dates = TRUE,
                     add.rownames = FALSE) {


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

  if ((tolower(file) != "por") &&
      !isTRUE(override)) {
    warning("Filending is not por.
             Use Override if this check should be ignored.")
    return(NULL)
  }

  encStr <- ""
  ownEnc <- localeToCharset(locale = Sys.getlocale("LC_CTYPE"))[1]

  if (!is.null(fromEncoding)) {
    encStr <- fromEncoding
  }

  if (encoding == FALSE)
    encStr <- "NA"

  # import data using an rcpp routine
  data <- readpor(filepath, debug, encStr, override)

  attribs <- attributes(data)

  if (NROW(data) == 0) {
    message("file contains no data")
    use.missings <- FALSE
    convert.factors <- FALSE
  }


  labtab    <- attribs$labtab
  label   <- attribs$label
  labnames <- names(labtab)
  varnames <- attribs$names
  vartypes <- attribs$vartypes
  fmt      <- attribs$fmt

  fmt <- do.call(rbind, fmt)
  attr(data, "fmt") <- fmt


  # convert NAs by missing information provided by SPSS.
  # these are just different missing values in Stata and NA in R.
  if (use.missings) {
    if (!identical(attribs$missings, list())) {

      mvtab <- attribs$missings

      for (i in seq_along(mvtab)) {
        mvtabi <- mvtab[[i]]

        missinf <- names(mvtabi)
        naval <- mvtabi[[1]]

        data[missinf][data[missinf] == naval] <- NA

      }
    }

    varrange <- attribs$varrange

    if (!identical(varrange, list())) {

      for (i in seq_along(varrange)) {

        # range
        mvtabi <- varrange[[i]]
        missinf <- names(varrange[i])

        minval <- mvtabi[1]
        maxval <- mvtabi[2]

        data[missinf][data[missinf] >= minval &
                        data[missinf] <= maxval] <- NA

      }

    }

    lothrux <- attribs$lothrux

    if (!identical(lothrux, list())) {
      for (i in seq_along(lothrux)) {

        # range
        mvtabi <- lothrux[[i]]
        missinf <- names(mvtabi)

        maxval <- mvtabi

        data[missinf][data[missinf] <= maxval] <- NA

      }

    }

    xthruhi <- attribs$xthruhi

    if (!identical(xthruhi, list())) {
      for (i in seq_along(xthruhi)) {

        # range
        mvtabi <- xthruhi[[i]]
        missinf <- names(mvtabi)

        maxval <- mvtabi

        data[missinf][data[missinf] >= maxval] <- NA

      }

    }
  }


  # if autoenc labels were not encoded during readsav() so encode now
  if (encoding) {

    # label
    for (i in seq_along(labtab))
      names(labtab[[i]]) <- read.encoding(names(labtab[[i]]),
                                         fromEncoding = encStr,
                                         encoding = ownEnc)

    label <- read.encoding(label,
                           fromEncoding = encStr,
                           encoding = ownEnc)


    for (v in (seq_along(data))[vartypes > 0]) {
      data[, v] <- read.encoding(data[, v],
                                  fromEncoding = encStr,
                                  encoding = ownEnc)
    }
  }


  # FixME: unsure
  if (convert.factors) {
    # vnames <- names(data)
    for (i in seq_along(labtab)) {

      labname <- labnames[[i]]
      labtable <- labtab[[i]]

      for (j in labname) {
        varname <- varnames[which(varnames == j)]
        isNum   <- is.numeric(data[, varname])
        anyNA   <- any(is.na(labtable))

        # get unique values / omit NA unless NA already in labtable
        if (anyNA) {
          varunique <- unique(data[[varname]])
        } else {
          varunique <- na.omit(unique(data[[varname]]))
        }

        if (isNum || all(is.na(labtable))) {
          nam <- names(labtable)
          labtable <- as.numeric(labtable)
          names(labtable) <- nam
        }

        # assign label if label set is complete
        if (all(varunique %in% labtable)) {
          data[[varname]] <- fast_factor(data[[varname]], y = labtable)

          # else generate labels from codes
        } else {
          if (generate.factors) {

            names(varunique) <- as.character(varunique)

            gen.lab  <-
              sort(c(varunique[!varunique %in% labtable], labtable),
                   na.last = TRUE)

            if (isNum) {
              nam <- names(gen.lab)
              gen.lab <- as.integer(gen.lab)
              names(gen.lab) <- nam
            }

            data[[varname]] <- fast_factor(data[[varname]], y = gen.lab)
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




  if (convert.dates) {

    nams   <- names(data)
    isdate <- fmt[, 1] %in% c(20, 23, 24, 28, 29, 30, 38, 39)
    isdatetime <- fmt[, 1] %in% c(22, 41)
    istime <- fmt[, 1] %in% c(21, 25, 40)

    if (any(isdate)) {
      for (nam in nams[isdate]) {
        # the tda function does not always provide integers. in rare cases the
        # date conversion might be off by a day e.g. "13770950400". This is
        # avoided by rounding the value first
        data[[nam]] <- as.Date(as.POSIXct(
          round(data[[nam]]), origin = "1582-10-14"))
      }
    }
    if (any(isdatetime)) {
      for (nam in nams[isdatetime]) {
        data[[nam]] <- as.POSIXct(
          data[[nam]],
          origin = "1582-10-14",
          tz = "GMT")
      }
    }
    if (any(istime)) {
      message(
        "time format found for:\n",
        paste(nams[istime], collapse = "\n"),
        "\ntime variables are not dates and thus not converted."
      )
    }

  }


  attr(data, "labtab") <- labtab
  attr(data, "label") <- label

  if (add.rownames) {
    rownames(data) <- data[[1]]
    data[[1]] <- NULL
  }

  # return
  return(data)

}

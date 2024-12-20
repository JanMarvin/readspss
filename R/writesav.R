resize_vartyp <- function(vec, var = NULL) {

  out <- NULL
  for (i in seq_along(vec)) {

    val <- vec[i]

    if (is.null(var)) {
      if (val <= 8) {
        out <- c(out, val)
      } else {
        out <- c(out, c(val, rep(-1, (ceiling(val / 8) - 1))))
      }
    } else {
      if (val <= 8) {
        out <- c(out, var[i])
      } else {
        out <- c(out, c(var[i], rep(var[i], (ceiling(val / 8) - 1))))
      }
    }

  }

  out
}

#' write.sav
#'
#' Function to write an SPSS sav or zsav file from a data.frame().
#' @param dat _data.frame_ a data.frame to store as SPSS file.
#' @param filepath _string_ full path where and how this file should be
#'  stored
#' @param label _character_ if any provided this must be a vector of
#'  labels. It must be of size `ncol(dat)`
#' @param add.rownames _logical_ If `TRUE`, a new variable rownames
#'  will be added to the sav-file.
#' @param compress _logical_ should compression be used. If TRUE some
#'  integers will be stored more efficiently. Everything will be stored in
#'  chunks of 8 chars. Reduces memory size of sav-file.
#' @param convert.dates _logical_ should dates be converted to SPSS format.
#' @param tz _character_ The name of the timezone convert.dates will use.
#' @param debug _logical_ print debug information.
#' @param is_zsav _logical_ explicitly create a zsav file. If the file
#'  ending zsav is used, this is selected as default.
#' @param disppar optional display parameter matrix. Needs documentation.
#' @details Writing of strings longer than 255 chars is not provided.
#'
#' @return `write.sav` returns nothing
#'
#' @export
write.sav <- function(dat, filepath, label, add.rownames = FALSE,
                      compress = FALSE, convert.dates = TRUE, tz = "GMT",
                      debug = FALSE, is_zsav = FALSE, disppar) {

  filepath <- path.expand(filepath)

  if (missing(filepath))
    stop("need a path")

  attrlab <- attr(dat, "var.label")

  if (identical(attrlab, character(0)))
    attrlab <- NULL


  if (missing(label) && is.null(attrlab))
    label <- ""

  if (missing(label) && !is.null(attrlab))
    label <- attrlab

  if (!identical(label, "") && (length(label) != ncol(dat)))
    stop("label and ncols differ. each col needs a label")

  if (any(nchar(label)) > 255)
    stop("longlabels not yet implemented")

  if (add.rownames) {
    dat <- data.frame(rownames = rownames(dat),
                      dat, stringsAsFactors = FALSE)
  }

  nams <- names(dat)

  # get labtab prior to any modification due to string sizes
  ff <- which(sapply(dat, is.factor))

  labtab <- lapply(ff, function(x) {

    ll <- levels(dat[[x]])

    x <- as.integer(labels(ll))
    names(x) <- ll

    x
  })

  LONGVAR <- FALSE

  if (all(nchar(nams) <= 8) && (identical(toupper(nams), nams))) {
    nams <- toupper(nams)
    nvarnames <- substr(nams, 0, 8)
  } else {
    nvarnames <- paste0("VAR", seq_along(nams))
    LONGVAR <- TRUE
  }

  vtyp <- as.integer(sapply(dat, is.character))
  vtyp[vtyp != 0] <- as.integer(sapply(dat[vtyp != 0],
                                       function(x) max(nchar(x), na.rm = TRUE)))


  if (any(vtyp > 255)) {
    message("if you really need this, split the string into segments of 255")
    stop("Strings longer than 255 characters not yet implemented")
  }

  vtyp <- ceiling(vtyp / 8) * 8

  vtyp[vtyp > 255] <- 255

  vartypes <- resize_vartyp(vtyp)

  vartypes[vartypes > 255] <- 255

  nams <- vector("character", length(vartypes))
  nams[vartypes > -1] <- nvarnames

  nvarnames <- nams

  # update factor position with new varnames
  pos <- which(nvarnames != "")

  if (length(ff) > 0)  {
    ff <- sapply(ff, function(x) {
      # newnam <- nvm[x]
      x <- pos[x]
      # names(x) <- newnam

      x
    })
  }

  longvarnames <- ""
  if ((length(nvarnames) > length(names(dat))) || LONGVAR)
    longvarnames <- paste(
      paste0(nvarnames[nvarnames != ""], "=", names(dat)),
      collapse = "\t")

  systime <- Sys.time()
  timestamp <- substr(systime, 12, 19)
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  datestamp <- format(Sys.Date(), "%d %b %y")
  Sys.setlocale("LC_TIME", lct)


  ii <- sapply(dat, is.integer)
  nn <- sapply(dat, function(x) {
    is.numeric(x) | is.factor(x)
  })
  itc <- rep(0, NCOL(dat))

  # get vartyp used for display parameters. has to be selected prior to
  # compression. otherwise factor will be wrongfully identified as integer.
  vartypen <- sapply(dat, function(x) class(x)[[1]])

  # if compression is selected, try to store numeric, logical and factor as
  # integer and try to compress integer as uint8 (with bias). Since R does
  # only know numeric and integer, this needs additional testing if a
  # conversion is safe.
  if (compress) {
    message("Compression is still experimental. Testing is welcome!")
    # check if numeric can be stored as integer
    numToCompress <- sapply(dat[nn], saveToExport)

    # convert numeric to integer without loss of information
    if (any(numToCompress)) {
      saveToConvert <- names(numToCompress[numToCompress])
      # replace numeric as integer
      dat[saveToConvert] <- sapply(dat[saveToConvert], as.integer)
    }

    # ii integer and not all missing
    ii <- sapply(dat, function(x) {
      (is.logical(x) | is.integer(x))
    })

    gg <- FALSE
    dat_ii <- dat[names(ii)[ii]] # might have length 0
    # gg check for ii if is.integer and min >= 100 and max < 151 (in range of)
    # uint8 +100 bias. Values > 250 are missing.
    if (length(dat_ii) > 0)
      gg <- sapply(dat_ii, function(x) {
        z <- NULL
        # if all values are missing, return TRUE: will write 255 in output
        if (all(is.na(x))) {
          z <- TRUE
        } else {
          # check if value can be stored as uint8 with bias
          z <- (min(x, na.rm = TRUE) >= -100 & max(x, na.rm = TRUE) < 151)
        }
        z
      })

    # adjust gg to the length of dat
    gg <- gg[names(dat)]
    # logical matrix: is integer and good for compression?
    checkll <- rbind(ii, gg)

    # logical for integer compression
    itc <- apply(checkll, 2, all)

  }

  cc <- sapply(dat, is.character)

  vartyp <- NA
  vartyp[vartypen == "factor" | vartypen == "logical"] <- -1
  vartyp[vartypen == "numeric" | vartypen == "integer"] <- 0
  vartyp[vartypen == "character"] <- 1
  vartyp[vartypen == "Date"] <- 20
  vartyp[vartypen == "POSIXct"] <- 22

  if (convert.dates) {
    dates <- which(sapply(dat,
                          function(x) inherits(x, "Date"))
    )
    for (v in dates)
      dat[[v]] <- as.vector(
        julian(dat[[v]], as.Date("1582-10-14", tz = "GMT")) * 24 * 60 * 60
      )
    dates <- which(
      sapply(dat, function(x) inherits(x, "POSIXt"))
    )
    for (v in dates)
      dat[[v]] <- as.vector(
        round(julian(dat[[v]], ISOdate(1582, 10, 14, tz = tz))) * 24 * 60 * 60
      )
  }

  # optional disppar parameter. if none is passed to the function, create a
  # default one with a few selected parameters.
  # TODO: add a similar logic for varmatrix
  if (missing(disppar)) {

    measure <- rep(NA, ncol(dat))
    # nominal if factor, logical or character; else metric
    # (nominal 1, ordinal 2, metric 3)
    sel <- vartyp == -1 | vartyp == 1
    measure[sel] <- 1
    measure[!sel] <- 3

    colwidth <- rep(NA, ncol(dat))
    # colwidth 10 if date; else 8
    sel <- vartyp == 20 | vartyp == 22
    colwidth[sel] <- 10
    colwidth[!sel] <- 8

    alignment <- rep(NA, ncol(dat))
    # characters left aligned; else right
    # (1 right, 2 center, 3 left)
    sel <- vartyp == 1
    alignment[sel] <- 3
    alignment[!sel] <- 1

    # create disppar matrix
    disppar <- matrix(c(measure, colwidth, alignment),
                      ncol = 3)
  }

  # make it flat
  disppar <- c(t(disppar))

  # resize vartyp for long strings
  if (length(vartyp) != length(vartypes)) {
    vartyp <- resize_vartyp(vtyp, vartyp)
  }

  if (length(label) != length(vartypes)) {
    label <- resize_vartyp(vtyp, label)
  }

  attr(dat, "vtyp") <- vtyp
  attr(dat, "vartyp") <- vartyp
  attr(dat, "vartypes") <- vartypes
  attr(dat, "nvarnames") <- nvarnames
  attr(dat, "longvarnames") <- longvarnames
  attr(dat, "timestamp") <- timestamp
  attr(dat, "datestamp") <- datestamp
  attr(dat, "label") <- label
  attr(dat, "haslabel") <- ff
  attr(dat, "labtab") <- labtab
  attr(dat, "itc") <- itc
  attr(dat, "cc") <- cc
  attr(dat, "disppar") <- disppar

  if (file_ext(filepath) == "zsav")
    is_zsav <- TRUE

  if (is_zsav)
    message("Zsav compression is still experimental. Testing is welcome!")

  writesav(filepath, dat, compress, debug, is_zsav)
}

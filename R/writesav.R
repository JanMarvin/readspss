#
# Copyright (C) 2018-2019 Jan Marvin Garbuszus
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


#' write.sav
#'
#' Function to write an SPSS sav file from a data.frame().
#' @param dat \emph{data.frame} a dat.aframe to strore as sav-file.
#' @param filepath \emph{string} full path where and how this file should be
#'  stored
#' @param label \emph{character} if any provided this must be a vector of
#'  labels. It must be of size `ncol(dat)`
#' @param add.rownames \emph{logical.} If \code{TRUE}, a new variable rownames
#'  will be added to the por-file.
#' @param compress \emph{logical} should compression be used. If TRUE some
#'  integers will be stored more efficiently. Everything will be stored in
#'  chunks of 8 chars. Reduces memory size of sav-file.
#' @param convert.dates \emph{logical} should dates be converted to SPSS format
#' @param tz \emph{character.} The name of the timezone convert.dates will use.
#' @param debug \emph{logical} print debug information.
#' @param is_zsav \emph{logical} create zsav file.
#' @details Strings longer than 255 chars are not provided.
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.sav <- function(dat, filepath, label, add.rownames = FALSE,
                      compress = FALSE, convert.dates = TRUE, tz="GMT",
                      debug = FALSE, is_zsav = FALSE) {

  filepath <- path.expand(filepath)

  if (missing(filepath))
    stop("need a path")

  attrlab <- attr(dat, "var.label")

  if (identical(attrlab, character(0)))
    attrlab <- NULL


  if (missing(label) & is.null(attrlab))
    label <- ""

  if (missing(label) & !is.null(attrlab))
    label <- attrlab

  if ( !identical(label, "") & (length(label) != ncol(dat)) )
    stop("label and ncols differ. each col needs a label")

  if (any(nchar(label))>255)
    stop("longlabels not yet implemented")

  if (add.rownames) {
    dat <- data.frame(rownames= rownames(dat),
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

  if (all(nchar(nams)<=8) & (identical(toupper(nams), nams))) {
    nams <- toupper(nams)
    nvarnames <- substr(nams, 0, 8)
  } else {
    nvarnames <- paste0("VAR", seq_along(nams))
    LONGVAR <- TRUE
  }

  vtyp <- as.integer(sapply(dat, is.character))
  vtyp[vtyp != 0] <- as.integer(sapply(dat[vtyp!=0],
                                       function(x) max(nchar(x), na.rm = TRUE)))


  if (any(vtyp>255)) {
    message("if you really need this, split the string into segments of 255")
    stop("Strings longer than 255 characters not yet implemented")
  }

  vtyp <- ceiling(vtyp/8) * 8;

  vtyp[vtyp > 255] <- 255

  fun <- function(vec) {

    vartypes <- NULL
    for (i in seq_along(vec)) {

      val <- vtyp[i]

      if (val <= 8) {
        vartypes <- c(vartypes, val)
      } else {
        vartypes <- c(vartypes, c(val, rep(-1, (ceiling(val/8) -1) ) ) )
      }
    }

    vartypes

  }

  vartypes <- fun(vtyp)

  vartypes[vartypes > 255] <- 255

  nams <- vector("character", length(vartypes))
  nams[vartypes > -1] <- nvarnames

  nvarnames <- nams

  # update factor position with new varnames
  nvm <- nvarnames[nvarnames!=""]
  pos <- which(nvarnames != "")

  if (length(ff)>0)  {
    ff <- sapply(ff, function(x){
      # newnam <- nvm[x]
      x <- pos[x]
      # names(x) <- newnam

      x
    })
  }

  longvarnames <- ""
  if ((length(nvarnames) > length(names(dat))) | LONGVAR)
    longvarnames <- paste(
      paste0(nvarnames[nvarnames!=""], "=", names(dat)),
      collapse = "\t")

  systime <- Sys.time()
  timestamp <- substr(systime, 12, 19)
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  datestamp <- format(Sys.Date(), "%d %b %y")
  Sys.setlocale("LC_TIME", lct)


  ii <- sapply(dat, is.integer)
  nn <- sapply(dat, function(x){is.numeric(x) | is.factor(x)})
  itc <- rep(0, NCOL(dat))

  if (compress) {
    message("Compression is still experimental. Testing is welcome!")
    # check if numerics can be stored as integers
    numToCompress <- sapply(dat[nn], saveToExport)

    if (any(numToCompress)) {
      saveToConvert <- names(numToCompress[numToCompress])
      # replace numerics as intergers
      dat[saveToConvert] <- sapply(dat[saveToConvert], as.integer)
    }

    ii <- sapply(dat, function(x) { !(is.integer(x) & all(is.na(x))) })
    gg <- sapply(dat[ii], function(x) {
        is.integer(x) & (min(x, na.rm = TRUE) >= -100 &
                           max(x, na.rm = TRUE) < 151)
    })
    gg <- gg[names(ii)]

    checkll <- rbind(ii, gg)

    itc <- as.logical(checkll)
    if (length(gg) > 0)
      itc <- apply(checkll, 2, all)

  }

  cc <- sapply(dat, is.character)


  vartypen <- sapply(dat, function(x)class(x)[[1]])
  vartyp <- NA
  vartyp[vartypen == "numeric" | vartypen == "integer" |
           vartypen == "factor"] <- 0
  vartyp[vartypen == "character"] <- 1
  vartyp[vartypen == "Date"] <- 20
  vartyp[vartypen == "POSIXct"] <- 22

  if (convert.dates) {
    dates <- which(sapply(dat,
                          function(x) inherits(x, "Date"))
    )
    for (v in dates)
      dat[[v]] <- as.vector(
        julian(dat[[v]],as.Date("1582-10-14", tz = "GMT"))*24*60*60
      )
    dates <- which(
      sapply(dat, function(x) inherits(x,"POSIXt"))
    )
    for (v in dates)
      dat[[v]] <- as.vector(
        round(julian(dat[[v]], ISOdate(1582, 10, 14, tz = tz)))*24*60*60
      )
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

  # if (compress == 1)
  #   dat <<- dat

  if (file_ext(filepath) == "zsav")
    is_zsav <- TRUE

  if (is_zsav)
    message("Zsav compression is still experimental. Testing is welcome!")

  writesav(filepath, dat, compress, debug, is_zsav)
}


# write.sav(cars)

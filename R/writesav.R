#
# Copyright (C) 2018 Jan Marvin Garbuszus
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
#' @param dat \emph{data.frame} a dat.aframe to strore as sav-file.
#' @param filepath \emph{string} full path where and how this file should be
#'  stored
#' @param label \emph{character} vector of labels. must be of size `ncol(dat)`
#' @param compress \emph{logical} should compression be used
#' @details For now stores data.frames containing numerics only. Nothing else
#'  aside varnames and numerics are stored.
#'  Missing values in character cols (<NA>) are written as empty ("").
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.sav <- function(dat, filepath, label, compress = FALSE) {

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

  fun <- function(vec) {

    vartypes <- NULL
    for (i in seq_along(vec)) {

      val <- vtyp[i]

      if (val <= 8) {
        vartypes <- c(vartypes, val)
      } else {
        vartypes <- c(vartypes, c(val, rep(-1, (val/8 - 1)) ) )
      }
    }

    vartypes

  }

  vartypes <- fun(vtyp)

  nams <- vector("character", length(vartypes))
  nams[vartypes > -1] <- nvarnames

  nvarnames <- nams

  # update factor position with new varnames
  nvm <- nvarnames[nvarnames!=""]
  pos <- which(nvarnames != "")

  ff <- sapply(ff, function(x){
    # newnam <- nvm[x]
    x <- pos[x]
    # names(x) <- newnam

    x
  })

  longvarnames <- ""
  if ((length(nvarnames) > length(names(dat))) | LONGVAR)
    longvarnames <- paste(
      paste0(nvarnames[nvarnames!=""], "=", names(dat)),
      collapse = "\t")

  systime <- Sys.time()
  timestamp <- substr(systime, 12, 19)
  datestamp <- format(Sys.Date(), "%d %b %y")


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
  # ii <<- ii
  # gg <<- gg
  #
  # dat <<- dat
  # itc <<- itc
  # vartypes <<- vartypes
  # vtyp <<- vtyp
  # cc <<- cc

  # isnum <- sapply(dat, function(x){is.numeric(x) & !is.integer(x)})

  # vartypes[which(isnum == TRUE)] <- 253


  attr(dat, "vtyp") <- vtyp
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

  writesav(filepath, dat, compress)
}


# write.sav(cars)

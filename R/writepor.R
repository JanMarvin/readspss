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


#' write.por
#'
#' !!! EXPERIMENTAL !!! Function to write an SPSS por file. Returns an por file
#' that read.por can read as well as SPSS can. Other packages as foreign,
#' memisc and haven might fail (fail reading or return wrong values).
#' @param dat \emph{data.frame} a data.frame to export as por-file.
#' @param filepath \emph{string} full path where and how this file should be
#'  stored
#' @param label \emph{character} vector of labels. must be of size `ncol(dat)`
#' @details For now stores data.frames containing numerics only. Nothing else
#'  aside varnames and numerics are stored.
#'  Missing values in character cols (<NA>) are written as empty ("").
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.por <- function(dat, filepath, label) {

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

  LONGVAR <- FALSE

  # if (all(nchar(nams)<=8) & (identical(toupper(nams), nams))) {
    nams <- toupper(nams)
    nvarnames <- substr(nams, 0, 8)
  # } else {
  #   nvarnames <- paste0("VAR", seq_along(nams))
  #   LONGVAR <- TRUE
  # }

  vtyp <- as.integer(sapply(dat, is.character))
  vtyp[vtyp != 0] <- as.integer(sapply(dat[vtyp!=0],
                                       function(x) max(nchar(x), na.rm = TRUE)))

  ff <- which(sapply(dat, is.factor))

  labtab <- lapply(ff, function(x) {

    ll <- levels(dat[[x]])

    x <- as.integer(labels(ll))
    names(x) <- ll

    x
  })

  if (any(vtyp>255)) {
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

  # nams <- vector("character", length(vartypes))
  # nams[vartypes > -1] <- nvarnames

  nvarnames <- nams

  longvarnames <- ""
  if ((length(nvarnames) > length(names(dat))) | LONGVAR)
    longvarnames <- paste(
      paste0(nvarnames[nvarnames!=""], "=", names(dat)),
      collapse = "\t")

  systime <- Sys.time()
  timestamp <- gsub(pattern = ":", replacement = "",
                    x = substr(systime, 12, 19))
  datestamp <- format(Sys.Date(), "%Y%m%d")


  ii <- sapply(dat, is.integer)
  nn <- sapply(dat, function(x){is.numeric(x) | is.factor(x)})
  itc <- rep(0, NCOL(dat))

  cc <- sapply(dat, is.character)
  # ii <<- ii
  # gg <<- gg
  #
  # dat <<- dat
  # itc <<- itc
  # vartypes <<- vartypes
  # vtyp <<- vtyp
  # cc <<- cc

  isint <- sapply(dat, function(x){is.numeric(x) & is.integer(x)})

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
  attr(dat, "isint") <- isint


  dat <<- dat

  writepor(filepath, dat)
}


# write.sav(cars)

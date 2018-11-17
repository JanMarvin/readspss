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
#' @param add.rownames \emph{logical.} If \code{TRUE}, a new variable rownames
#'  will be added to the por-file.
#' @param convert.factors \emph{logical.} If \code{TRUE}, factors will be
#'  converted to SPSS variables with labels.
#'  SPSS expects strings to be encoded as Windows-1252, so all levels will be
#'  recoded.  Character which can not be mapped in Windows-1252 will be saved as
#'  hexcode.
#' @details For now stores data.frames containing numerics only. Nothing else
#'  aside varnames and numerics are stored.
#'  Missing values in character cols (<NA>) are written as empty ("").
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.por <- function(dat, filepath, label, add.rownames = FALSE,
                      convert.factors = TRUE) {

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


  toEncoding <- "CP1252"

  if (add.rownames) {
    rwn <- save.encoding(rownames(dat), toEncoding)

    dat <- data.frame(rownames= rwn,
                      dat, stringsAsFactors = FALSE)
  }

  nams <- names(dat)

  LONGVAR <- FALSE

  # if (all(nchar(nams)<=8) & (identical(toupper(nams), nams))) {
  nams <- toupper(nams)
  nvarnames <- substr(nams, 0, 8)
  names(dat) <- nvarnames

  # } else {
  #   nvarnames <- paste0("VAR", seq_along(nams))
  #   LONGVAR <- TRUE
  # }

  if (convert.factors) {
    # If our data.frame contains factors, we create a label.table
    factors <- which(sapply(dat, is.factor))
    f.names <- attr(factors,"names")

    label.table <- vector("list", length(f.names))
    names(label.table) <- f.names

    i <- 0
    for (v in factors)  {
      i <- i + 1
      f.levels <- save.encoding(levels(dat[[v]]), toEncoding)
      f.labels <-  as.integer(labels(levels(dat[[v]])))
      attr(f.labels, "names") <- f.levels
      f.labels <- f.labels[names(f.labels) != ".."]
      label.table[[ (f.names[i]) ]] <- f.labels
    }
    attr(dat, "labtab") <- rev(label.table)
  } else {
    attr(dat, "labtab") <- NULL
  }

  vtyp <- as.integer(sapply(dat, is.character))
  vtyp[vtyp != 0] <- as.integer(sapply(dat[vtyp!=0],
                                       function(x) max(nchar(x), na.rm = TRUE)))

  ff <- which(sapply(dat, is.factor))

  # labtab <- lapply(ff, function(x) {
  #
  #   ll <- levels(dat[[x]])
  #
  #   x <- as.integer(labels(ll))
  #   names(x) <- ll
  #
  #   x
  # })

  if (identical(unname(ff), integer(0)))
    ff <- unname(ff)

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

  systime <- Sys.time()
  timestamp <- gsub(pattern = ":", replacement = "",
                    x = substr(systime, 12, 19))
  datestamp <- format(Sys.Date(), "%Y%m%d")


  ii <- sapply(dat, is.integer)
  nn <- sapply(dat, function(x){is.numeric(x) | is.factor(x)})
  itc <- rep(0, NCOL(dat))

  cc <- sapply(dat, is.character)

  isint <- sapply(dat, function(x){is.numeric(x) & is.integer(x)})

  # vartypes[which(isnum == TRUE)] <- 253


  attr(dat, "vtyp") <- vtyp
  attr(dat, "vartypes") <- vartypes
  attr(dat, "nvarnames") <- nvarnames
  attr(dat, "timestamp") <- timestamp
  attr(dat, "datestamp") <- datestamp
  attr(dat, "label") <- label
  attr(dat, "haslabel") <- ff
  # attr(dat, "labtab") <- labtab
  attr(dat, "itc") <- itc
  attr(dat, "cc") <- cc
  attr(dat, "isint") <- isint

  dat <<- dat

  writepor(filepath, dat)
}


# write.sav(cars)

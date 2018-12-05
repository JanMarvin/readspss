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
#' @param toEncoding encoding used for the por file. SPSS itself claims to
#'  have problems with unicode and por files, so "CP1252" is the default.
#' @param convert.dates \emph{logical} should dates be converted to SPSS format
#' @param tz \emph{character.} The name of the timezone convert.dates will use.
#' @details Strings longer than 255 chars are not provided.
#'  File will be stored using "CP1252" encoding.
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.por <- function(dat, filepath, label, add.rownames = FALSE,
                      convert.factors = TRUE, toEncoding = "CP1252",
                      convert.dates = TRUE, tz="GMT") {

  filepath <- path.expand(filepath)

  if (missing(filepath))
    stop("need a path")

  attrlab <- attr(dat, "label")

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

  nams <- toupper(nams)
  nvarnames <- substr(nams, 0, 8)
  names(dat) <- nvarnames


  if (convert.factors) {
    # If our data.frame contains factors, we create a label.table
    factors <- which(sapply(dat, is.factor))
    f.names <- attr(factors,"names")

    label.table <- vector("list", length(f.names))
    names(label.table) <- f.names

    i <- 0
    for (v in factors)  {
      i <- i + 1
      f.levels <- levels(dat[[v]])
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

  if (identical(unname(ff), integer(0)))
    ff <- unname(ff)

  if (any(vtyp>255)) {
    stop("Strings longer than 255 characters not yet implemented")
  }

  vtyp <- ceiling(vtyp/8) * 8

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
  attr(dat, "timestamp") <- timestamp
  attr(dat, "datestamp") <- datestamp
  attr(dat, "label") <- label
  attr(dat, "haslabel") <- ff
  attr(dat, "itc") <- itc
  attr(dat, "cc") <- cc
  attr(dat, "isint") <- isint
  attr(dat, "toEncoding") <- toEncoding

  writepor(filepath, dat)
}

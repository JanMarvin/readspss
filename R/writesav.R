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
#' @details For now stores data.frames containing numerics only. Nothing else
#'  aside varnames and numerics are stored.
#'
#' @return \code{readspss} returns nothing
#'
#' @export
write.sav <- function(dat, filepath) {

  if (missing(filepath))
    stop("need a path")

  nams <- names(dat)
  nams <- toupper(nams)
  nvarnames <- substr(nams, 0, 8)

  vtyp <- as.integer(sapply(dat, is.character))

  systime <- Sys.time()
  timestamp <- substr(systime, 12, 19)
  datestamp <- format(Sys.Date(), "%d %b %y")

  attr(dat, "vtyp") <- vtyp
  attr(dat, "nvarnames") <- nvarnames
  attr(dat, "timestamp") <- timestamp
  attr(dat, "datestamp") <- datestamp

  writesav(filepath, dat)
}


# write.sav(cars)

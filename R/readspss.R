#' read.spss
#'
#' Function to read a SPSS (z)sav or por file into a data.frame(). This is just
#' a wrapper around read.sav and read.por for convenience.
#' @param x file to import
#' @param ... additional arguments passed to read.sav or read.por please see the
#'  documentation for these functions.
#' @useDynLib readspss, .registration=TRUE
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @importFrom utils download.file localeToCharset
#' @seealso [read.sav] and [read.por]
#' @export
read.spss <- function(x, ...) {

  if (!file.exists(x))
    return(message("File not found."))

  file <- file_ext(basename(x))

  if (tolower(file) == "sav" | tolower(file) == "zsav") {
    res <- read.sav(x, ...)
  } else if (tolower(file) == "por") {
    res <- read.por(x, ...)
  } else {
    message(c("Sorry file extension is either not detected or not known sav.",
              "You could try read.sav or read.por with override option."))
  }

  res

}

#' read.sav
#'
#' @useDynLib readspss, .registration=TRUE
#' @importFrom tools file_ext
#' @importFrom stats na.omit
#' @importFrom utils download.file localeToCharset
#' @export
read.por <- function(file, convert.factors = TRUE, generate.factors = TRUE,
                     encoding = TRUE, fromEncoding = NULL, use.missings = TRUE,
                     debug = FALSE, override = FALSE, convert.dates = TRUE) {


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

  if ((file != "por" & file != "POR") &
      !isTRUE(override) ){
    warning ("Filending is not por.
             Use Override if this check should be ignored.")
    return( NULL )
  }

  encStr <- ""
  ownEnc <- localeToCharset(locale = Sys.getlocale("LC_CTYPE"))[1]
  forceEncoding <- FALSE

  if (!is.null(fromEncoding)) {
    encStr <- fromEncoding
    forceEncoding <- TRUE
  }

  if (encoding == FALSE)
    encStr <- "NA"

  # import data using an rcpp routine
  data <- readpor(filepath, debug, encStr)

  attribs <- attributes(data)



  label    <- attribs$labtab
  labnames <- names(label)
  varnames <- attribs$names
  vartypes <- attribs$vartypes

  # FixME: unsure
  if (convert.factors) {
    # vnames <- names(data)
    for (i in seq_along(label)) {

      labname <- labnames[[i]]
      # vartype <- types[i]
      labtable <- label[[i]]

      for (j in labname) {
        vartype <- vartypes[which(varnames == j)]
        varname <- varnames[which(varnames == j)]
        isNum   <- is.numeric(data[,varname])
        anyNA   <- any(is.na(labtable))

        # get unique values / omit NA unless NA already in labtable
        if (anyNA) {
          varunique <- unique(data[[varname]])
        } else {
          varunique <- na.omit(unique(data[[varname]]))
        }

        if (isNum | all(is.na(labtable))) {
          nam <- names(labtable)
          labtable <- strtoi(labtable, 30)
          names(labtable) <- nam
        }

        # assign label if label set is complete
        if (all(varunique %in% labtable)) {
          data[[varname]] <- fast_factor(data[[varname]], y=labtable)

          # else generate labels from codes
        } else {
          if (generate.factors) {

            names(varunique) <- as.character(varunique)

            gen.lab  <-
              sort(c(varunique[!varunique %in% labtable], labtable),
                   na.last = TRUE)

            if (isNum) {
              nam <- names(gen.lab)
              gen.lab <- strtoi(gen.lab, 30)
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


  # if (convert.dates) {
  #
  #   nams   <- names(data)
  #   isdate <- varmat[,6] %in% c(20,23,24,38,39)
  #   istime <- varmat[,6] %in% c(21,22,25)
  #
  #   if (any(isdate)) {
  #     for (nam in nams[isdate]) {
  #       data[[nam]] <- as.Date(as.POSIXct(data[[nam]], origin="1582-10-14"))
  #     }
  #   }
  #   if (any(istime)) {
  #     message("time format found for", nams[istime],
  #             "This is a 24 time and no date and thus not converted.")
  #     #   for (nam in nams[istime]) {
  #     #     data[[nam]] <- as.POSIXlt(data[[nam]], origin="1582-10-14")
  #     #   }
  #   }
  #
  # }


  # return
  return(data)

}

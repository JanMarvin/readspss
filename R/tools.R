# Wrapper Around iconv Calls for Code Readability
#
# @param x element to be converted
# @param encoding encoding to be used.
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
read.encoding <- function(x, fromEncoding, encoding) {
  iconv(x,
        from=fromEncoding,
        to=encoding ,
        sub="byte")
}

save.encoding <- function(x, encoding) {
  iconv(x,
        to=encoding,
        sub="byte")
}
# Construct File Path
#
# @param path path to dta file
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
get.filepath <- function(path=""){
  if(substring(path, 1, 1) == "~") {
    filepath <- path.expand(path)
  } else {
    filepath <- path
  }
  if(!file.exists(filepath)) {
    return("File does not exist.")
  }

  return(filepath)
}

#' Check if numeric vector can be expressed as integer vector
#'
#' Compression can reduce numeric vectors as integers if the vector does only
#' contain integer type data.
#'
#' @param x vector of data frame
# @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
# @author Sebastian Jeworutzki \email{sebastian.jeworutzki@@ruhr-uni-bochum.de}
saveToExport <- function(x) {
  isTRUE(all.equal(x, as.integer(x)))
}

library(rebus)
library(gtools) # install.packages('gtools')

# Function to covert marker ID formats
#' @param marker character value of the marker ID
#' ex. "II:1234567" or "2:1234567"
#' @param to_marker selection of "RM" or "NUM" if the input is a "RM"
#' the marker will be converted to roman numeral format (e.g "II:1234567")
#' @param from_marker starting selection input definition
#' of "RM" or "NUM" if the input is a "NUM"
#' @return character value of the marker ID in the desired format
convert_marker <- function(marker, to_marker = "RM", from_marker = "NUM") {
  marker <- as.character(marker)

  if (from_marker == "RM" && to_marker == "NUM") {
    # convert from roman numeral to numeric
    if (grepl(":", marker)) {
      chr <- gtools::roman2int(strsplit(marker, ":")[[1]][1])
      pos <- as.numeric(strsplit(marker, ":")[[1]][2])
      return(paste0(chr, ":", pos))
    } else {
      stop("Invalid marker format for from_marker 'RM'")
    }
  } else if (from_marker == "NUM" && to_marker == "RM") {
    # convert from numeric to roman numeral
    if (grepl(":", marker)) {
      chr <- strsplit(marker, ":")[[1]][1]
      pos <- as.numeric(strsplit(marker, ":")[[1]][2])

      # key to convert to roman numerals
      roman_key <- c(
        "1" = "I",
        "2" = "II",
        "3" = "III",
        "4" = "IV",
        "5" = "V",
        "6" = "X"
      )

      return(paste0(roman_key[chr], ":", pos))
    } else {
      stop("Invalid marker format for from_marker 'NUM'")
    }
  } else if (from_marker == to_marker) {
    # no conversion needed
    return(marker)
  } else {
    stop("Invalid combination of from_marker and to_marker")
  }
}

# test the function
convert_marker("II:1234567", "NUM", "RM")
convert_marker("2:1234567", "RM", "NUM")

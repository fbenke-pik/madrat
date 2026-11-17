#' Create source documentation for a given madrat full function
#'
#' Collects the sources of all calcFunctions that are called in fullMFA
#' and their respective bibtex entries in SOURCE_INFO.txt if available
#' Saves a table with the calcFunctions, source names and bibtex keys as csv
#'
#' @author Leonie Schweiger, Falk Benke
#'
#' @param fullFunction a madrat full function
#'
#' @export
createSourceDocumentation <- function(fullFunction, outputFile = "./sources.csv") {
  # get mapping of calc functions to file names in full function
  mapping <- extractCalcOutputCalls(fullFunction)
  mapping$Source <- ""

  for (fn in mapping$Filename) {
    sources <- getSources(mapping[mapping$Filename == fn, "CalcFunction"])
    mapping[mapping$Filename == fn, "Source"] <- paste0(sources$source, collapse = ", ")
  }

  mapping <- mapping[order(mapping$Filename), , drop = FALSE]

  utils::write.csv(mapping, outputFile, row.names = FALSE)
}

#' parse full function to obtain a mapping of calc functions and the output
#' files they are producing
extractCalcOutputCalls <- function(fullFunction) {
  # convert the function body to text
  text <- paste(deparse(fullFunction), collapse = "\n")

  # regex to match calcOutput("FunctionName", ..., file = "filename.cs4r")
  pattern <- 'calcOutput\\s*\\(\\s*"([A-Za-z0-9_]+)"[^\\)]*file\\s*=\\s*"([A-Za-z0-9_./]+)"'

  # find matches
  matches <- gregexpr(pattern, text, perl = TRUE)
  captures <- regmatches(text, matches)[[1]]

  # extract filenames and function names
  extract <- lapply(captures, function(x) {
    m <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]
    data.frame(Filename = m[3], CalcFunction = paste("calc", m[2], sep = ""), stringsAsFactors = FALSE)
  })

  # combine into one data.frame
  return(do.call(rbind, extract))
}

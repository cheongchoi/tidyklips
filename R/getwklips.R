#' `getwklips()' is used to obtain data.frame for KLIPS (career data)
#'
#'
#' @param path A string vector specifying folder containing KLIPS career data
#' @param datatype A string vector specifying the format of the raw data you want to convert to a data frame ("spss", "sas", "stata", "excel")
#' @param klipsvars A string vector specifying the variables in the raw data that you want to convert to a data frame ("jobseq", "jobtype")
#' @param outvars A string vector specifying the variable names of converted data ("jobseq", "jobtype")
#'
#' @return A data frame containing klips household member data with the specified years and variables.
#' * `getwklips()` returns an integer dataframe with two and more columns and
#'   rows for each respondent. The first column, `pid`,
#'   refers to the respondent id number, and the last column, `year`,
#'   refers to the year that the user wants to include in the dataframe.
#'
#' @importFrom stringr str_sub
#' @importFrom stringr str_width
#' @importFrom readxl read_excel
#' @importFrom haven read_sav
#' @importFrom haven read_dta
#' @importFrom haven read_sas
#' @importFrom dplyr across
#' @importFrom dplyr group_by
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr summarise
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' path <- system.file("extdata", package = "tidyklips")
#' df <- getwklips(path = path, datatype = "stata")
#' df %>%
#'   dplyr::group_by(jobseq) %>%
#'   dplyr::summarise(count = dplyr::n()) %>%
#'   dplyr::mutate(proportion = count / sum(count))
#'
#'
#' @export
getwklips <- function(path, datatype = c("stata", "spss", "sas", "xlsx"), klipsvars=c("jobseq", "jobtype"), outvars=c("jobseq", "jobtype")) {
  dtype <- ifelse(datatype == "stata", ".dta",
                  ifelse(datatype=="spss", ".sav",
                         ifelse(datatype=="sas", ".sas7bdat", ".xlsx")))

  path <- ifelse ((str_sub(path,-1,-1)=="/") | is.null(path), path, paste0(path, "/"))

  headers <- c("pid", outvars)
  temp <- data.frame(matrix(, ncol=length(outvars) + 1, nrow =0))
  names(temp) <- headers
  cols <- sapply(temp, is.logical)
  temp[,cols] <- lapply(temp[,cols], as.numeric)
  inputvars <- klipsvars

  if (datatype=="stata") {
    filename <- paste0(path,"klips", "26", "w", dtype)
    outputvars <- paste0("temp$", outvars)

    temp <- read_dta(file = filename, encoding='utf-8', col_select=c("pid", inputvars))

    for (j in 1:length(klipsvars)) {
      colnames(temp)[j+1] <- outvars[j]
    }

  } else if (datatype=="spss") {

    filename <- paste0(path,"klips", "26", "w", dtype)
    outputvars <- paste0("temp$", outvars)

    temp <- read_sav(file = filename, encoding='utf-8', col_select=c("pid", inputvars))

    for (j in 1:length(klipsvars)) {
      colnames(temp)[j+1] <- outvars[j]
    }

  } else if (datatype=="sas") {

    filename <- paste0(path,"klips", "26", "w", dtype)
    outputvars <- paste0("temp$", outvars)

    temp <- read_sas(data_file = filename,  col_select=c("pid", inputvars))

    for (j in 1:length(klipsvars)) {
      colnames(temp)[j+1] <- outvars[j]
    }


  } else if (datatype=="xlsx") {
    filename <- paste0(path,"klips", "26", "w", dtype)
    outputvars <- paste0("temp$", outvars)

    temp <- read_excel(path = filename)
    temp <- temp  %>%
      dplyr::select(c(pid, inputvars))

    for (j in 1:length(klipsvars)) {
      colnames(temp)[j+1] <- outvars[j]
    }

  }

  temp[] <- lapply(temp, function(x) { attributes(x) <- NULL; x })

  return(temp)
}


#' `gethklips()' is used to obtain data.frame for KLIPS (head of household survey)
#'
#'
#' @param path A string vector specifying folder containing KLIPS head of household survey data
#' @param year an integer vector  specifying the years from 1998 to 2023 that the user wants to include in the dataframe.
#' @param datatype A string vector specifying the format of the raw data you want to convert to a data frame ("spss", "sas", "stata", "excel")
#' @param klipsvars A string vector specifying the variables in the raw data that you want to convert to a data frame ("0141", "2102")
#' @param outvars A string vector specifying the variable names of converted data ("province", "income")
#'
#' @return A data frame containing klips household member data with the specified years and variables.
#' * `gethklips()` returns an integer dataframe with two and more columns and
#'   rows for each head of household. The first column, `hhid`,
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
#' df <- gethklips(path = path, year = 2023, datatype = "stata")
#' df %>%
#'   dplyr::group_by(year) %>%
#'   dplyr::summarise(count = dplyr::n()) %>%
#'   dplyr::mutate(proportion = count / sum(count))
#'
#'
#' @export
gethklips <- function(path, year, datatype = c("stata", "spss", "sas", "xlsx"), klipsvars=c("0141", "2102"), outvars=c("province", "income")) {
  dtype <- ifelse(datatype == "stata", ".dta",
                  ifelse(datatype=="spss", ".sav",
                         ifelse(datatype=="sas", ".sas7bdat", ".xlsx")))


  set <- case_when(
    year == 1998 ~ "01",
    year == 1999 ~ "02",
    year == 2000 ~ "03",
    year == 2001 ~ "04",
    year == 2002 ~ "05",
    year == 2003 ~ "06",
    year == 2004 ~ "07",
    year == 2005 ~ "08",
    year == 2006 ~ "09",
    year == 2007 ~ "10",
    year == 2008 ~ "11",
    year == 2009 ~ "12",
    year == 2010 ~ "13",
    year == 2011 ~ "14",
    year == 2012 ~ "15",
    year == 2013 ~ "16",
    year == 2014 ~ "17",
    year == 2015 ~ "18",
    year == 2016 ~ "19",
    year == 2017 ~ "20",
    year == 2018 ~ "21",
    year == 2019 ~ "22",
    year == 2020 ~ "23",
    year == 2021 ~ "24",
    year == 2022 ~ "25",
    year == 2023 ~ "26")

  path <- ifelse ((str_sub(path,-1,-1)=="/") | is.null(path), path, paste0(path, "/"))

  headers <- c("orghid18", outvars, "year")
  df <- data.frame(matrix(, ncol=length(outvars) + 2, nrow =0))
  names(df) <- headers
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  inputvars <- klipsvars

  if (datatype=="stata") {
    for (i in 1:length(year)) {
      for (k in 1:length(klipsvars)) {
        if (str_width(klipsvars[k])<=3 | str_width(klipsvars[k]) > 4) {
          inputvars[k] <- klipsvars[k]
        } else {
          inputvars[k] <- paste0("h", set[i], klipsvars[k])
        }
      }

      filename <- paste0(path,"klips", set[i], "h", dtype)
      outputvars <- paste0("temp$", outvars)

      temp <- read_dta(file = filename, encoding='utf-8', col_select=c("orghid18", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      # new_name <-  paste0("h", year[i])
      # assign(new_name, temp)

      df <- rbind(df, temp)
    }
  } else if (datatype=="spss") {
    for (k in 1:length(klipsvars)) {
      if (str_width(klipsvars[k])<=3 | str_width(klipsvars[k]) > 4) {
        inputvars[k] <- klipsvars[k]
      } else {
        inputvars[k] <- paste0("h", set[i], klipsvars[k])
      }
    }
    for (i in 1:length(year)) {
      filename <- paste0(path,"klips", set[i], "h", dtype)
      outputvars <- paste0("temp$", outvars)

      temp <- read_sav(file = filename, encoding='utf-8', col_select=c("orghid18", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  } else if (datatype=="sas") {
    for (k in 1:length(klipsvars)) {
      if (str_width(klipsvars[k])<=3 | str_width(klipsvars[k]) > 4) {
        inputvars[k] <- klipsvars[k]
      } else {
        inputvars[k] <- paste0("h", set[i], klipsvars[k])
      }
    }
    for (i in 1:length(year)) {
      filename <- paste0(path,"klips", set[i], "h", dtype)
      outputvars <- paste0("temp$", outvars)

      temp <- read_sas(data_file = filename,  col_select=c("orghid18", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  } else if (datatype=="xlsx") {
    for (k in 1:length(klipsvars)) {
      if (str_width(klipsvars[k])<=3 | str_width(klipsvars[k]) > 4) {
        inputvars[k] <- klipsvars[k]
      } else {
        inputvars[k] <- paste0("h", set[i], klipsvars[k])
      }
    }
    for (i in 1:length(year)) {
      filename <- paste0(path,"klips", set[i], "h", dtype)
      outputvars <- paste0("temp$", outvars)

      temp <- read_excel(path = filename)
      temp <- temp  %>%
        dplyr::select(c(pid, inputvars)) %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  }


  df[] <- lapply(df, function(x) { attributes(x) <- NULL; x })

  return(df)
}

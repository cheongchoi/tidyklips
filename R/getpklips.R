#' Obtain data.frame for the Korea Labor & Income Panel Study (only household member survey)
#'
#'
#' @param path A string specifying folder containing KLIPS household member survey data
#' @param year integer specifying the years from 1998 to 2023 that the user wants to include in the dataframe.
#' @param datatype A string specifying the format of the raw data you want to convert to a data frame ("spss", "sas", "stata", "excel")
#' @param klipsvars A string or strings specifying the variables in the raw data that you want to convert to a data frame ("0101", "0107")
#' @param outvars A string or strings specifying the variable names of converted data ("gender", "age")
#'
#' @return A data frame containing klips household member data with the specified years and variables.
#' @importFrom stringr str_sub
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
#' @examples \dontrun{
#' # Plot of race/ethnicity by county in Illinois for 2010
#' library(tidyklips)
#' library(tidyverse)
#'
#' df <- getpklips(path = "../datastata", year = 2022:2023, datatype = "stata")
#' df %>%
#'   group_by(year, gender) %>%
#'   summarise(count = n(),
#'             proportion = count/sum(gender, na.rm=TRUE) )
#' }
#' @export
getpklips <- function(path, year, datatype = c("stata", "spss", "sas", "xlsx"), klipsvars=c("0101", "0107"), outvars=c("gender", "age")) {

  if (sum(year %in% 1998:2023)< length(year)) {
    stop("KLIPS in tidyklips supports between 1998 and 2023.", call. = FALSE)
  }

  dtype <- ifelse(datatype == "stata", ".dta",
                  ifelse(datatype=="spss", ".sav",
                         ifelse(datatype=="sas", ".sas7bdat", ".xlsx")))

  setid <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23",
             "24", "25", "26")

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

  headers <- c("pid", outvars, "year")
  df <- data.frame(matrix(, ncol=length(outvars) + 2, nrow =0))
  names(df) <- headers
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)

  if (datatype=="stata") {

    for (i in 1:length(year)) {
      filename <- paste0(path,"/klips", set[i], "p", dtype)
      inputvars <- paste0("p", set[i], klipsvars)
      outputvars <- paste0("temp$", outvars)

      temp <- read_dta(file = filename, encoding='utf-8', col_select=c("pid", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      # new_name <-  paste0("p", year[i])
      # assign(new_name, temp)

      df <- rbind(df, temp)
    }
  } else if (datatype=="spss") {

    for (i in 1:length(year)) {
      filename <- paste0(path,"/klips", set[i], "p", dtype)
      inputvars <- paste0("p", set[i], klipsvars)
      outputvars <- paste0("temp$", outvars)

      temp <- read_sav(file = filename, encoding='utf-8', col_select=c("pid", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  } else if (datatype=="sas") {

    for (i in 1:length(year)) {
      filename <- paste0(path,"/klips", set[i], "p", dtype)
      inputvars <- paste0("p", set[i], klipsvars)
      outputvars <- paste0("temp$", outvars)

      temp <- read_sas(data_file = filename,  col_select=c("pid", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  } else if (datatype=="spss") {

    for (i in 1:length(year)) {
      filename <- paste0(path,"/klips", set[i], "p", dtype)
      inputvars <- paste0("p", set[i], klipsvars)
      outputvars <- paste0("temp$", outvars)

      temp <- read_sav(file = filename, encoding='utf-8', col_select=c("pid", inputvars))
      temp <- temp  %>%
        dplyr::mutate(year = year[i])

      for (j in 1:length(klipsvars)) {
        colnames(temp)[j+1] <- outvars[j]
      }

      df <- rbind(df, temp)
    }
  } else if (datatype=="xlsx") {

    for (i in 1:length(year)) {
      filename <- paste0(path,"klips", set[i], "p", dtype)
      inputvars <- paste0("p", set[i], klipsvars)
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


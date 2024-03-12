
# Contains two external functions:
#     psytools_folder_getraw
#     psytools_folder_convert

# Use this function to safely make a combined ID+timestamp variable
#   delimited by "_":
safe_idstamp_maker <- function(x, y) {
  x <- gsub("_", "-", x)
  y <- gsub("_", "-", y)
  return(paste(x, y, sep = "_"))
}

#' list_psytools_files
#'
#' List psytools files in a directory (folder_loc).
#' Files can be filtered with two regular expressions which are pasted together
#'  one for extensions, one for the names.
#'  e.g. \code{`file_ext_regexp = "\\.csv\\.gz$"`} selects all files with
#'  a .csv.gz extension. For more information on regular expressions in R, see
#'  \code{\link[base]{regexp}}.
#'
#' @param folder_loc "/path/to/folder/to/scan/"
#' @param regexp_filter_ext regular expression filter for file extensions.
#' @param regexp_filter_name regular expression filter for file names
#'     (pasted with \code{regexp_filter_ext})
#'
#' @returns A named character vector of file paths.
#'     The names have directory paths and extensions stripped
#'     (the latter via \code{regexp_filter_ext}).
#'
#' @examples
#' \dontrun{
#' # example code
#' # default - list all .csv.gz files:
#' list_psytools_files("my_psytools_log_directory/")
#' # list all .csv files:
#' list_psytools_files("my_psytools_log_directory/", "\\.csv$")
#' # list all .csv files with "STUDY_B" in the file name anywhere before the
#' #   file extension:
#' list_psytools_files("my_psytools_log_directory/", "\\.csv$", ".*STUDY_B.*")
#' }
#' @importFrom stats setNames
#' @export
list_psytools_files <- function(folder_loc,
                                regexp_filter_ext = "\\.csv\\.gz$",
                                regexp_filter_name = ".*") {
  input <- list.files(
    folder_loc,
    pattern = paste0(regexp_filter_name, regexp_filter_ext),
    full.names = TRUE,
    recursive = FALSE
  )
  # Make names (no path, no extension):
  nm <- basename(input)
  nm <- gsub(regexp_filter_ext, "", nm)

  setNames(input, nm)
}

#' read_psytools_logs
#'
#' Reads in psytools log files to a list. Note: errors indicate the csv
#'     format is not as expected. Try to check the raw files.
#'
#' @param filenames a named list of filepaths (e.g. output from
#'     \code{\link{list_psytools_files}})
#' @param format how to try to read the data? csv (default) = tabular,
#'     txt = raw strings.
#' @importFrom readr read_csv
#' @importFrom readr cols_only col_character col_datetime col_double col_logical
#' @export
read_psytools_logs <- function(filenames, format = c("csv", "txt")) {

  fmt <- readr::cols_only(
    `User code` = readr::col_character(),
    Iteration = readr::col_double(),
    Language = readr::col_character(),
    Completed = readr::col_logical(),
    `Completed Timestamp` = readr::col_datetime(format = ""),
    `Processed Timestamp` = readr::col_datetime(format = ""),
    Block = readr::col_character(),
    Trial = readr::col_character(),
    `Trial result` = readr::col_character(),
    Response = readr::col_character(),
    `Response time [ms]` = readr::col_double()
  )

  lapply(filenames,
         readr::read_csv,
         col_types = fmt,
         show_col_types = FALSE)
}

#' process_psytools_logs
#'
#' Reshape log file into tabular data. Reading table column names from the
#'     \code{Trial} column in the log, and associated values from the
#'     last logged \code{Trial Result} under that \code{Trial}.
#'
#' @param x a list of imported psytools logs
#'     (see: \code{\link{read_psytools_logs}}).
#'
#' @importFrom dplyr arrange mutate last
#' @importFrom tidyr separate_wider_delim pivot_wider
#' @export
process_psytools_logs <- function(x) {

  .preproc <- function(x) {
    bad_rows <- which((x$`Trial result` == "skip_back") &
                        (x$Response %in% c("skip_back",
                                           "keybRefuse",
                                           "skip_q")))
    x <- x[-bad_rows, ]
    x$IDDATE <- safe_idstamp_maker(x$`User code`,
                                   x$`Completed Timestamp`)
    x[order(x$`User code`, x$`Completed Timestamp`), ]
  }

  dat <- lapply(x, .preproc)

  .make_wide <- function(x) {
    x <- pivot_wider(x,
                     id_cols = c("IDDATE", "Iteration", "Completed"),
                     values_fn = dplyr::last,
                     names_from = "Trial",
                     values_from = "Trial result")
    x <- separate_wider_delim(x,
                              cols = "IDDATE",
                              names = c("UserCode", "CompletedTimestamp"),
                              delim = "_")
    x
  }

  res <- lapply(dat, .make_wide)
  res
}

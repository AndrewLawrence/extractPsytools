
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
#' List psytools files in a directory (folder_location).
#' Files can be filtered with two regular expressions which are pasted together
#'  one for extensions, one for the names.
#'  e.g. \code{`file_ext_regexp = "\\.csv\\.gz$"`} selects all files with
#'  a .csv.gz extension. For more information on regular expressions in R, see
#'  \code{\link[base]{regexp}}.
#'
#' @param folder_location "/path/to/folder/to/scan/"
#' @param regexp_filter_ext regular expression filter for file extensions.
#'     The default selects all gzipped csv files.
#'     For unzipped csv use \code{"\\.csv$"}.
#' @param regexp_filter_name regular expression filter for file names
#'     (is pasted with \code{regexp_filter_ext})
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
list_psytools_files <- function(folder_location,
                                regexp_filter_ext = "\\.csv\\.gz$",
                                regexp_filter_name = ".*") {
  input <- list.files(
    folder_location,
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
#' Reads in data from a list of Psytools log file locations to a list.
#'
#' Errors indicate the format is not as expected for a psytools log. Check all
#' files in the input list are actually Psytools logs.
#'
#' @param filenames a list of filepaths (e.g. output from
#'     \code{\link{list_psytools_files}})
#'
#' @returns A list of \code{\link[tibble:tbl_df]{tibble}} objects,
#'     one per input file. Convert these from log format to tables with
#'     \code{\link{process_psytools_logs}}.
#'
#' @importFrom readr read_csv
#' @importFrom readr cols_only col_character col_datetime col_double col_logical
#' @export
read_psytools_logs <- function(filenames) {

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
#' Reshape log file into tabular data. Column names are read from the contents
#'     of the \code{Trial} column in the log. The associated values
#'     for a given \code{Trial} are read from the last logged
#'     \code{Trial Result}.
#'
#' @param x a list of imported psytools logs
#'     (see: \code{\link{read_psytools_logs}}).
#' @param include_skipbacks If TRUE, will simulate the behaviour of an old
#'     (bugged) version of the script.
#' @returns A list of \code{\link[tibble:tbl_df]{tibble}} objects,
#'     one per input log file. Objects have been rotated from the input log
#'     format, such that \code{Trial}s are columns and each row is a
#'     single acquisition session.
#'
#' @importFrom dplyr arrange mutate last
#' @importFrom tidyr separate_wider_delim pivot_wider
#' @export
process_psytools_logs <- function(x,
                                  include_skipbacks = FALSE) {

  .preproc <- function(x) {
    bad_rows <- which((x$`Trial result` == "skip_back") &
                        (x$Response %in% c("skip_back",
                                           "keybRefuse",
                                           "skip_q")))

    if (!include_skipbacks) x <- x[-bad_rows, ]

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

#' convert_psytools_folder
#'
#' Read all psytools logfiles from a folder (\code{folder_location}),
#'     rotate/format data (using \code{\link{process_psytools_logs}}) and
#'     write out results in csv format to a folder specified by
#'     \code{output_location} (default: the input folder).
#'
#' @inheritParams list_psytools_files
#' @inherit process_psytools_logs return
#' @param output_location Where to place output files
#'     (default: same directory as input).
#' @param output_suffix Each output file will be named as the input, but
#'     with \code{output_suffix} added.
#'     E.g. the default (\code{"_proc"}) means that
#'     \code{"input/data.csv.gz"} will become
#'     \code{"output/data_proc.csv"}.
#' @param output_format \code{"xlsx"}, \code{"csv"},
#'     both (i.e. \code{c("xlsx", "csv")}),
#'     or "none" to suppress output and simply return processed data.
#' @importFrom writexl write_xlsx
#' @importFrom readr write_csv
#' @export
convert_psytools_folder <- function(folder_location,
                                    output_location = folder_location,
                                    output_suffix = "_proc",
                                    output_format = "xlsx",
                                    regexp_filter_ext = "\\.csv\\.gz$",
                                    regexp_filter_name = ".*") {

  output_format <- match.arg(output_format,
                             choices = c("xlsx", "csv", "none"),
                             several.ok = TRUE)

  output_location <- normalizePath(output_location, mustWork = FALSE)

  # Check output directory:
  if (!dir.exists(output_location)) {
    dir.create(output_location, recursive = TRUE)
  }

  fl <- list_psytools_files(folder_location = folder_location,
                            regexp_filter_ext = regexp_filter_ext,
                            regexp_filter_name = regexp_filter_name)
  data <- read_psytools_logs(fl)
  proc <- process_psytools_logs(data)

  outnames <- file.path(output_location, paste0(names(fl), output_suffix)) |>
    normalizePath(mustWork = FALSE)

  # Write out if requested:
  if ("xlsx" %in% output_format) {
    for (i in seq.int(length(proc))) {
      writexl::write_xlsx(x = proc[[i]],
                          path = paste0(outnames[[i]], ".xlsx"))
    }
  }
  if ("csv" %in% output_format) {
    for (i in seq.int(length(proc))) {
      readr::write_csv(x = proc[[i]],
                       file = paste0(outnames[[i]], ".csv"))
    }
  }

  # return the processed data:
  proc
}


#' skipback_bugreport_folder
#'
#' Read all psytools logfiles from a folder (\code{folder_location}),
#'     for each write out an excel-file with data values that would have
#'     been lost if processed with a previous (bugged) version of the
#'     psytools processing scripts.
#'
#' This function was developed to help assess the impact of a retrospective
#'     script problem identified in 2024. The problem pre-dates the script's
#'     move to github/r-package.
#'
#' @inheritParams list_psytools_files
#' @param output_location Where to place output files
#'     (default: same directory as input).
#' @param output_suffix Each output file will be named as the input, but
#'     with \code{output_suffix} added.
#'     E.g. the default (\code{"_skipbackbugreport"}) means that
#'     \code{"input/data.csv.gz"} will become
#'     \code{"output/data_skipbackbugreport.csv"}.
#' @param output_format \code{"xlsx"}, \code{"csv"},
#'     both (i.e. \code{c("xlsx", "csv")}),
#'     or "none" to suppress output and simply return processed data.
#' @param remove_ids a character vector of IDs to remove.
#' @importFrom writexl write_xlsx
#' @importFrom readr write_csv
#' @export
skipback_bugreport_folder <- function(folder_location,
                                      output_location = folder_location,
                                      output_suffix = "_skipbackbugreport",
                                      output_format = "xlsx",
                                      regexp_filter_ext = "\\.csv\\.gz$",
                                      regexp_filter_name = ".*",
                                      remove_ids = c("EBTEST")) {


  output_format <- match.arg(output_format,
                             choices = c("xlsx", "csv", "none"),
                             several.ok = TRUE)

  output_location <- normalizePath(output_location, mustWork = FALSE)

  # Check output directory:
  if (!dir.exists(output_location)) {
    dir.create(output_location, recursive = TRUE)
  }

  fl <- list_psytools_files(folder_location = folder_location,
                            regexp_filter_ext = regexp_filter_ext,
                            regexp_filter_name = regexp_filter_name)
  data <- read_psytools_logs(fl)
  # process correctly:
  proc1 <- process_psytools_logs(data)
  # process with bug:
  proc2 <- process_psytools_logs(data, include_skipbacks = TRUE)


  # filter out non-relevant IDs and
  #   align comparable columns in bugged file to non-bugged:
  for (i in seq_along(proc2)) {
    proc1[[i]] <- proc1[[i]][!(proc1[[i]]$UserCode %in% remove_ids), ]
    proc2[[i]] <- proc2[[i]][!(proc2[[i]]$UserCode %in% remove_ids), ]
    proc2[[i]] <- proc2[[i]][, colnames(proc1[[i]])]
  }

  .skipback_bugreport <- function(df1, df2) {
    if (! any(df2 == "skip_back", na.rm = TRUE)) {
      # if no skip-backs are present using the old processing method,
      #   return empty object of the correct format:
      return(data.frame(matrix(NA, nrow = 0, ncol = 8,
                               dimnames = list(
                                 NULL,
                                 c("row", "col", "UserCode",
                                   "CompletedTimestamp", "Iteration",
                                   "Completed", "varname", "new")
                               ))))
    }

    # otherwise identify problems:
    sel <- which(df1 != df2, arr.ind = TRUE)
    # Note: above does not identify as a problem where either df1 or df2 have NA
    #       values. This is desired and happens implicity by using `which`
    #       around the "!=" call.

    # extract associated variable name and subject info:
    varname <- colnames(df2)[sel[, 2]]
    subs <- df1[sel[, 1], 1:4]

    # extract new values:
    new <- vapply(seq_len(nrow(sel)),
                  \(i) as.character(df1[sel[i, 1], sel[i, 2]]),
                  FUN.VALUE = "c")

    # return:
    rtn <- cbind(sel, subs, varname, new)
    rtn[order(rtn$row), ]
  }

  # init output data:
  bug_reports <- vector(mode = "list", length = length(proc1))
  # populate output data:
  for (i in seq_along(proc1)) {
    bug_reports[[i]] <- .skipback_bugreport(df1 = proc1[[i]],
                                            df2 = proc2[[i]])
  }

  outnames <- file.path(output_location, paste0(names(fl), output_suffix)) |>
    normalizePath(mustWork = FALSE)

  # Write out if requested:
  if ("xlsx" %in% output_format) {
    for (i in seq.int(length(bug_reports))) {
      writexl::write_xlsx(x = bug_reports[[i]],
                          path = paste0(outnames[[i]], ".xlsx"))
    }
  }
  if ("csv" %in% output_format) {
    for (i in seq.int(length(bug_reports))) {
      readr::write_csv(x = bug_reports[[i]],
                       file = paste0(outnames[[i]], ".csv"))
    }
  }

  # return the processed data:
  bug_reports
}


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
#'  a .csv.gz extension. For more information on regular expressions, see
#'  \code{\link[base]{regexp}}.
#'
#' @param folder_loc "/path/to/folder/to/scan/"
#' @param regexp_filter_ext regular expression filter for file extensions.
#' @param regexp_filter_name regular expression filter for file names
#'     (pasted with \code{regexp_filter_ext})
#'
#' @returns A named list of file paths. The names have directory paths and
#'     extensions stripped (via \code{file_ext_regexp}).
#'
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
  input
}

#' @importFrom readr read_lines
#' @importFrom purrr map
#' @importFrom purrr `%>%`
psytools_folder_getraw <- function(folder_loc,
                                   file_ext_regexp = ".csv.gz$") {
  # List all files in "raw" directory ending in .csv.gz:
  input <- list.files(
    folder_loc,
    pattern = file_ext_regexp,
    full.names = TRUE,
    recursive = FALSE
  )

  # Make tidy names for them:
  in_names <- list.files(
    folder_loc,
    pattern = ".csv.gz$",
    full.names = FALSE,
    recursive = FALSE
  )
  in_names <- gsub(".csv.gz", "", in_names)

  names(input) <- in_names

  # Read in the csv files:
  dat <- map(input,
             ~ read_lines(.x))
  dat
}

#' @importFrom purrr walk2
#' @importFrom tidyr separate
#' @importFrom openxlsx write.xlsx
psytools_convert_folder <- function(folder_loc,
                                    output_loc) {
  # List all files in "raw" directory ending in .csv.gz:
  input <- list.files(
    folder_loc,
    pattern = ".csv.gz$",
    full.names = TRUE,
    recursive = FALSE
  )

  # Make tidy names for them:
  in_names <- list.files(
    folder_loc,
    pattern = ".csv.gz$",
    full.names = FALSE,
    recursive = FALSE
  )
  in_names <- gsub(".csv.gz", "", in_names)

  names(input) <- in_names

  # Make output names:
  out_names <- paste0(output_loc, in_names, ".xlsx")

  # Read in the csv files:
  dat <- map(
    input,
    ~ read_csv(.x) %>%
      #  The following IDs are excluded as they contain junk data:
      filter(!`User code` %in% c("EBTEST", "DEVELOPMENT")) %>%
      # potential BUGFIX (2024-03-04):
      filter(!(
        `Trial result` == "skip_back" &
          Response %in% c("skip_back", "keybRefuse", "skip_q")
      )) %>%
      mutate(IDDATE = safe_idstamp_maker(`User code`,
                                         `Completed Timestamp`)) %>%
      arrange(`User code`, `Completed Timestamp`)
  )

  # Pivot to one row (multiple variables) per acquisition:
  res <- map(
    dat,
    ~ .x %>% pivot_wider(
      id_cols = c(IDDATE, Iteration, `Completed`),
      # NOTE: Comment next code line out to inspect
      #       duplicated data. In development
      #       it was always that case that repeated
      #       data (nested within ID and Timestamp)
      #       was valid in the last slot.
      values_fn = last,
      names_from = Trial,
      values_from = `Trial result`
    ) %>%
      separate(
        IDDATE,
        into = c("UserCode", "CompletedTimestamp"),
        sep = "_"
      )
  )

  # Write out pivoted data as xlsx format:
  walk2(res,
        out_names,
        ~ openxlsx::write.xlsx(.x, file = .y, overwrite = TRUE))

  return(list(input = dat,
              output = res))
}

test_that(
  "listing files works:",
  {
    loc <- withr::local_tempdir()
    withr::local_dir(loc)

    dummy_file_names <- c("STUDY_FORM_A",
                          "STUDY_FORM_B",
                          "STUDY_FORM_C",
                          "STUDY_FORM_D1",
                          "STUDY_FORM_D2",
                          "STUDY_FORM_D3")

    # setup a dummy directory:
    make_dummy_psytools_files(dummy_file_names, data = FALSE)

    all_files <- list_psytools_files(".")
    # test the regexp name filter:
    d_files <- list_psytools_files(".", regexp_filter_name = ".*D.")

    expect_setequal(names(all_files), dummy_file_names)
    expect_setequal(names(d_files),
                    dummy_file_names[grepl("_D.$", dummy_file_names)])
    # Check we get a length 0 return if no matching files:
    expect_length(list_psytools_files(".", regexp_filter_ext = "\\.txt$"), 0L)

    # Check we get an error when reading a malformatted file.
    expect_error(suppressWarnings(read_psytools_logs(d_files)))
  }
)


test_that(
  "reading files works:",
  {
    loc <- withr::local_tempdir()
    withr::local_dir(loc)

    dummy_file_names <- c("STUDY_FORM_A",
                          "STUDY_FORM_B",
                          "STUDY_FORM_C",
                          "STUDY_FORM_D1",
                          "STUDY_FORM_D2",
                          "STUDY_FORM_D3")

    # setup a dummy directory:
    make_dummy_psytools_files(dummy_file_names, data = TRUE)

    data <- read_psytools_logs(list_psytools_files("."))
    proc <- process_psytools_logs(data)

    # we got all 6 files:
    expect_equal(length(data), 6L)
    # files have the same colnames:
    expect_equal(colnames(data[[1]]), colnames(data[[2]]))

    # The processed data doesn't include skip_backs:
    any_skip_back <- vapply(
      proc,
      \(x) any(x[,seq(from = 7L, to = ncol(x))] == "skip_back"),
      FUN.VALUE = TRUE
    )

    expect_false(any(any_skip_back, na.rm = TRUE))
  }
)

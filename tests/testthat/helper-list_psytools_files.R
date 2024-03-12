make_dummy_psytools_file <- function(fname) {
  # in the current working directory will make a dummy psytools log file
  file.create(fname)
}

make_dummy_psytools_files <- function(froots, ext = ".csv.gz") {
  for (f in froots) {
    make_dummy_psytools_file(paste0(f, ext))
  }
}

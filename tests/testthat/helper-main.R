# nolint start
example_data <- function() {
  c("User code,Iteration,Language,Completed,Completed Timestamp,Processed Timestamp,Block,Trial,Trial result,Response,Response time [ms]",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,,MaxVolume,UnSupported,,",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_01,B1_PB2_01,1,continue,997",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_02,B1_PB2_02,12,,",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_02,B1_PB2_03,13,,",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_02,B1_PB2_04,12,,",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_02,B1_PB2_05,12,,",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_08,B1_PB2_08,2,continue,571",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_09,B1_PB2_09,13-5-2017,continue,27661",
    "subject1,1,en,t,2017-04-12 16:32:28.707,2017-04-12 16:32:29.787,B1_PB2_10,B1_PB2_10,5,continue,1411",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,,MaxVolume,UnSupported,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_01,B1_PB2_01,1,continue,1269",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_02,9,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_03,13,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_04,10,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_05,7,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_08,B1_PB2_08,2,continue,1071",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_09,B1_PB2_09,0-0-Year,skip_back,19843",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_09,B1_PB2_09,skip_back,skip_back,19843",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_08,B1_PB2_08,2,skip_back,759",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_08,B1_PB2_08,skip_back,skip_back,759",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_02,9,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_03,13,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_04,10,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_02,B1_PB2_05,7,,",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_08,B1_PB2_08,2,continue,2429",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_09,B1_PB2_09,25-11-2015,continue,33313",
    "subject2,1,en,t,2017-04-13 20:49:34.993,2017-04-13 20:49:37.36,B1_PB2_10,B1_PB2_10,4,continue,1741"
  )
}
# nolint end

make_dummy_psytools_file <- function(fname, data = FALSE) {
  # in the current working directory will make a dummy psytools log file
  if (!data) {
    return(file.create(fname))
  }
  fl <- gzfile(fname, open = "wt", compression = 6)
  dat <- example_data()
  writeLines(dat, con = fl)
  close(fl)
}

make_dummy_psytools_files <- function(froots, data = FALSE) {
  for (f in froots) {
    make_dummy_psytools_file(paste0(f, ".csv.gz"), data = data)
  }
}


# extractPsytools

extractPsytools is an R package providing functions to parse and extract
data from [Delosis](https://www.delosis.com/) Psytools server raw-data 
log-files.

This software is based on scripts used internally for data processing by 
researchers from the [eBRAIN](https://www.ebrainstudy.com/) study team at KCL.

This software and its authors are not affiliated with Delosis and the software
has only been tested on eBRAIN questionnaire log files. 
There is an R package provided by Delosis available at 
https://github.com/delosis/psytools.

## Installation

You can install \code{extractPsytools} to your R environment directly from
github using the `remotes` (or `devtools`) package:

``` r
require(remotes)
remotes::install_github("AndrewLawrence/extractPsytools")
```

## Example

The main intended use of extractPsytools is to process a folder of 
questionnaire logs from Psytools, converting the log format into tabular data
so questionnaire items become columns. Processed data can be output as xlsx 
and/or csv format.

``` r
library(extractPsytools)
## Given a directory ("my_psytools_data") read all *.csv.gz files, convert 
##  to tabular format and write out results in xlsx format:
convert_psytools_folder("my_psytools_data/")

## ...or as csv format:
convert_psytools_folder("my_psytools_data/", output_format = "csv")

## Note: all file locations/directory paths are relative to R's
##  working directory, so set this appropriately first 
##  e.g. with:
##  setwd("/path/to/parent_directory")
```


library(readr)
library(stringr)
library(dplyr)
library(httr)

# Download and extract KNMI data file and return dataframe
extract_knmi <- function (start_date, end_date, filename) {
  
  download_knmi(start_date, end_date, filename, vars, stations)
  
  knmi_headers <- read_regexp(filename, "^# ([a-zA-Z]+,)+", n_max = 50)
  knmi_headers <- str_trim(str_split(gsub('#', '', knmi_headers), ',', simplify = TRUE))
  
  knmi_lines <- paste0(read_regexp(filename, "^[^#]"), collapse="\n")
  
  knmi <- read_csv(knmi_lines, col_names = knmi_headers) %>%
    mutate( datetime = ymd_hms(paste(YYYYMMDD, paste0(as.numeric(HH)-1, ":00:00")))) %>%
    select(-YYYYMMDD, -HH) %>%
    select(datetime, everything())
  
  return(knmi)
}

# Download KNMI data file and save to disk
download_knmi <- function (start_date, end_date, output_file, vars, stations) {
  start_string <- paste0(pad_nr(year(start_date)), pad_nr(month(start_date)), pad_nr(day(start_date)), pad_nr(hour(start_date)+1))
  end_string <- paste0(pad_nr(year(end_date)), pad_nr(month(end_date)), pad_nr(day(end_date)), pad_nr(hour(end_date)+1))
  
  url <- "http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi"
  body <- list(start=start_string, end=end_string, vars=vars, stns=stations)

  r <- POST( url = url,
  		     body = body,
		     encode = "form")

  bin <- content(r, 'raw')
  writeBin(bin, output_file)
}

pad_nr <- function (nr) {
  str_pad(nr, 2, "left", "0")
}

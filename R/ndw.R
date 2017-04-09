library(dplyr)
library(readr)

# :TODO: Toevoegen functies om contourplot te maken, tijdwegdiagrammen

# Load NDW data files from ndw_dir
load_ndw <- function (ndw_dir) {
  v <- load_data(ndw_dir, "snelheid", full_join)
  i <- load_data(ndw_dir, "intensiteit", full_join)
    
  return(
    list(
      i = i$data,
      i_meta = i$meta,
      v = v$data,
      v_meta = v$meta
      )
    )
}

# Load NDW data, of type snelheid or intensiteit, including metadata
load_data <- function(ndw_dir, type, join_method) {
  meta <- dir(path = ndw_dir, pattern = paste(type, "metadata", sep="_"))
  files <- dir(path = ndw_dir, pattern = type)
  files <- setdiff(files, meta)
  
  list <- sapply(X = files, FUN = function(file) {
    format_ndw(paste0(ndw_dir, file))
  })
  
  data <- Reduce(full_join, list)
  meta <- load_metadata(paste0(ndw_dir, meta))
  
  return( list( data = data, meta = meta ) )
}

# Create a normalized table of the NDW data from the provided csv_file
format_ndw <- function (csv_file) {
  
  # First issue: fix double headers
  headers <- read_csv(csv_file,
                      col_types = cols(.default = "c"),
                      col_names = FALSE,
                      n_max = 2)
  
  th <- t(headers)
  th <- paste_headers(th[,1], th[,2])
  headers <- t(th)
  
  # Read the columns that don't contain traffic data (date/time) as strings, the rest as numbers
  col_types <- sapply(headers, function(h){
    ifelse(startsWith(as.character(h), "period"), "c", "n")
  })
  col_types <- paste0(col_types, collapse="")
  
  # Read the rest of the data and combine with unique headers
  df <- read_csv(csv_file,
    col_names = FALSE,
    col_types = col_types,
    skip = 2)

  names(df) <- headers
  
  df %>% 
    mutate(datetime = ymd_hms(paste(periodStartDate, periodStartTime)),
           weekday = wday(datetime, label = TRUE),
           hour = hour(datetime)) %>% 
    select(-starts_with("period")) %>%
    select(datetime, weekday, hour, everything())
  
  return(df)
}  

# Load and prepare/normalize the provided metadata csv-file
load_metadata <- function (meta_csv_file) {
  x <- read_csv(meta_csv_file, col_types = cols(.default = col_character()), col_names = FALSE)
  tx <- t(x) # transpose
  colnames(tx) <- tx[1,] # Set column names
  tx <- tx[-1,] # remove first row
  tx <- tx[rowSums(is.na(tx)) != ncol(tx),] # remove empty rows
  tx <- tx[,colSums(is.na(tx)) != nrow(tx)] # remove empty columns
  tx <- tbl_df(tx) # Make a proper table
  tx %>%
    mutate(fullSiteReference = paste_headers(measurementSiteReference, index)) %>%
    select(fullSiteReference, everything())
}

# Create a unique NDW site reference by concatenating the measurement site reference and index columns
paste_headers <- function (headers1, headers2) {
  th <- paste(headers1, headers2, sep=";")
  th <- gsub(";NA", "", th)
  return(th)
}
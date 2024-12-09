library(httr)
library(tidyverse)


# reqest session
get_session = function() {
  
  user_agent = 
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) 
        AppleWebKit/537.36 (KHTML, like Gecko) 
        Chrome/98.0.4758.102 Safari/537.36"
  
  sess <- httr::config(useragent = user_agent)
  
  return(sess)
}

#Test
sess <- get_session()
#print(sess)

url <- "https://drive.google.com/drive/folders/1SSoHGbwgyhRwUCzLE0YWvUlS0DjLCd4k"
response <- httr::GET(url, config = sess)
#print(httr::status_code(response))

content <- httr::content(response, as = "text", encoding = "UTF-8")
#writeLines(content, "google_drive_test.html")



####################
# parse google file:
####################
library(rvest)
library(jsonlite)
library(stringr)

parse_google_drive_file <- function(url, content) {
  # Parse HTML content with rvest
  folder_soup <- read_html(content)
  
  # Find the script tag with window['_DRIVE_ivd']
  encoded_data <- NULL
  script_tags <- html_nodes(folder_soup, "script") %>% html_text()
  encoded_data <- script_tags[str_detect(script_tags, "_DRIVE_ivd")] %>%
    str_match("window\\['_DRIVE_ivd'\\]\\s*=\\s*'(.*?)';") %>%
    .[2]

  
  if (is.null(encoded_data)) {
    stop("Cannot retrieve the folder information from the link. You may need to change the permission to 'Anyone with the link', or have had many accesses.")
  }
  
  matches <- gregexpr("\\\\x([0-9A-Fa-f]{2})", encoded_data, perl = TRUE)
  matched_strings <- regmatches(encoded_data, matches)

  decoded_matches <- sapply(matched_strings[[1]], function(match) {
  #print(paste("Raw match:", match))  
  hex_digits <- sub("\\\\x", "", match)  # Remove `\\x`
  #print(paste("Hex digits:", hex_digits)) 
  numeric_value <- strtoi(hex_digits, 16L)  # Convert to numeric
  #print(paste("Numeric value:", numeric_value))  
  character <- rawToChar(as.raw(numeric_value))  # Convert to character
  #print(paste("Decoded character:", character))  
  return(character)
  })

  
  decoded_data <- encoded_data
  for (i in seq_along(matched_strings[[1]])) {
    decoded_data <- sub(matched_strings[[1]][i], decoded_matches[i], decoded_data, fixed = TRUE)
  }

  folder_arr <- fromJSON(decoded_data)
  
  # Folder name and contents extraction
  if (is.null(folder_arr[[1]])) {
    folder_contents <- list()
  } else {
    folder_contents <- folder_arr  
  }
  sep <- " - "  # unicode dash
  title_text <- html_text(html_nodes(folder_soup, "title"))
  splitted <- str_split(title_text, sep)[[1]]
  if (length(splitted) >= 2) {
  name <- paste(splitted[-length(splitted)], collapse = sep)
  } else {
  warning("File/folder name cannot be split correctly. Using full title as fallback.")
  name <- title_text  # fallback name
  }
  
  # structure for the Google Drive folder
  gdrive_file <- list(
    id = str_split(url, "/")[[1]][length(str_split(url, "/")[[1]])], 
    name = name,
    type = "application/vnd.google-apps.folder",
    children = list()
  )
  
  # second-level elements from folder_contents[[1]]
  entries <- folder_contents[[1]]

  # Process each entry to extract id, name, and type
  id_name_type_iter <- lapply(entries, function(e) {
    if (length(e) >= 4) {  # Ensure the entry has at least 4 elements
      list(
        id = e[[1]],      # Extract the ID
        name = e[[3]],    # Extract the file name
        type = e[[4]]     # Extract the type
      )
    } else {
      NULL
    }
  })


    gdrive_file$children <- id_name_type_iter
    return(list(gdrive_file = gdrive_file, id_name_type_iter = id_name_type_iter))
  }


# Test
#parse_google_drive_file(url, content)
# success! (warning can be ignored at this place)



# download and parse Google Drive folder structure
download_and_parse_google_drive_link <- function(sess, url) {
  # Fetch HTML content from the folder URL
  for (i in 1:2) {
    # Add "?hl=en" to ensure the content is in English
    url <- ifelse(grepl("\\?", url), paste0(url, "&hl=en"), paste0(url, "?hl=en"))
    
    # Get the folder content
    response <- GET(url, config = sess)
    if (status_code(response) != 200) {
      stop("Failed to fetch the page. HTTP status code: ", status_code(response))
    }
    
    # Update URL in case of redirection
    url <- response$url
  }
  
  # Parse the folder HTML content
  content <- content(response, as = "text", encoding = "UTF-8")
  parsed <- parse_google_drive_file(url, content)
  
  gdrive_file <- parsed$gdrive_file
  id_name_type_iter <- parsed$id_name_type_iter
  
  # Process children (files and subfolders)
  for (child in id_name_type_iter) {
    child_id <- child[[1]]
    child_name <- child[[2]]
    child_type <- child[[3]]
    
    # Skip folders named 'Individual' or 'Results'
    if (child_type == "application/vnd.google-apps.folder" && child_name %in% c("Individual", "Results")) {
      next
    }
    
    if (child_type == "application/vnd.google-apps.folder") {
      # Recursively parse subfolders
      child_folder <- download_and_parse_google_drive_link(
        sess = sess, 
        url = paste0("https://drive.google.com/drive/folders/", child_id)
      )
      gdrive_file$children <- append(gdrive_file$children, list(child_folder))
    } else {
      # Add files to the children list
      gdrive_file$children <- append(gdrive_file$children, list(
        list(id = child_id, name = child_name, type = child_type)
      ))
    }
  }
  
  return(gdrive_file)
}

# Test
download_and_parse_google_drive_link(sess, url)





# Function to recursively search for the folder named 'Predictors'

get_individual_signal_folder_id <- function(sess, url) {
  # Ensure the URL is set to English
  if (!grepl("\\?", url)) {
    url <- paste0(url, "?hl=en")
  } else {
    url <- paste0(url, "&hl=en")
  }
  
  # Fetch the folder content
  response <- GET(url, config = sess)
  if (status_code(response) != 200) {
    stop("Failed to fetch the folder page. HTTP status code: ", status_code(response))
  }
  
  # Parse the HTML content
  content <- content(response, as = "text", encoding = "UTF-8")
  parsed <- parse_google_drive_file(url, content)
  gdrive_file <- parsed$gdrive_file
  id_name_type_iter <- parsed$id_name_type_iter
  
  # Iterate over the children to find 'Predictors'
  for (child in id_name_type_iter) {
    child_id <- child[[1]]
    child_name <- child[[2]]
    child_type <- child[[3]]
    
    # Return the ID if the folder name is 'Predictors'
    if (child_type == "application/vnd.google-apps.folder" && child_name == "Predictors") {
      return(child_id)
    }
    
    # Recursively search if it's another folder
    if (child_type == "application/vnd.google-apps.folder") {
      result <- get_individual_signal_folder_id(
        sess = sess,
        url = paste0("https://drive.google.com/drive/folders/", child_id)
      )
      if (!is.null(result)) {
        return(result)
      }
    }
  }
  
  # If no 'Predictors' folder is found, return NULL
  return(NULL)
}

# Test
get_individual_signal_folder_id(sess, url)





get_directory_structure <- function(gdrive_file, visited = character()) {
  # Initialize an empty directory structure list
  directory_structure <- list()
  
  # Loop through each child in the folder
  for (file in gdrive_file$children) {
    # Replace problematic characters in file names
    file$name <- gsub("/", "_", file$name)
    
    # Skip if file_id is already visited
    if (!is.null(file$id) && file$id %in% visited) {
      next
    }
    
    # Mark file_id as visited
    visited <- c(visited, file$id)
    
    if (file$type == "application/vnd.google-apps.folder") {
      # Add folder to the structure
      directory_structure <- append(directory_structure, list(c(NULL, file$name)))
      
      # Recursively get subdirectory structure
      subdir_structure <- get_directory_structure(file, visited)
      directory_structure <- append(directory_structure, subdir_structure)
    } else if (is.null(file$children)) {
      # Add file to the structure
      directory_structure <- append(directory_structure, list(c(file$id, file$name)))
    }
  }
  
  return(directory_structure)
}



# Test
gdrive_file_test = download_and_parse_google_drive_link(sess, url)
test = get_directory_structure(gdrive_file_test)



duplicates <- check_duplicates(test)





get_url_from_gdrive_confirmation <- function(contents) {
  # Parse the HTML content
  soup <- read_html(contents)
  
  # Find the download form and its action URL
  form <- html_node(soup, "form#download-form")
  if (is.null(form)) {
    stop("Download form not found in the HTML content.")
  }
  
  action <- html_attr(form, "action")
  if (is.null(action)) {
    stop("Form action URL not found.")
  }
  
  # Extract hidden input fields
  inputs <- html_nodes(form, "input[type='hidden']")
  params <- sapply(inputs, function(input) {
    name <- html_attr(input, "name")
    value <- html_attr(input, "value")
    if (!is.null(name) && !is.null(value)) {
      paste0(name, "=", URLencode(value, reserved = TRUE))
    } else {
      NULL
    }
  })
  
  # Construct the full URL with query parameters
  query <- paste(na.omit(params), collapse = "&")
  full_url <- paste0(action, "?", query)
  
  return(full_url)
}


# Test
url <- "https://drive.google.com/uc?id=1T-nogu88A4hcFXijjftSO41K5P4Hj27y"
response <- httr::GET(url, config = sess)
content <- httr::content(response, as = "text", encoding = "UTF-8")

get_url_from_gdrive_confirmation(content)








get_name_id_map <- function(url) {
  sess <- get_session()
  
  # Parse the Google Drive folder
  gdrive_file <- download_and_parse_google_drive_link(sess, url)
  directory_structure <- get_directory_structure(gdrive_file)
  url_prefix <- "https://drive.google.com/uc?id="
  
  # Define mappings for datasets
  datasets_map <- c(
    "SignalDoc.csv" = "signal_doc",
    "PredictorPortsFull.csv" = "op",
    "PredictorAltPorts_Deciles.zip" = "deciles_ew",
    "PredictorAltPorts_DecilesVW.zip" = "deciles_vw",
    "PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct.zip" = "ex_nyse_p20_me",
    "PredictorAltPorts_LiqScreen_NYSEonly.zip" = "nyse",
    "PredictorAltPorts_LiqScreen_Price_gt_5.zip" = "ex_price5",
    "PredictorAltPorts_Quintiles.zip" = "quintiles_ew",
    "PredictorAltPorts_QuintilesVW.zip" = "quintiles_vw",
    "signed_predictors_dl_wide.zip" = "firm_char"
  )
  
  # Create a data frame from directory structure
  df <- do.call(rbind, lapply(directory_structure, function(x) {
    data.frame(file_id = x[1], name = x[2], stringsAsFactors = FALSE)
  })) %>%
    filter(!grepl("xlsx$|docx$|txt$", name)) %>%
    mutate(
      full_name = ifelse(is.na(file_id), name, paste0(name, "/", lag(name, default = ""))),
      file_id = ifelse(!is.na(file_id), paste0(url_prefix, file_id), NA)
    ) %>%
    filter(!is.na(file_id)) %>%
    mutate(download_name = datasets_map[name]) %>%
    filter(!is.na(download_name))
  
  # Process individual signals
  signal_folder_id <- get_individual_signal_folder_id(sess, url)
  signal_folder_url <- paste0("https://drive.google.com/embeddedfolderview?id=", signal_folder_id)
  signal_response <- GET(signal_folder_url)
  signal_text <- content(signal_response, as = "text", encoding = "UTF-8")
  
  # Extract signal file names and IDs
  signal_matches <- data.frame(
    signal = str_extract_all(signal_text, '<div class="flip-entry-title">(.*?).csv</div>')[[1]],
    file_id = str_extract_all(signal_text, 'https://drive\\.google\\.com/file/d/([\\w-]{25,})/view\\?usp=drive_web')[[1]]
  ) %>%
    mutate(file_id = paste0(url_prefix, file_id))
  
  return(list(main = df, signals = signal_matches))
}

# Test
name_id_map = get_name_id_map(url)
name_id_map

# Predictors and download names (no duplicates:)
name_id_map$main %>%
  dplyr::filter(stringr::str_starts(name, "Predictor")) %>%
  dplyr::select(name, download_name)



get_readable_link <- function(url) {
  sess <- get_session()
  response <- GET(url, config = sess)
  readable_link <- get_url_from_gdrive_confirmation(content(response, as = "text", encoding = "UTF-8"))
  return(readable_link)
}


# Test 
get_readable_link("https://drive.google.com/uc?id=1T-nogu88A4hcFXijjftSO41K5P4Hj27y")  #success 




##############
# Creating the package


list_release <- function(urls) {
  releases <- sapply(names(urls), function(x) {
    if (grepl("^release\\d+_url$", x)) {
      sub("release(\\d+)_url", "\\1", x)
    } else {
      NA
    }
  })
  releases <- na.omit(releases)
  
  # Print releases in a tabular format
  print(data.frame(Release = releases))
}





OpenAP <- R6::R6Class(
  "OpenAP",
  public = list(
    name_id_map = NULL,
    individual_signal_id_map = NULL,
    signal_sign = NULL,
    url = NULL,

    initialize = function(release_year = NULL) {
      release_url <- "https://drive.google.com/drive/folders/1SSoHGbwgyhRwUCzLE0YWvUlS0DjLCd4k"
      mappings <- get_name_id_map(release_url)
      self$name_id_map <- mappings$main
      self$individual_signal_id_map <- mappings$signals
      
      # Process the signal documentation
      signal_doc_url <- self$get_url("signal_doc")
      self$signal_sign <- read.csv(signal_doc_url)
    },

    list_port = function() {
      print(self$name_id_map %>%
              dplyr::filter(stringr::str_starts(name, "Predictor")) %>%
              dplyr::select(name, download_name))
    },

    get_url = function(data_name) {
    # Filter the dataset for matching download_name
    data_entry <- self$name_id_map[self$name_id_map$download_name == data_name, ]
    
    # Check if no matching entry exists
    if (nrow(data_entry) == 0) {
      stop("Dataset not found in name_id_map.")
    }
    
    # Extract the URL
    url <- data_entry$file_id[1] 

    # Handle special cases for files that need confirmation
    file_with_confirm <- c("firm_char", "deciles_ew", "deciles_vw")
    if (data_name %in% file_with_confirm) {
      if (exists("get_readable_link")) {
        url <- get_readable_link(url)
      } else {
        stop("`get_readable_link` function is not implemented.")
      }
    }
    
    return(url)
  },

    download_port = function(data_name, predictor = NULL) {
      if (is.null(self$name_id_map)) {
        stop("name_id_map is not initialized.")
      }

      # Get URL
      url <- self$get_url(data_name)

      # Download and process the data
      temp_file <- tempfile()
      download.file(url, temp_file)

      if (grepl("\\.zip$", url)) {
        # Handle ZIP files
        zip_files <- unzip(temp_file, list = TRUE)
        data <- read.csv(unz(temp_file, zip_files$Name[1]))
      } else {
        data <- read.csv(temp_file)
      }

      # Filter data by predictors if provided
      if (!is.null(predictor)) {
        data <- data[data$signalname %in% predictor, ]
      }

      data <- data[order(data$signalname, data$port, data$date), ]
      return(data)
    },

    download_signal = function(predictor = NULL) {
      # Download signal data
      url <- self$get_url("firm_char")
      temp_file <- tempfile()
      download.file(url, temp_file)

      data <- read.csv(temp_file)
      if (!is.null(predictor)) {
        data <- data[data$signalname %in% predictor, ]
      }

      return(data)
    },

    download_signal_doc = function() {
      url <- self$get_url("signal_doc")
      data <- read.csv(url)
      return(data)
    }
  )
)


# Tests (final package)

# Unzip error:
temp_file = tempfile()
download.file("https://drive.google.com/uc?id=1nS0XHY84ZPOqxos7-8JfKvoojTDgoZ3_", temp_file)

unzip(temp_file)







# Examples:
# initialize instance
openap_instance <- OpenAP$new()

# Download keys:
openap_instance$list_port()

# get url dependant on folder/file key
openap_instance$get_url("signal_doc")

# get data
data <- openap_instance$download_port("nyse")
head(data)

# specific predictors:
data2 <- openap_instance$download_port("nyse")
head(data2)

# all signals:
signals <- openap_instance$download_signal()
head(signals)

# specific signals:
signals <- openap_instance$download_signal(predictor = c("Signal1", "Signal2"))























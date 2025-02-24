
list_release <- function(urls) {
  releases <- lapply(names(urls), function(x) {
    # Expect keys of the form "release_<release_id>_url"
    m <- regmatches(x, regexec("^release_(.+)_url$", x))[[1]]
    if (length(m) > 0) {
      release_id <- m[2]  # e.g. "2024_08" or "2023"
      # Try to parse a numeric year and month from the release_id:
      if (grepl("_", release_id)) {
        parts <- strsplit(release_id, "_")[[1]]
        year <- as.numeric(parts[1])
        month <- as.numeric(parts[2])
      } else {
        year <- as.numeric(release_id)
        month <- 0
      }
      return(data.frame(full_release = release_id, year = year, month = month,
                        stringsAsFactors = FALSE))
    }
    NULL
  })
  releases <- do.call(rbind, releases)

  # Sort releases in descending order (latest first)
  releases <- releases[order(-releases$year, -releases$month), ]
  print(releases)
  return(releases)
}

urls <- list(
  release_2024_10_url = "https://drive.google.com/drive/folders/1SSoHGbwgyhRwUCzLE0YWvUlS0DjLCd4k",
  release_2024_08_url = "https://drive.google.com/drive/folders/1-PqsR-tOjv3-U9DRHw85X-VznYlu-Sfc",
  release_2023_url    = "https://drive.google.com/drive/folders/1EP6oEabyZRamveGNyzYU0u6qJ-N43Qfq",
  release_2022_url    = "https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo"
)


library(roxygen2)
library(DBI)
library(dplyr)

#' OpenAP Download
#'
#' @description
#' A package that allows to download data from the Open Source Asset Pricing (OpenAP) directly in R.
#' The package enables users to access two primary types of data:
#'
#' 1. **Predictor Portfolio Returns**:
#'    - Access 212 cross-sectional predictors.
#'    - Download portfolio returns using various portfolio construction methods, including original paper methods, deciles, quintiles, equal-weighted, value-weighted, price filters and more.
#'
#' 2. **Firm Characteristics**:
#'    - Access 209 firm characteristics from OpenAP (+ 3 additional characteristics from CRSP (Price, Size, Short-term Reversal)).
#'
#'Learn more about OpenAP: [Data webside](https://openassetpricing.com) | [GitHub code](https://github.com/OpenSourceAP/CrossSection) | [Publication](https://www.nowpublishers.com/article/Details/CFR-0112)
#'
#' @field name_id_map A mapping between names and their corresponding IDs in the OpenAP database.
#' @field individual_signal_id_map A mapping of unique identifiers for individual signals.
#' @field signal_sign The direction or "sign" of the signal (i.e. positive or negative).
#' @field url The base URL for downloading OpenAP data.
#'
#'
#' @export
OpenAP <- R6::R6Class(
  "OpenAP",
  public = list(
    name_id_map = NULL,
    individual_signal_id_map = NULL,
    signal_sign = NULL,
    url = NULL,

    #' @description
    #' Initializes the OpenAP class instance with data for the specified release year (or per default with the latest data).
    #' Loads mappings, individual signal IDs and signal documentation.
    #'
    #' @param release_year
    #'
    #' @examples
    #' openap_instance <- OpenAP$new(release_year = 2023)
    initialize = function(release_year = NULL) {
      # Get the list of available releases (with identifiers like "2024_08", "2023", etc.)
      releases <- list_release(urls)

      if (is.null(release_year)) {
        # Default to the latest release (first row in the sorted data frame)
        selected <- releases[1, ]
        message("No release specified. Defaulting to latest release: ",
                selected$full_release)
      } else {
        # Allow the user to pass a string like "2024_08" or a numeric value like 2022.
        # If a numeric value is provided, convert it to character.
        if (is.numeric(release_year)) {
          release_year <- as.character(release_year)
        }
        candidate <- releases[releases$full_release == release_year, ]
        if (nrow(candidate) == 0) {
          stop("Invalid release identifier provided. Please use one of: ",
              paste(releases$full_release, collapse = ", "))
        }
        selected <- candidate[1, ]
        message("Selected release: ", selected$full_release)
      }

      # Build the key to extract the URL from the urls list
      release_key <- paste0("release_", selected$full_release, "_url")
      release_url <- urls[[release_key]]

      # Store the selected URL
      self$url <- release_url

      # Extract and process mappings
      mappings <- get_name_id_map(release_url)
      self$name_id_map <- mappings$main

      self$individual_signal_id_map <- data.frame(
        signal = sapply(mappings$signals$signal, function(x) {
          sub(".*class=\\\"flip-entry-title\\\">(.*?)</div>.*", "\\1", x) %>%
            sub("\\.csv$", "", .)
        }),
        file_id = sapply(mappings$signals$file_id, function(x) {
          sub(".*?/d/([a-zA-Z0-9_-]+).*", "\\1", x)
        }),
        stringsAsFactors = FALSE
      )

      # Process the signal documentation
      signal_doc_url <- self$get_url("signal_doc")
      self$signal_sign <- read.csv(signal_doc_url)
    },

    #' @description
    #' Returns a list of available portfolio types for the OpenAP dataset, depending on the specified release year.
    #'
    #' @examples
    #' openap_instance$list_port()
    list_port = function() {
      print(self$name_id_map %>%
              dplyr::filter(stringr::str_starts(name, "Predictor")) %>%
              dplyr::select(name, download_name))
    },

    #' @description
    #' Retrieves the URL for a specific dataset based on its name.
    #'
    #' @param data_name The name of the Portfolio.
    #'
    #' @examples
    #' openap_instance$get_url("nyse")
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
          stop("get_readable_link function is not implemented.")
        }
      }

      return(url)
    },

    #' @description
    #' Downloads portfolio data for a specified data set and optionally filters by predictor.
    #'
    #' @param data_name The name of the data set.
    #'
    #' @param predictor A vector of predictor names to filter (optional).
    #'
    #' @examples
    #' data <- openap_instance$dl_port("deciles_ew", predictor = c("Accruals"))
    dl_port = function(data_name, predictor = NULL) {
        if (is.null(self$name_id_map)) {
            stop("name_id_map is not initialized.")
        }

        # Get URL
        url <- self$get_url(data_name)

        # Download the file to a temporary location
        temp_file <- tempfile()
        download.file(url, temp_file, mode = "wb")  # Ensure binary mode for ZIP files

        # Check file type using magic bytes
        con <- file(temp_file, "rb")
        magic_bytes <- readBin(con, "raw", 4)
        close(con)

        # Detect if the file is a ZIP (magic bytes start with 'PK..')
        if (all(magic_bytes[1:2] == charToRaw("PK"))) {
            # Handle ZIP files using process_zip
            data <- process_zip(temp_file)
        } else {
            # Handle direct CSV files
            data <- tryCatch(
                read.csv(temp_file),
                error = function(e) stop("Error reading CSV file: ", e$message)
            )
        }

        # Filter data by predictors if provided
        if (!is.null(predictor)) {
            data <- data[data$signalname %in% predictor, ]
        }

        # Order the data
        data <- data[order(data$signalname, data$port, data$date), ] %>%
          # make sure data are in tibble format and date correctly formatted
          mutate(date = lubridate::ymd(date)) %>%
          tibble()
        return(data)
    },

    #' @description
    #' Retrieves the URL for an individual signal based on its name.
    #' @param signal_name The name of the signal to retrieve.
    #' @return A string representing the URL of the signal.
    #' @examples
    #' url <- openap_instance$get_individual_signal_url("Accruals.csv")
    get_individual_signal_url = function(signal_name) {
      # Check if individual_signal_id_map exists
      if (is.null(self$individual_signal_id_map)) {
        stop("The individual_signal_id_map is not initialized.")
      }

      # Filter for the specific signal
      signal_entry <- self$individual_signal_id_map %>%
        filter(signal == signal_name)

      if (nrow(signal_entry) == 0) {
        stop(paste("Signal name", signal_name, "not found in individual_signal_id_map."))
      }

      # Extract the file_id
      url <- paste0("https://drive.google.com/uc?id=", signal_entry$file_id[1])
      return(url)
    },

    dl_signal_crsp3 = function() {
      connect <- dbConnect(
        RPostgres::Postgres(),
        host = 'wrds-pgdata.wharton.upenn.edu',
        port = 9737,
        dbname = 'wrds',
        sslmode = 'require',
        user = Sys.getenv("WRDS_USER"),
        pass = Sys.getenv("WRDS_PASS")
      )

      query <- "SELECT PERMNO, DATE, PRC, RET, SHROUT FROM CRSP.MSF"

      crsp_data <- dbGetQuery(connect, query)

      dbDisconnect(connect)

      processed_data <- crsp_data  |>
        mutate(
          yyyymm = as.integer(format(date, "%Y%m")),
          Price = -log(abs(prc)),
          Size = -log(abs(prc * shrout / 1000)),
          STreversal = -coalesce(ret, 0)
          )  |>
        select(permno, yyyymm, Price, Size, STreversal)

      return(processed_data)
    },

    merge_crsp_with_signals = function(signals, crsp_data) {
      merged_data <- signals  |>
      left_join(crsp_data, by = c("permno", "yyyymm"))
    return(merged_data)
    },

    apply_sign_logic = function(data, predictors, signal_sign, signed = TRUE) {
      if (!signed) {
        # Check for CRSP signals and apply transformation
        crsp_signals <- c("Price", "Size", "STreversal")
        for (signal in predictors) {
          if (signal %in% crsp_signals) {
            data[[signal]] <- -data[[signal]]
          } else {
            # Apply the sign from signal_sign
            signal_row <- signal_sign[signal_sign$Acronym == signal, ]
            if (nrow(signal_row) > 0 && !is.na(signal_row$Sign)) {
              data[[signal]] <- data[[signal]] * signal_row$Sign
            }
          }
        }
      }
      return(data)
    },

    #' @description
    #' Downloads specific firm characteristics.
    #'
    #' @param predictor A vector of predictor names to download.
    #'
    #' @param signed Logical; whether to apply signed transformation based on signal documentation.
    #'
    #' @return A data frame containing the signal data.
    #' @examples
    #' signals <- openap_instance$dl_signal(predictor = c("BM"))
    dl_signal = function(predictor = NULL, signed = TRUE) {
      if (is.null(predictor)) {
        stop("Predictor(s) must be specified.")
      }

      crsp_signals <- c("Price", "Size", "STreversal")
      all_predictors <- unique(c(crsp_signals, predictor))
      result <- data.frame()

      for (signal_name in all_predictors) {
        if (signal_name %in% crsp_signals) {
          next
        }

        url <- self$get_individual_signal_url(signal_name)
        if (is.null(url)) {
          stop(paste("Could not retrieve URL for signal:", signal_name))
        }

        temp_file <- tempfile()
        download.file(url, temp_file, mode = "wb")
        signal_data <- tryCatch(
          read.csv(temp_file),
          error = function(e) stop(paste("Error reading data for signal:", signal_name, "-", e$message))
        )

        if (nrow(result) == 0) {
          result <- signal_data
        } else {
          result <- merge(result, signal_data, all = TRUE)
        }
      }

      crsp_data <- self$dl_signal_crsp3()
      signals <- self$merge_crsp_with_signals(result, crsp_data)
      signals <- self$apply_sign_logic(signals, all_predictors, self$signal_sign, signed)

      return(signals)
    },

    #' @description
    #' Downloads all firm level characteristics from the release folder.
    #'
    #' @return A data frame containing all firm level characteristics.
    #'
    #' @examples
    #' signals_data <- openap_instance$dl_all_signals()
    dl_all_signals = function(signed = FALSE) {
      url <- self$get_url("firm_char")
      temp_file <- tempfile()
      download.file(url, temp_file, mode = "wb")

      con <- file(temp_file, "rb")
      magic_bytes <- readBin(con, "raw", 4)
      close(con)

      if (all(magic_bytes[1:2] == charToRaw("PK"))) {
        data <- process_zip(temp_file)
      } else {
        data <- tryCatch(
          read.csv(temp_file),
          error = function(e) stop("Error reading 'firm_char' dataset:", e$message)
        )
      }

      crsp_data <- self$dl_signal_crsp3()
      all_signals <- self$merge_crsp_with_signals(data, crsp_data)
      all_predictors <- unique(c("Price", "Size", "STreversal", colnames(data)))
      all_signals <- self$apply_sign_logic(all_signals, all_predictors, self$signal_sign, signed)

      return(all_signals)
    },

    #' @description
    #' Downloads the signal documentation CSV for the release.
    #' @return A data frame containing the signal documentation.
    #' @examples
    #' signal_doc <- openap_instance$dl_signal_doc()
    dl_signal_doc = function() {
      url <- self$get_url("signal_doc")
      data <- read.csv(url)
      return(data)
    }

  )
)


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
      return(data.frame(release_id = release_id, year = year, month = month,
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
  release_2025_10_url = "https://drive.google.com/drive/folders/1qQDuTsnyvWfEJR6nPBQZ8xxlq6bkLG_y",
  release_2024_10_url = "https://drive.google.com/drive/folders/1SSoHGbwgyhRwUCzLE0YWvUlS0DjLCd4k",
  release_2024_08_url = "https://drive.google.com/drive/folders/1-PqsR-tOjv3-U9DRHw85X-VznYlu-Sfc",
  release_2023_url    = "https://drive.google.com/drive/folders/1EP6oEabyZRamveGNyzYU0u6qJ-N43Qfq",
  release_2022_url    = "https://drive.google.com/drive/folders/1O18scg9iBTiBaDiQFhoGxdn4FdsbMqGo"
)


#' OpenAP Download
#'
#' @description
#' Provides access to data from the Open Source Asset Pricing (OpenAP) project directly in R.
#' The package enables users to access two primary types of data:
#'
#' 1. **Predictor Portfolio Returns**:
#'    - Access 212 cross-sectional predictors.
#'    - Download portfolio returns using various portfolio construction methods, including original paper methods, deciles, quintiles, equal-weighted, value-weighted, price filters and more.
#'
#' 2. **Firm Characteristics**:
#'    - Access 209 firm characteristics from OpenAP (+ 3 additional characteristics from CRSP (Price, Size, Short-term Reversal)).
#'
#'Learn more about OpenAP: [Data website](https://www.openassetpricing.com) | [GitHub code](https://github.com/OpenSourceAP/CrossSection) | [Publication](https://www.nowpublishers.com/article/Details/CFR-0112)
#'
#' @field name_id_map A mapping between names and their corresponding IDs in the OpenAP database.
#' @field individual_signal_id_map A mapping of unique identifiers for individual signals.
#' @field signal_sign The direction or "sign" of the signal (i.e. positive or negative).
#' @field url The base URL for downloading OpenAP data.
#' @field mock Logical; if TRUE, network calls are skipped and mock data is returned for all download functions
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
    mock = FALSE,

    #' @description
    #' Initializes the OpenAP class instance with data for the specified release year (or per default with the latest data).
    #' Loads mappings, individual signal IDs and signal documentation.
    #'
    #' @param release_year Optional release identifier, e.g. `"2024_08"`.
    #' @param mock Logical; if TRUE, network initialization is skipped and
    #'   small mock datasets are returned instead of downloading data.
    #'   This is intended for examples, testing, and offline use.
    #' 
    #' @examples
    #' # Offline example
    #' obj <- OpenAP$new(mock = TRUE)
    #'
    #' # Real initialization (requires internet connection)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   openap_24 <- OpenAP$new(release_year = "2024_10")
    #'}
    
    initialize = function(release_year = NULL, mock = FALSE) {

      # Store mock flag
      self$mock <- mock

      # If we're in mock mode, skip all network work
      if (isTRUE(self$mock)) {
        message("Mock mode active - skipping network initialization")

        # Minimal safe defaults
        self$url <- "mock"
        self$name_id_map <- data.frame()
        self$individual_signal_id_map <- data.frame()
        self$signal_sign <- data.frame()

        return(invisible(self))
      }

      # Get the list of available releases 
      releases <- list_release(urls)

      if (is.null(release_year)) {
        # Default to the latest release (first row in the sorted data frame)
        selected <- releases[1, ]
        message("No release specified. Defaulting to latest release: ",
                selected$release_id)
      } else {
        if (is.numeric(release_year)) {
          release_year <- as.character(release_year)
        }
        candidate <- releases[releases$release_id == release_year, ]
        if (nrow(candidate) == 0) {
          stop("Invalid release identifier provided. Please use one of: ",
              paste(releases$release_id, collapse = ", "))
        }
        selected <- candidate[1, ]
        message("Selected release: ", selected$release_id)
      }

      # Build key to extract the URL from the urls list
      release_key <- paste0("release_", selected$release_id, "_url")
      release_url <- urls[[release_key]]

      self$url <- release_url

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

      # Process signal documentation
      signal_doc_url <- self$get_url("signal_doc")
      self$signal_sign <- read.csv(signal_doc_url)
    },

    #' @description
    #' Returns a list of available portfolio types for the OpenAP dataset, depending on the specified release year.
    #'
    #' @examples
    #' # List available portfolios  (offline example)
    #' openap <- OpenAP$new(mock = TRUE) 
    #' openap$list_port()
    #' 
    #' # Real usage (internet connection required)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   openap$list_port()
    #' }
    list_port = function() {
      if (isTRUE(self$mock)) {
        return(data.frame(
          name = c("Predictor_1"),
          download_name = c("mock_port"),
          stringsAsFactors = FALSE
        ))
      }

      print(self$name_id_map %>%
              dplyr::filter(stringr::str_starts(name, "Predictor")) %>%
              dplyr::select(name, download_name))
    },

    #' @description
    #' Retrieves the URL for a specific dataset based on its name.
    #'
    #' @param data_name The name of the Portfolio.
    #'
    #' @keywords internal
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
    #' # Download entire portfolio file (offline example)
    #' openap <- OpenAP$new(mock = TRUE)
    #' df <- openap$dl_port("op")
    #' # Download with specific predictor(s) only
    #' df <- openap$dl_port("op", predictor = "Accruals")
    #' 
    #' # Real usage (internet connection required)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   df <- openap$dl_port("op", predictor = "Accruals")
    #'   df <- openap$dl_port("op", predictor = c("Accruals", "Mom12m"))
    #' }
    dl_port = function(data_name, predictor = NULL) {
        if (isTRUE(self$mock)) {

        # Create small mock dataset similar to real OpenAP portfolios
        df <- data.frame(
          signalname = rep(c("BM", "Accruals", "Mom12m"), each = 2),
          port       = rep(c(1, 5), times = 3),
          date       = as.Date(rep(c("2020-01-01", "2020-02-01"), times = 3)),
          ret        = c(0.01, 0.02,  0.03, 0.01,  -0.01, 0.005),
          signallag  = c(-0.001, -0.001, -0.001,  -0.001, -0.001, -0.001),
          Nlong      = c(135, 135, 135, 135, 135, 135),
          Nshort     = c(1,1,1,1,1,1),
          stringsAsFactors = FALSE
        )

        # Apply predictor filtering
        if (!is.null(predictor)) {
          df <- df[df$signalname %in% predictor, ]
        }

        # Guarantee valid output for tests
        df <- dplyr::tibble(df)
        return(df)
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
                utils::read.csv(temp_file),
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
          dplyr::mutate(date = lubridate::ymd(date)) %>%
          dplyr::tibble()
        return(data)
    },

    #' @description
    #' Retrieves the URL for an individual signal based on its name.
    #' @param signal_name The name of the signal to retrieve.
    #' @return A string representing the URL of the signal.
    #' 
    #' @keywords internal
    get_individual_signal_url = function(signal_name) {
      # Check if individual_signal_id_map exists
      if (is.null(self$individual_signal_id_map)) {
        stop("The individual_signal_id_map is not initialized.")
      }

      # Filter for the specific signal
      signal_entry <- self$individual_signal_id_map %>%
        dplyr::filter(signal == signal_name)

      if (nrow(signal_entry) == 0) {
        stop(paste("Signal name", signal_name, "not found in individual_signal_id_map."))
      }

      # Extract the file_id
      url <- paste0("https://drive.google.com/uc?id=", signal_entry$file_id[1])
      return(url)
    },

    #' @description
    #' Is being used in "dl_signal()" to download CRSP signals. Requires WRDS credentials.
    #' @param requested_crsp_signals A vector of CRSP signals to download.
    #' 
    #' @keywords internal
    dl_signal_crsp3 = function(requested_crsp_signals = c("Price", "Size", "STreversal")) {

      # Ensure requested_crsp_signals is always assigned the default if NULL
      if (is.null(requested_crsp_signals)) {
        requested_crsp_signals <- c("Price", "Size", "STreversal")
      }

      # Checking whether wrds access is saved in environment variables, if not prompt:
      if (Sys.getenv("WRDS_USER") == '' | Sys.getenv("WRDS_PASS") == '') {
        message("WRDS credentials not found. To avoid entering them each time, please save them as environment variables WRDS_USER and WRDS_PASS.
                Still, you might have to log in to the website first, to go through 2FA once.")
        user = getPass::getPass("Enter your WRDS username:")
        pass = getPass::getPass("Enter your WRDS password:")
      } else {
        user = Sys.getenv("WRDS_USER")
        pass = Sys.getenv("WRDS_PASS")
      }

      connect <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = 'wrds-pgdata.wharton.upenn.edu',
        port = 9737,
        dbname = 'wrds',
        sslmode = 'require',
        user = user,
        pass = pass
      )

      query <- "SELECT PERMNO, DATE, PRC, RET, SHROUT FROM CRSP.MSF"
      crsp_data <- DBI::dbGetQuery(connect, query)
      DBI::dbDisconnect(connect)

      processed_data <- crsp_data  %>%
        dplyr::mutate(
          yyyymm = as.integer(format(date, "%Y%m")),
          Price = -log(abs(prc)),
          Size = -log(abs(prc * shrout / 1000)),
          STreversal = -coalesce(ret, 0)
        ) |>
      dplyr::select(permno, yyyymm, all_of(requested_crsp_signals)) %>%
        dplyr::tibble()

      return(processed_data)
    },

    #' @description
    #' applies the sign logic to the data based on the signal documentation.
    #'
    #' @param data The data frame to apply the sign logic to.
    #' @param predictors A vector of predictor names.
    #' @param signal_sign A data frame containing the signal documentation.
    #' @param signed Logical; whether to apply signed transformation based on signal documentation. 
    #'   Default is TRUE.
    #' @keywords internal
    apply_sign_logic = function(data, predictors, signal_sign, signed = TRUE) {
      if (signed) {
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
    #' Merges downloaded OpenAP firm-level signals with CRSP-based signals 
    #' (e.g., Size, Price, STreversal).
    #'
    #' @param signals Data frame containing OpenAP firm-level signals with columns 
    #'   \code{permno} and \code{yyyymm}.
    #' @param crsp_data Data frame containing CRSP signals (e.g., Size, Price) with columns 
    #'   \code{permno} and \code{yyyymm}.
    #' @keywords internal
    merge_crsp_with_signals = function(signals, crsp_data) {
      merged_data <- dplyr::left_join(signals, crsp_data, by = c("permno", "yyyymm"))
      return(merged_data)
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
    #' # Download firm characteristics (offline example)
    #' openap <- OpenAP$new(mock = TRUE)
    #' 
    #' # Single signal (OpenAP or WRDS)
    #' df <- openap$dl_signal("BM")
    #'
    #' # Multiple signals
    #' df <- openap$dl_signal(c("BM", "STreversal"))
    #'
    #' # Real usage (requires internet connection)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   df <- openap$dl_signal("BM")
    #'   df <- openap$dl_signal(c("BM", "STreversal"))
    #'   df <- openap$dl_signal("BM", signed = FALSE)
    #' }
    dl_signal = function(predictor = NULL, signed = FALSE) {

      if (is.null(predictor) || identical(predictor, character(0))) {
      stop("Predictor(s) must be specified.")
      }

      # mock mode
      if (isTRUE(self$mock)) {
        df <- data.frame(
          permno = c(1,2),
          yyyymm = c(202001, 202002)
        )
        for (p in predictor) {
          df[[p]] <- c(0.5, -0.4)
        }
        return(df)
      }

      crsp_signals <- c("Price", "Size", "STreversal")

      # distinction between crsp and openap signals
      requested_crsp_signals <- intersect(crsp_signals, predictor)
      requested_openap_signals <- setdiff(predictor, crsp_signals)

      result <- data.frame()

      # Download OpenAP signals (only)
      if (length(requested_openap_signals) > 0) {
        for (signal_name in requested_openap_signals) {
          url <- self$get_individual_signal_url(signal_name)
          if (is.null(url)) {
            stop(paste("Could not retrieve URL for signal:", signal_name))
          }

          temp_file <- tempfile()
          utils::download.file(url, temp_file, mode = "wb")
          signal_data <- tryCatch(
            utils::read.csv(temp_file),
            error = function(e) stop(paste("Error reading data for signal:", signal_name, "-", e$message))
          )

          # Merge OpenAP data into result
          if (nrow(result) == 0) {
            result <- signal_data
          } else {
            result <- merge(result, signal_data, all = TRUE)
          }
        }
      }

      # Download CRSP signals
      if (length(requested_crsp_signals) > 0) {
        if (nrow(result) == 0) {
          # If no OpenAP signals are available, download only CRSP signals
          crsp_data <- self$dl_signal_crsp3(requested_crsp_signals)
          result <- crsp_data
        } else {
          # If OpenAP signals exist: merge
          crsp_data <- self$dl_signal_crsp3(requested_crsp_signals)
          result <- self$merge_crsp_with_signals(result, crsp_data)
        }
      }

      # Signing logic
      if (nrow(result) > 0) {
        all_signals <- unique(c(requested_openap_signals, requested_crsp_signals))
        result <- self$apply_sign_logic(result, all_signals, self$signal_sign, signed)
      }

      return(result)
    },

    #' @description
    #' Downloads all firm level characteristics from the release folder.
    #' @param signed Logical; whether to apply signed transformation based on signal documentation. Default is TRUE.
    #'
    #' @return A data frame containing all firm level characteristics.
    #'
    #' @examples
    #' # Download all signals (offline example)
    #' openap <- OpenAP$new(mock = TRUE)
    #' signals_data <- openap$dl_all_signals()
    #'
    #' # Real usage (requires internet connection)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   signals_data <- openap$dl_all_signals()
    #' }
    dl_all_signals = function(signed = FALSE) {
      
      if (isTRUE(self$mock)) {
        return(data.frame(
          permno = c(1L, 2L),
          yyyymm = c(202001L, 202002L),
          Price = c(5, 7),        # WRDS example
          Size = c(10, 20),       # WRDS example
          BM = c(0.1, -0.2),      # OpenAP example
          Accruals = c(0.03, 0.01), 
          OtherSignal = c(0.1, 0.1)
        ))
      }
      
      url <- self$get_url("firm_char")
      temp_file <- tempfile()

      withr::with_options(
        new = list(timeout = 600),
        code = {
          utils::download.file(url, temp_file, mode = "wb")
        }
      )

      con <- file(temp_file, "rb")
      magic_bytes <- readBin(con, "raw", 4)
      close(con)

      if (all(magic_bytes[1:2] == charToRaw("PK"))) {
        data <- process_zip(temp_file)
      } else {
        data <- tryCatch(
          utils::read.csv(temp_file),
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
    #' # Load signal documentation (offline example)
    #' openap <- OpenAP$new(mock = TRUE)
    #' signal_doc <- openap$dl_signal_doc()
    #' # Real usage (requires internet connection)
    #' \dontrun{
    #'   openap <- OpenAP$new()
    #'   signal_doc <- openap$dl_signal_doc()
    #' }
    dl_signal_doc = function() {

      if (isTRUE(self$mock)) {
        return(
        readRDS(system.file("extdata", "signal_doc_mock.rds",
                          package = "OpenSourceAP.DownloadR"))
        )
      }

      url <- self$get_url("signal_doc")
      data <- utils::read.csv(url)
      return(data)
    }

  )
)

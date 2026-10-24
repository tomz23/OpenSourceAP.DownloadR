
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
#' Data are read from the OpenAP Hugging Face dataset
#' (<https://huggingface.co/datasets/OpenSourceAP/CrossSection>), where each data release
#' is a branch. Releases 2020.05 through 2025.10 are available; 2025.10 is the default.
#'
#'Learn more about OpenAP: [Data website](https://www.openassetpricing.com) | [GitHub code](https://github.com/OpenSourceAP/CrossSection) | Publication: \doi{10.1561/104.00000112}
#'
#' @field name_id_map A mapping between dataset names and their download URLs for the selected release.
#' @field individual_signal_id_map Kept for backward compatibility. Signals are now read
#'   from the release's firm characteristics file, so this is an empty data frame.
#' @field signal_sign The signal documentation (`SignalDoc.csv`) of the release, including the
#'   sign of each signal. Empty for releases without `SignalDoc.csv` (2021.04 and earlier).
#' @field url The Hugging Face URL of the selected release.
#' @field release_id The selected release, e.g. `"2025_10"`.
#' @field hf_revision The Hugging Face branch of the selected release, e.g. `"2025.10"`.
#' @field mock Logical; if TRUE, network calls are skipped and mock data is returned for all download functions
#'
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr tibble
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @importFrom getPass getPass
#' @export
OpenAP <- R6::R6Class(
  "OpenAP",
  public = list(
    name_id_map = NULL,
    individual_signal_id_map = NULL,
    signal_sign = NULL,
    url = NULL,
    release_id = NULL,
    hf_revision = NULL,
    mock = FALSE,

    #' @description
    #' Initializes the OpenAP class instance with data for the specified release (or per default with the 2025.10 release).
    #' Loads the release's file list and signal documentation.
    #'
    #' @param release_year Optional release identifier, e.g. `"2024_08"`. Accepts `"2024.08"` too.
    #'   `"2023"` and `"2022"` are the 2023.08 and 2022.03 releases. `"2026_10"` is test data,
    #'   not for research use.
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
      list_release()

      if (is.null(release_year)) {
        selected <- resolve_release(default_release)
        message("No release specified. Defaulting to release: ",
                selected$release_id)
      } else {
        selected <- resolve_release(release_year)
        message("Selected release: ", selected$release_id)
      }
      if (selected$note != "") {
        warning("Release ", selected$release_id, ": ", selected$note, call. = FALSE)
      }

      self$release_id <- selected$release_id
      self$hf_revision <- selected$hf_revision
      self$url <- paste0("https://huggingface.co/datasets/", hf_repo, "/tree/",
                         utils::URLencode(selected$hf_revision, reserved = TRUE))

      self$name_id_map <- get_name_id_map(selected$hf_revision)
      self$individual_signal_id_map <- data.frame()

      # Process signal documentation
      if ("signal_doc" %in% self$name_id_map$download_name) {
        self$signal_sign <- self$dl_signal_doc()
      } else {
        message("Release ", selected$release_id, " has no SignalDoc.csv.")
        self$signal_sign <- data.frame()
      }
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

      ports <- self$name_id_map[self$name_id_map$type == "port", c("name", "download_name", "full_name")]
      rownames(ports) <- NULL
      print(ports)
    },

    #' @description
    #' Retrieves the URL for a specific dataset based on its name.
    #'
    #' @param data_name The name of the Portfolio.
    #'
    #' @keywords internal
    get_url = function(data_name) {
      urls <- self$get_urls(data_name)
      return(urls[1])
    },

    #' @description
    #' Retrieves the URLs for a dataset. Unsigned firm characteristics are two files
    #' in the 2020 releases; every other dataset is one file.
    #'
    #' @param data_name The name of the dataset.
    #'
    #' @keywords internal
    get_urls = function(data_name) {
      data_entry <- self$name_id_map[self$name_id_map$download_name == data_name, ]

      if (nrow(data_entry) == 0) {
        stop("Dataset '", data_name, "' is not available in release ", self$release_id,
             ". Available: ", paste(unique(self$name_id_map$download_name), collapse = ", "))
      }

      return(data_entry$file_id)
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

        url <- self$get_url(data_name)
        if (!data_name %in% self$name_id_map$download_name[self$name_id_map$type == "port"]) {
          stop("'", data_name, "' is not a portfolio dataset. See list_port().")
        }
        data <- read_parquet_url(url)
        # the 2020 files kept an unnamed row number column
        data <- data[, names(data) != "", drop = FALSE]

        # Filter data by predictors if provided
        if (!is.null(predictor)) {
            missing <- setdiff(predictor, data$signalname)
            if (length(missing) > 0) {
              warning("Predictors not available: ", paste(missing, collapse = ", "), call. = FALSE)
            }
            data <- data[data$signalname %in% predictor, ]
        }

        # Order the data
        keys <- intersect(c("signalname", "port", "date"), names(data))
        data <- data[do.call(order, unname(as.list(data[keys]))), ]
        return(dplyr::tibble(data))
    },

    #' @description
    #' Is being used in "dl_signal()" to download CRSP signals. Requires WRDS credentials.
    #' Reads the CRSP CIZ monthly stock file (`crsp.msf_v2`).
    #' @param requested_crsp_signals A vector of CRSP signals to download.
    #' @param signed Logical; if TRUE (default), signals are signed (`Price = -log(|prc|)` etc.).
    #'
    #' @keywords internal
    dl_signal_crsp3 = function(requested_crsp_signals = c("Price", "Size", "STreversal"), signed = TRUE) {

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

      # CIZ replaces the legacy monthly file (last updated through 2024).
      # shrout is in thousands; mthret uses CIZ compounding and delisting
      # treatment, so historical STreversal can differ from older releases.
      query <- "SELECT permno, mthcaldt AS date, mthprc AS prc, mthret AS ret, shrout FROM crsp.msf_v2"
      crsp_data <- DBI::dbGetQuery(connect, query)
      DBI::dbDisconnect(connect)

      sign <- if (signed) -1 else 1
      processed_data <- data.frame(
        permno = as.integer(crsp_data$permno),
        yyyymm = as.integer(format(crsp_data$date, "%Y%m")),
        Price = sign * log(abs(crsp_data$prc)),
        Size = sign * log(abs(crsp_data$prc * crsp_data$shrout / 1000)),
        STreversal = sign * dplyr::coalesce(crsp_data$ret, 0)
      )
      processed_data$Size[!is.finite(processed_data$Size)] <- NA

      return(dplyr::tibble(processed_data[, c("permno", "yyyymm", requested_crsp_signals)]))
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
    #' Downloads specific firm characteristics. Only the requested columns of the
    #' release's firm characteristics file are downloaded.
    #'
    #' @param predictor A vector of predictor names to download.
    #'
    #' @param signed Logical; if TRUE, signals are signed so that high values predict high
    #'   returns (the release's `signed_predictors_dl_wide` file). If FALSE (default), signals
    #'   are as constructed (`Predictors`). CRSP signals follow the same choice.
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
    #'   df <- openap$dl_signal("BM", signed = TRUE)
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

      result <- NULL

      # Download OpenAP signals (only their columns)
      if (length(requested_openap_signals) > 0) {
        files <- lapply(self$signal_urls(signed), open_remote_parquet)
        on.exit(unlink(vapply(files, function(x) x$file, character(1))))

        available <- unlist(lapply(files, function(x) x$columns))
        missing <- setdiff(requested_openap_signals, available)
        if (length(missing) > 0) {
          stop("Predictors not available in release ", self$release_id, ": ",
               paste(missing, collapse = ", "))
        }

        for (file in files) {
          signals <- intersect(requested_openap_signals, file$columns)
          if (length(signals) == 0) next
          signal_data <- read_remote_columns(file, c(intersect(signal_key_columns, file$columns), signals))
          signal_data <- standardize_signal_keys(signal_data)
          if (is.null(result)) {
            result <- signal_data
          } else {
            result <- dplyr::full_join(result, signal_data, by = c("permno", "yyyymm"))
          }
        }
      }

      # Download CRSP signals
      if (length(requested_crsp_signals) > 0) {
        crsp_data <- self$dl_signal_crsp3(requested_crsp_signals, signed = signed)
        if (is.null(result)) {
          result <- crsp_data
        } else {
          result <- self$merge_crsp_with_signals(result, crsp_data)
        }
      }

      # Keep rows with at least one requested signal
      result <- as.data.frame(result)[, c("permno", "yyyymm", predictor)]
      result <- result[rowSums(!is.na(result[predictor])) > 0, ]
      result <- result[order(result$permno, result$yyyymm), ]
      return(dplyr::tibble(result))
    },

    #' @description
    #' Retrieves the URLs of the firm characteristics file(s) of the release.
    #' @param signed Logical; signed or unsigned characteristics.
    #'
    #' @keywords internal
    signal_urls = function(signed) {
      data_name <- if (signed) "firm_char" else "firm_char_unsigned"
      if (!data_name %in% self$name_id_map$download_name) {
        stop("Release ", self$release_id, " has no ", if (signed) "signed" else "unsigned",
             " firm characteristics. Use signed = ", !signed, ".")
      }
      return(self$get_urls(data_name))
    },

    #' @description
    #' Downloads all firm level characteristics from the release folder.
    #' @param signed Logical; whether signals are signed so that high values predict high
    #'   returns. Default is TRUE, the release's `signed_predictors_dl_wide` file.
    #'   The 2020 releases have unsigned signals only.
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
    dl_all_signals = function(signed = TRUE) {

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

      data <- NULL
      for (url in self$signal_urls(signed)) {
        file_data <- standardize_signal_keys(read_parquet_url(url))
        if (is.null(data)) {
          data <- file_data
        } else {
          data <- dplyr::full_join(data, file_data, by = c("permno", "yyyymm"))
        }
      }

      crsp_data <- self$dl_signal_crsp3(signed = signed)
      all_signals <- self$merge_crsp_with_signals(data, crsp_data)

      return(dplyr::tibble(all_signals))
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
      text <- httr::content(hf_get(url), as = "text", encoding = "UTF-8")
      data <- utils::read.csv(text = text)
      return(data)
    }

  )
)

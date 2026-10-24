# Data releases are branches of a public Hugging Face dataset repo, read
# without login or token. Inputs: the branch file listing (HF API) and the
# files themselves (resolve URLs). Outputs: data frames.

hf_repo <- "OpenSourceAP/CrossSection"
hf_resolve_prefix <- paste0("https://huggingface.co/datasets/", hf_repo, "/resolve/")
hf_tree_prefix <- paste0("https://huggingface.co/api/datasets/", hf_repo, "/tree/")


####################
# Releases
####################

# release_id is what users pass to OpenAP$new(); hf_revision is the branch.
# "2023" and "2022" are the ids the Google Drive versions used.
releases <- data.frame(
  release_id = c("2026_10", "2025_10", "2024_10", "2024_08", "2023", "2022_04",
                 "2022", "2021_04", "2021_03", "2020_07", "2020_05"),
  hf_revision = c("Test-Data-2026.10", "2025.10", "2024.10", "2024.08", "2023.08",
                  "2022.04", "2022.03", "2021.04", "2021.03", "2020.07", "2020.05"),
  note = c("TEST DATA ONLY: mixed vintage, not for research use",
           rep("", 10)),
  stringsAsFactors = FALSE
)

default_release <- "2025_10"

# other spellings of release ids, e.g. "2023.08" or 2023
release_aliases <- c("2023_08" = "2023", "2022_03" = "2022")

list_release <- function() {
  ym <- regmatches(releases$hf_revision, regexpr("[0-9]{4}\\.[0-9]{2}", releases$hf_revision))
  out <- data.frame(
    release_id = releases$release_id,
    year = as.numeric(substr(ym, 1, 4)),
    month = as.numeric(substr(ym, 6, 7)),
    hf_revision = releases$hf_revision,
    default = releases$release_id == default_release,
    note = releases$note,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$year, -out$month), ]
  rownames(out) <- NULL
  print(out)
  return(out)
}

resolve_release <- function(release_year) {
  id <- gsub("[.-]", "_", as.character(release_year))
  if (id %in% names(release_aliases)) {
    id <- release_aliases[[id]]
  }
  if (!id %in% releases$release_id) {
    stop("Invalid release identifier provided. Please use one of: ",
         paste(releases$release_id, collapse = ", "))
  }
  releases[releases$release_id == id, ]
}


####################
# HTTP
####################

# GET with a range ("0-99", "-8") returns only those bytes
hf_get <- function(url, range = NULL, ...) {
  config <- list(httr::user_agent("OpenSourceAP.DownloadR"), ...)
  if (!is.null(range)) {
    config <- c(config, list(httr::add_headers(Range = paste0("bytes=", range))))
  }
  response <- do.call(httr::GET, c(list(url), config))
  if (httr::http_error(response)) {
    stop("Download failed (HTTP ", httr::status_code(response), "): ", url)
  }
  if (!is.null(range) && httr::status_code(response) != 206) {
    stop("Server ignored the byte range request: ", url)
  }
  return(response)
}

hf_url <- function(revision, path) {
  paste0(hf_resolve_prefix, utils::URLencode(revision, reserved = TRUE), "/",
         utils::URLencode(path))
}

# All file paths on a release branch (a release has well under the
# 1000 entries per page of the tree API)
hf_list_files <- function(revision) {
  url <- paste0(hf_tree_prefix, utils::URLencode(revision, reserved = TRUE),
                "?recursive=true")
  tree <- jsonlite::fromJSON(httr::content(hf_get(url), as = "text", encoding = "UTF-8"))
  return(tree$path[tree$type == "file"])
}


####################
# File map
####################

# download_name: pattern for its path. Release layouts differ (e.g. no daily
# portfolios before 2021.04, Full_Sets_Main/ in 2021.03, signalbase/
# signaladditional in 2020), so a dataset exists only if its pattern matches
# a file on the branch.
hf_datasets <- c(
  signal_doc = "^SignalDoc\\.csv$",
  firm_char = "^Firm Level Characteristics/signed_predictors_dl_wide\\.parquet$",
  firm_char_unsigned = "^(Firm Level Characteristics/Predictors|signalbase|signaladditional)\\.parquet$",
  # Monthly portfolios
  op = "^Portfolios/(Full Sets OP|Full_Sets_Main)/PredictorPortsFull\\.parquet$",
  placebo_op = "^Portfolios/(Full Sets OP|Full_Sets_Extended)/PlaceboPortsFull\\.parquet$",
  deciles_ew = "/PredictorAltPorts_Deciles\\.parquet$",
  deciles_vw = "/PredictorAltPorts_DecilesVW\\.parquet$",
  ex_nyse_p20_me = "/PredictorAltPorts_LiqScreen_ME_gt_NYSE20pct\\.parquet$",
  nyse = "/PredictorAltPorts_LiqScreen_NYSEonly\\.parquet$",
  ex_price5 = "/PredictorAltPorts_LiqScreen_Price_gt_5\\.parquet$",
  vw_force = "/PredictorAltPorts_LiqScreen_VWforce\\.parquet$",
  quintiles_ew = "/PredictorAltPorts_Quintiles\\.parquet$",
  quintiles_vw = "/PredictorAltPorts_QuintilesVW\\.parquet$",
  ff93 = "/PredictorAltPorts_FF93style\\.parquet$",
  holdper1 = "/PredictorAltPorts_HoldPer_1\\.parquet$",
  holdper3 = "/PredictorAltPorts_HoldPer_3\\.parquet$",
  holdper6 = "/PredictorAltPorts_HoldPer_6\\.parquet$",
  holdper12 = "/PredictorAltPorts_HoldPer_12\\.parquet$",
  # Daily portfolios
  daily_op_ew = "^DailyPortfolios/Predictor\\.parquet$",
  daily_op_vw = "^DailyPortfolios/PredictorVW\\.parquet$",
  daily_deciles_ew = "^DailyPortfolios/CtsPredictorDecile\\.parquet$",
  daily_deciles_vw = "^DailyPortfolios/CtsPredictorDecileVW\\.parquet$",
  daily_quintiles_ew = "^DailyPortfolios/CtsPredictorQuintile\\.parquet$",
  daily_quintiles_vw = "^DailyPortfolios/CtsPredictorQuintileVW\\.parquet$"
)

# Map from a branch's file paths: name, full_name (path), file_id (URL),
# download_name, type ("doc", "firm" or "port"). Portfolio files without a
# download_name above (e.g. 2020's portbase, 2021.03's
# CheckPredictorPorts_Deciles) are listed under their file stem.
map_hf_files <- function(paths, revision) {
  download_name <- rep(NA_character_, length(paths))
  for (i in seq_along(hf_datasets)) {
    hit <- is.na(download_name) & grepl(hf_datasets[[i]], paths)
    download_name[hit] <- names(hf_datasets)[i]
  }

  name <- basename(paths)
  other_port <- is.na(download_name) &
    grepl("\\.parquet$", paths) &
    !grepl("^(Results|Firm Level Characteristics)/", paths) &
    !grepl("^SignalDoc|LSretWide", name) &
    !name %in% name[!is.na(download_name)]
  download_name[other_port] <- sub("\\.parquet$", "", name[other_port])

  keep <- !is.na(download_name)
  df <- data.frame(
    name = name[keep],
    full_name = paths[keep],
    file_id = vapply(paths[keep], function(p) hf_url(revision, p), character(1), USE.NAMES = FALSE),
    download_name = download_name[keep],
    stringsAsFactors = FALSE
  )
  df$type <- ifelse(df$download_name == "signal_doc", "doc",
                    ifelse(df$download_name %in% c("firm_char", "firm_char_unsigned"), "firm", "port"))
  # firm_char_unsigned is two files in 2020; every other name is one file
  df <- df[df$type == "firm" | !duplicated(df$download_name), ]
  rownames(df) <- NULL
  return(df)
}

get_name_id_map <- function(revision) {
  map_hf_files(hf_list_files(revision), revision)
}


####################
# Parquet
####################

read_parquet_url <- function(url) {
  file <- tempfile(fileext = ".parquet")
  on.exit(unlink(file))
  progress <- if (interactive()) httr::progress() else NULL
  hf_get(url, NULL, httr::write_disk(file, overwrite = TRUE), progress)
  return(as.data.frame(nanoparquet::read_parquet(file)))
}

# Remote parquet file: the footer is fetched into a sparse local file at
# its real offset, so nanoparquet can read the metadata. Column chunks are
# fetched later, only for the columns read.
open_remote_parquet <- function(url) {
  tail8 <- hf_get(url, range = "-8")
  size <- as.numeric(sub(".*/", "", httr::headers(tail8)[["content-range"]]))
  footer_len <- readBin(httr::content(tail8, as = "raw")[1:4], "integer", size = 4, endian = "little")
  footer <- httr::content(hf_get(url, range = paste0("-", footer_len + 8)), as = "raw")

  file <- tempfile(fileext = ".parquet")
  con <- file(file, "w+b")
  writeBin(charToRaw("PAR1"), con)
  seek(con, size - footer_len - 8, rw = "write")
  writeBin(footer, con)
  close(con)

  chunks <- nanoparquet::read_parquet_metadata(file)$column_chunks
  chunks$column <- vapply(chunks$path_in_schema, paste, character(1), collapse = ".")
  return(list(url = url, file = file, chunks = chunks,
              columns = unique(chunks$column)))
}

read_remote_columns <- function(remote, columns) {
  chunks <- remote$chunks[remote$chunks$column %in% columns, ]
  start <- pmin(chunks$data_page_offset, chunks$dictionary_page_offset, na.rm = TRUE)
  end <- start + chunks$total_compressed_size - 1
  ord <- order(start)
  start <- start[ord]
  end <- end[ord]

  # merge adjacent chunks into one request
  group <- cumsum(c(TRUE, start[-1] != end[-length(end)] + 1))
  start <- tapply(start, group, min)
  end <- tapply(end, group, max)

  # fetch the chunks in parallel, write each at its offset
  con <- file(remote$file, "r+b")
  on.exit(close(con))
  failed <- character()
  pool <- curl::new_pool(host_con = 8)
  for (i in seq_along(start)) {
    local({
      offset <- start[[i]]
      handle <- curl::new_handle(followlocation = TRUE)
      curl::handle_setheaders(handle, Range = sprintf("bytes=%.0f-%.0f", offset, end[[i]]),
                              `User-Agent` = "OpenSourceAP.DownloadR")
      curl::curl_fetch_multi(
        remote$url, handle = handle, pool = pool,
        done = function(res) {
          if (res$status_code != 206) {
            failed <<- c(failed, paste("HTTP", res$status_code))
          } else {
            seek(con, offset, rw = "write")
            writeBin(res$content, con)
          }
        },
        fail = function(msg) failed <<- c(failed, msg)
      )
    })
  }
  curl::multi_run(pool = pool)
  if (length(failed) > 0) {
    stop("Download failed (", failed[1], "): ", remote$url)
  }
  close(con)
  on.exit()

  return(as.data.frame(nanoparquet::read_parquet(remote$file, col_select = columns)))
}

# 2020 files key months as time_avail_m ("1986m1")
signal_key_columns <- c("permno", "yyyymm", "time_avail_m")

# permno and yyyymm as integers, first. Drops the unnamed row number column
# of the 2020 files.
standardize_signal_keys <- function(data) {
  data <- data[, names(data) != "", drop = FALSE]
  if ("time_avail_m" %in% names(data)) {
    ym <- strsplit(as.character(data$time_avail_m), "m", fixed = TRUE)
    data$yyyymm <- vapply(ym, function(x) as.integer(x[1]) * 100L + as.integer(x[2]), integer(1))
    data$time_avail_m <- NULL
  }
  data$permno <- as.integer(data$permno)
  data$yyyymm <- as.integer(data$yyyymm)
  return(data[, c("permno", "yyyymm", setdiff(names(data), c("permno", "yyyymm"))), drop = FALSE])
}

library(testthat)

# Offline: release ids and file maps

test_that("release ids resolve to Hugging Face branches", {
  resolve <- OpenSourceAP.DownloadR:::resolve_release

  expect_equal(resolve("2025_10")$hf_revision, "2025.10")
  expect_equal(resolve("2025.10")$hf_revision, "2025.10")
  expect_equal(resolve("2023")$hf_revision, "2023.08")
  expect_equal(resolve(2023)$hf_revision, "2023.08")
  expect_equal(resolve("2023_08")$release_id, "2023")
  expect_equal(resolve("2022")$hf_revision, "2022.03")
  expect_equal(resolve("2020_05")$hf_revision, "2020.05")
  expect_match(resolve("2026_10")$note, "TEST DATA")
  expect_error(resolve("1999"), "Invalid release")
})

test_that("default release is 2025_10, listed with all releases", {
  releases <- suppressMessages(capture.output(
    out <- OpenSourceAP.DownloadR:::list_release()))
  expect_equal(out$release_id[out$default], "2025_10")
  expect_true(all(c("2020_05", "2020_07", "2021_03", "2021_04", "2022_04") %in% out$release_id))
})

test_that("file map follows the branch's files", {
  map_files <- OpenSourceAP.DownloadR:::map_hf_files

  paths_2025 <- c(
    "SignalDoc.csv", "SignalDoc.parquet",
    "Firm Level Characteristics/Placebos.parquet",
    "Firm Level Characteristics/Predictors.parquet",
    "Firm Level Characteristics/signed_predictors_dl_wide.parquet",
    "Portfolios/Full Sets OP/PredictorPortsFull.parquet",
    "Portfolios/Full Sets OP/PredictorLSretWide.parquet",
    "Portfolios/Full Sets OP/PlaceboPortsFull.parquet",
    "Portfolios/Full Sets Alt/PlaceboPortsFull.parquet",
    "Portfolios/Full Sets Alt/PredictorAltPorts_Deciles.parquet",
    "Portfolios/Full Sets Alt/PredictorAltPorts_DecilesEW.parquet",
    "DailyPortfolios/Predictor.parquet",
    "Results/rhoStockLevel.RDS"
  )
  m <- map_files(paths_2025, "2025.10")
  expect_setequal(m$download_name, c("signal_doc", "firm_char_unsigned", "firm_char", "op",
                                     "placebo_op", "deciles_ew", "PredictorAltPorts_DecilesEW",
                                     "daily_op_ew"))
  expect_equal(m$file_id[m$download_name == "op"],
               "https://huggingface.co/datasets/OpenSourceAP/CrossSection/resolve/2025.10/Portfolios/Full%20Sets%20OP/PredictorPortsFull.parquet")

  # 2021.03: no SignalDoc, no daily portfolios, Full_Sets_Main
  m <- map_files(c("Firm Level Characteristics/Predictors.parquet",
                   "Portfolios/Full_Sets_Main/PredictorPortsFull.parquet",
                   "Portfolios/Full_Sets_Main/CheckPredictorPorts_Deciles.parquet"), "2021.03")
  expect_setequal(m$download_name, c("firm_char_unsigned", "op", "CheckPredictorPorts_Deciles"))

  # 2020: signals in two files, portfolios under their own names
  m <- map_files(c("signalbase.parquet", "signaladditional.parquet", "portbase.parquet",
                   "allportretdec.parquet", "OnlineAppendixOPCSAP.pdf"), "2020.05")
  expect_equal(sum(m$download_name == "firm_char_unsigned"), 2)
  expect_setequal(m$download_name[m$type == "port"], c("portbase", "allportretdec"))
})

test_that("2020 signal keys are standardized", {
  standardize <- OpenSourceAP.DownloadR:::standardize_signal_keys
  df <- data.frame(x = 1:2, permno = c(10000, 10001), time_avail_m = c("1986m1", "1986m12"),
                   BM = c(0.1, 0.2))
  names(df)[1] <- ""
  out <- standardize(df)
  expect_equal(names(out), c("permno", "yyyymm", "BM"))
  expect_equal(out$yyyymm, c(198601L, 198612L))
  expect_type(out$permno, "integer")
})


# Network: Hugging Face downloads

test_that("default release downloads from Hugging Face", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  openap <- suppressMessages(OpenAP$new())
  expect_equal(openap$hf_revision, "2025.10")
  expect_gt(nrow(openap$signal_sign), 200)

  ports <- capture.output(port_list <- openap$list_port())
  expect_true(all(c("op", "deciles_vw", "daily_op_ew") %in% port_list$download_name))

  port <- openap$dl_port("op", predictor = c("BM", "Mom12m"))
  expect_setequal(unique(port$signalname), c("BM", "Mom12m"))
  expect_true(all(c("port", "date", "ret") %in% names(port)))
  expect_s3_class(port$date, "Date")

  expect_warning(openap$dl_port("op", predictor = "NotASignal"), "not available")
  expect_error(openap$dl_port("firm_char"), "not a portfolio")
})

test_that("dl_signal reads requested columns, signed or not", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  openap <- suppressMessages(OpenAP$new())
  unsigned <- openap$dl_signal(c("BM", "Accruals"))
  signed <- openap$dl_signal(c("BM", "Accruals"), signed = TRUE)

  expect_equal(names(unsigned), c("permno", "yyyymm", "BM", "Accruals"))
  expect_gt(nrow(unsigned), 1e6)
  expect_type(unsigned$yyyymm, "integer")

  sign <- openap$signal_sign$Sign[match("Accruals", openap$signal_sign$Acronym)]
  both <- merge(unsigned, signed, by = c("permno", "yyyymm"))
  ok <- is.finite(both$Accruals.x) & is.finite(both$Accruals.y)
  expect_equal(both$Accruals.y[ok], sign * both$Accruals.x[ok], tolerance = 1e-6)

  expect_error(openap$dl_signal("NotASignal"), "not available")
})

test_that("older releases follow their own layout", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  old <- suppressMessages(OpenAP$new("2023"))
  expect_equal(old$hf_revision, "2023.08")

  old <- suppressMessages(OpenAP$new("2022_04"))
  expect_false("daily_op_ew" %in% old$name_id_map$download_name)

  old <- suppressMessages(OpenAP$new("2021_04"))
  expect_equal(nrow(old$signal_sign), 0)
  expect_error(old$dl_signal_doc(), "not available")

  old <- suppressMessages(OpenAP$new("2020_05"))
  signal <- old$dl_signal("BM")
  expect_equal(names(signal), c("permno", "yyyymm", "BM"))
  expect_true(all(signal$yyyymm %% 100 %in% 1:12))
  expect_error(old$dl_signal("BM", signed = TRUE), "no signed")
})

test_that("2026_10 test data is flagged", {
  skip_on_cran()
  skip_if_offline("huggingface.co")

  expect_warning(suppressMessages(OpenAP$new("2026_10")), "TEST DATA")
})

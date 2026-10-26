# OpenSourceAP.DownloadR 0.2.0

* Data are now read from the OpenAP Hugging Face dataset
  (<https://huggingface.co/datasets/OpenSourceAP/CrossSection>) instead of Google Drive.
  Files are parquet, read with nanoparquet. Google Drive scraping is removed, and with it the
  rvest, stringr, data.table, lubridate, withr and magrittr dependencies.
* Release ids are unchanged; `"2023"` and `"2022"` are the 2023.08 and 2022.03 releases.
* `2026_10` (Data Release 2026.10, data through December 2025) is added and is the default.
* New releases: `2020_05`, `2020_07`, `2021_03`, `2021_04` and `2022_04`. Each release's
  datasets come from its own file listing, so older releases offer only what they contain
  (e.g. no daily portfolios before 2021.04).
* New portfolio datasets: `placebo_op`, `vw_force`, `ff93`, `holdper1`, `holdper3`, `holdper6`,
  `holdper12` and the daily portfolios (`daily_op_ew`, `daily_op_vw`, `daily_deciles_ew`,
  `daily_deciles_vw`, `daily_quintiles_ew`, `daily_quintiles_vw`), where the release has them.
* `dl_signal()` downloads only the requested columns of the release's wide firm
  characteristics file and errors on unavailable signals.
* Signing is consistent: `signed = FALSE` returns all signals, including Price, Size and
  STreversal, unsigned, and `signed = TRUE` returns them all signed. `dl_all_signals()` now
  defaults to `signed = TRUE`, which matches what it returned before; its `signed = TRUE`
  no longer flips signs twice.
* CRSP signals come from the CRSP CIZ monthly file (`crsp.msf_v2`), since the legacy file is
  no longer updated.
* `dl_port()` warns about unavailable predictors, and portfolio `port` is character
  (`"01"`, ..., `"LS"`), as in the release files.
* Removed internal methods `get_individual_signal_url()` and `apply_sign_logic()`.
  `individual_signal_id_map` is kept as an empty data frame. New fields `release_id` and
  `hf_revision`.

# OpenSourceAP.DownloadR 0.1.0

* Initial CRAN submission.

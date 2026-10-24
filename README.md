# OpenSourceAP.DownloadR

**OpenSourceAP_DownloadR** is an R package that allows you to download data from [Open Source Asset Pricing](https://www.openassetpricing.com/).

## Installation

```
install.packages("OpenSourceAP.DownloadR")
# or
devtools::install_github("tomz23/OpenSourceAP.DownloadR")
```

Data are read from the [OpenAP Hugging Face dataset](https://huggingface.co/datasets/OpenSourceAP/CrossSection), where each data release is a branch. No account or token is needed.

## Usage

### Import package and open an instance to the data
```
library(OpenSourceAP.DownloadR)

openap <- OpenAP$new()

# a specific release (2020_05 through 2025_10; 2025_10 is the default)
openap_2024 <- OpenAP$new("2024_10")
```

Older releases differ in what they contain. For example, daily portfolios start with 2021_04, `SignalDoc.csv` with 2022_03, and the 2020 releases have their own portfolio files (`portbase`, `allportretdec`, ...) and unsigned signals only. `list_port()` shows what a release has. `2026_10` is test data, not for research use.

### List available portfolios (various implementations)

```
openap$list_port()
```

### Signal documentation

```
openap$dl_signal_doc()
```

### Download portfolio returns

```
# download all original paper portfolios
openap$dl_port('op')

# download all decile-value-weighted portfolios
openap$dl_port('deciles_vw')

# Download portfolios for specific signals
openap$dl_port('op', 'AssetGrowth')
```

### Download firm characteristics

Only the requested columns are downloaded.
```
openap$dl_signal(c("BM", "Mom12m"))

# signed so that high values predict high returns
openap$dl_signal(c("BM", "Mom12m"), signed = TRUE)
```

To download all signals, or the CRSP signals Price, Size and STreversal, you need a WRDS account.
```
openap$dl_all_signals()
```

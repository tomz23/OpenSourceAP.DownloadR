# OpenSourceAP.DownloadR

**OpenSourceAP_DownloadR** is an R package that allows you to download data from [Open Source Asset Pricing](https://www.openassetpricing.com/).

## Installation

```
install.packages("OpenSourceAP.DownloadR")
# or
devtools::install_github("tomz23/OpenSourceAP.DownloadR")
```

## Usage

### Import package and open an instance to the data
```
library(OpenSourceAP.DownloadR)

openap <- OpenAP$new()
```

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

To download all signals, you need a WRDS account.
```
openap$dl_all_signals()
```

library(OpenSourceAP.DownloadR)

# Open an instance to the OpenAP dataset and show available releases:
openap <- OpenAP$new()


# ==========
# List available portfolios
# ==========
openap$list_port()


# ==========
# Download SignalDoc.csv
# ==========
df = openap$dl_signal_doc()


# ==========
# Portfolios -> Full Sets OP -> PredictorPortsFull.csv
# ==========

# Download entire file
df = openap$dl_port('op')

# Download specific predictors (can be single or multiple predictors)
df = openap$dl_port('op', 'AM')
df = openap$dl_port('op', c('AM', 'Mom12m'))



# ==========
# Portfolios -> Full Sets Alt -> PredictorAltPorts_Deciles.zip
# Portfolios -> Full Sets Alt -> PredictorAltPorts_DecilesVW.zip
# Portfolios -> Full Sets Alt -> PredictorAltPorts_LiqScreen_NYSEonly.zip
# ==========

# Download entire file
df = openap$dl_port('deciles_ew')
df = openap$dl_port('deciles_vw')
df = openap$dl_port('nyse')

# Download specific predictors (can be single or multiple predictors)
df = openap$dl_port('deciles_ew', 'Accruals')
df = openap$dl_port('deciles_ew', c('BM', 'Mom6m'))



# ==========
# Firm Level Characteristics
# ==========

# Download all firm characteristics (signed)
df = openap$dl_all_signals()

# Download specific firm characteristics
df = openap$dl_signal(c('BM', 'AssetGrowth'))
df = openap$dl_signal(c('BM', 'AssetGrowth', 'Size'))  # Requires WRDS login


###########################
# Tests:
###########################

#devtools::install()

# initialize OpenAP
openap_instance <- OpenAP$new(2022)
#openap_instance <- OpenAP$new(release_year = 2023)

# ==========
# list available datasets:
# ==========
openap_instance$list_port()


# ==========
# Download SignalDoc.csv 
# ==========
signal_doc = openap_instance$dl_signal_doc()


# ==========
# Portfolios -> Full Sets OP -> PredictorPortsFull.csv
# ==========

# Download entier file 
data <- openap_instance$dl_port("op")

# Download specific predictors (can be single or multiple predictors)
data2 <- openap_instance$dl_port("op", predictor = c("AM"))
data3 <- openap_instance$dl_port("op", predictor = c("AM", "Mom12m"))


# ==========
# Portfolios -> Full Sets Alt -> PredictorAltPorts_Deciles.zip
# Portfolios -> Full Sets Alt -> PredictorAltPorts_DecilesVW.zip
# Portfolios -> Full Sets Alt -> PredictorAltPorts_LiqScreen_NYSEonly.zip
# ==========

# Download entier file
data4 <- openap_instance$dl_port("deciles_ew")
data5 <- openap_instance$dl_port("deciles_vw")
data6 <- openap_instance$dl_port("port_nyse")

# Download specific predictors (can be single or multiple predictors)
data7 <- openap_instance$dl_port("deciles_ew", predictor = c("Accruals"))
data8 <- openap_instance$dl_port("deciles_ew", predictor = c("BM", "Mom6m"))


# ==========
# Firm Level Characteristics -> Full Sets -> signed_predictors_dl_wide.zip
# ==========

# Download all firm characteristics
signals_data1 <- openap_instance$dl_all_signals()

# Download specific firm characteristics
data9 <- openap_instance$dl_signal("BM")








# ---------------------------------------------------------------------------
# Refresh everything. Run from the project root, e.g. in RStudio with the
# gas_forecasting.Rproj open:   source("R/run_all.R")
#
# 1. Pull new weekly prices from EIA (Python pipeline)
# 2. Re-run the 2-month projection
# 3. Re-knit both reports to HTML
# ---------------------------------------------------------------------------

library(here)

# 1. data refresh (uses the python on your PATH)
system2("python", here("lib", "eia_gas_price_pipeline.py"))

# 2. projection
source(here("R", "gas_forecasting_process.R"))

# 3. reports
rmarkdown::render(here("R", "gas_EDA.Rmd"))
rmarkdown::render(here("R", "gas_forecasting_results.Rmd"))

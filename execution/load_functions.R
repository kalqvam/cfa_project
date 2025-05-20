source("data/financials_historical_is.R")
source("data/financials_historical_bs.R")
source("data/projection_params_is.R")
source("data/projection_params_bs.R")
source("data/utils.R")

source("pro_forma/data_frame_ini.R")

source("pro_forma/populate_historical.R")
source("pro_forma/populate_projections.R")
source("pro_forma/post_processing.R")

source("pro_forma/wrapper_function.R")

cat("All functions and data loaded successfully.\n")
cat("Ready to run financial model with run_complete_financial_model(df_is, df_bs)\n")

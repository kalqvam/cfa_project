source("execution/load_functions.R")

financial_statements <- run_complete_financial_model(df_is, df_bs)

income_statement <- financial_statements$income_statement
balance_sheet <- financial_statements$balance_sheet

wacc_results <- calculate_wacc(dcf_inputs)
dcf_results <- calculate_dcf(income_statement, balance_sheet, wacc_results, fixed_assets_params)

cat("\nFinancial model executed successfully.\n")

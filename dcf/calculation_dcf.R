calculate_dcf <- function(income_statement, balance_sheet, dcf_inputs, fixed_assets_params) {
  wacc_result <- calculate_wacc(dcf_inputs)
  wacc <- wacc_result$wacc
  
  projection_periods <- 2025:2030
  
  df <- data.frame(
    Period = projection_periods,
    EBITDA = income_statement$EBITDA[income_statement$Period %in% projection_periods],
    Tax = income_statement$Corporate_income_tax[income_statement$Period %in% projection_periods]
  )
  
  df$NOPAT <- df$EBITDA - df$Tax
  
  for(i in 1:nrow(df)) {
    period <- df$Period[i]
    prev_period <- if(period == 2025) "2024_Q4" else as.character(period - 1)
    
    current_ppe <- balance_sheet$PP_and_A_owned[balance_sheet$Period == period] +
      balance_sheet$Intangible_assets[balance_sheet$Period == period]
    
    prev_ppe <- balance_sheet$PP_and_A_owned[balance_sheet$Period == prev_period] +
      balance_sheet$Intangible_assets[balance_sheet$Period == prev_period]
    
    ppe_DA <- balance_sheet$PP_and_A_owned[balance_sheet$Period == prev_period] * 
      (1 + fixed_assets_params$growth_rates$pp_and_a_owned[[1]]) * 
      (fixed_assets_params$depreciation_rates$pp_and_a_owned[[1]]) +
      balance_sheet$Intangible_assets[balance_sheet$Period == prev_period] * 
      (1 + fixed_assets_params$growth_rates$intangible_assets[[1]]) * 
      (fixed_assets_params$depreciation_rates$intangible_assets[[1]])
    
    df$CapEx[i] <- -(current_ppe - prev_ppe + ppe_DA)
  }
  
  for(i in 1:nrow(df)) {
    period <- df$Period[i]
    prev_period <- if(period == 2025) "2024_Q4" else as.character(period - 1)
    
    current_nwc <- balance_sheet$Inventories[balance_sheet$Period == period] +
      balance_sheet$Trade_receivables[balance_sheet$Period == period] +
      balance_sheet$Other_current_assets[balance_sheet$Period == period] -
      balance_sheet$Trade_payables[balance_sheet$Period == period] -
      balance_sheet$Other_current_liabilities[balance_sheet$Period == period] -
      balance_sheet$Provisions[balance_sheet$Period == period]
    
    prev_nwc <- balance_sheet$Inventories[balance_sheet$Period == prev_period] +
      balance_sheet$Trade_receivables[balance_sheet$Period == prev_period] +
      balance_sheet$Other_current_assets[balance_sheet$Period == prev_period] -
      balance_sheet$Trade_payables[balance_sheet$Period == prev_period] -
      balance_sheet$Other_current_liabilities[balance_sheet$Period == prev_period] -
      balance_sheet$Provisions[balance_sheet$Period == prev_period]
    
    df$Change_in_NWC[i] <- -(current_nwc - prev_nwc)
  }

  df$FCFF <- df$NOPAT + df$CapEx + df$Change_in_NWC

  df$Discount_factor <- 1 / (1 + wacc)^(1:nrow(df))
  
  df$Discounted_FCFF <- df$FCFF * df$Discount_factor
  
  terminal_fcff <- df$FCFF[nrow(df)] * (1 + dcf_inputs$terminal_growth_rate)
  terminal_value <- terminal_fcff / (wacc - dcf_inputs$terminal_growth_rate)
  discounted_terminal_value <- terminal_value * df$Discount_factor[nrow(df)]
  
  enterprise_value <- sum(df$Discounted_FCFF) + discounted_terminal_value
  
  latest_cash <- balance_sheet$Cash_and_equivalents[balance_sheet$Period == "2024_Q3"]
  
  latest_interest_bearing_debt <- if ("Total_Interest_Bearing_Debt" %in% names(balance_sheet)) {
    balance_sheet$Total_Interest_Bearing_Debt[balance_sheet$Period == "2024_Q3"]
  } else {
    q3_idx <- which(balance_sheet$Period == "2024_Q3")
    balance_sheet$Short_term_loans[q3_idx] +
      balance_sheet$Long_term_loans[q3_idx] +
      balance_sheet$Lease_liabilities_current[q3_idx] +
      balance_sheet$Lease_liabilities_non_current[q3_idx]
  }
  
  equity_value <- enterprise_value - latest_interest_bearing_debt + latest_cash
  price_per_share <- equity_value / dcf_inputs$shares_outstanding

  return(list(
    wacc = wacc,
    wacc_components = wacc_result,
    fcff_calculations = df,
    enterprise_value = enterprise_value,
    equity_value = equity_value,
    price_per_share = price_per_share,
    terminal_value = terminal_value,
    discounted_terminal_value = discounted_terminal_value
  ))
}

# DCF Model inputs
dcf_inputs <- list(
  # Operational inputs
  terminal_growth_rate = 0.025,
  shares_outstanding = 55.27,
  
  # WACC components
  risk_free_rate = 0.025,
  market_risk_premium = 0.047,
  country_risk_premium = 0,
  unlevered_beta = 1.3,
  credit_spread = 0.004,
  
  # Capital structure comes from latest BS, all interest-bearing debt included
  book_value_equity = 111.5,
  interest_bearing_debt = 37.5,  # Total of loans and lease liabilities from latest BS
  tax_rate = 0.20
)

calculate_dcf_valuation <- function(income_statement, balance_sheet, dcf_inputs) {
  library(dplyr)
  
  # Calculate WACC
  total_capital <- dcf_inputs$book_value_equity + dcf_inputs$interest_bearing_debt
  weight_equity <- dcf_inputs$book_value_equity / total_capital
  weight_debt <- dcf_inputs$interest_bearing_debt / total_capital
  
  de_ratio <- dcf_inputs$interest_bearing_debt / dcf_inputs$book_value_equity
  leverage_factor <- 1 + (1 - dcf_inputs$tax_rate) * de_ratio
  levered_beta <- dcf_inputs$unlevered_beta * leverage_factor
  
  cost_equity <- dcf_inputs$risk_free_rate + dcf_inputs$country_risk_premium + 
    dcf_inputs$market_risk_premium * dcf_inputs$unlevered_beta
  
  cost_debt_pretax <- dcf_inputs$risk_free_rate + dcf_inputs$country_risk_premium + 
    dcf_inputs$credit_spread
  cost_debt_posttax <- 0.05 * (1 - dcf_inputs$tax_rate)
  
  wacc <- weight_equity * cost_equity + weight_debt * cost_debt_posttax
  
  # Prepare projection periods (2025-2030)
  projection_periods <- 2025:2030
  
  # Calculate FCFF components
  df <- data.frame(
    Period = projection_periods,
    EBITDA = income_statement$EBITDA[income_statement$Period %in% projection_periods],
    Tax = income_statement$Corporate_income_tax[income_statement$Period %in% projection_periods]
  )
  
  # Calculate NOPAT
  df$NOPAT <- df$EBITDA - df$Tax
  
  # Calculate CapEx
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
  
  # Calculate Change in NWC (now including provisions)
  for(i in 1:nrow(df)) {
    period <- df$Period[i]
    prev_period <- if(period == 2025) "2024_Q4" else as.character(period - 1)
    
    current_nwc <- balance_sheet$Inventories[balance_sheet$Period == period] +
      balance_sheet$Trade_receivables[balance_sheet$Period == period] +
      balance_sheet$Other_current_assets[balance_sheet$Period == period] -
      balance_sheet$Trade_payables[balance_sheet$Period == period] -
      balance_sheet$Other_current_liabilities[balance_sheet$Period == period] -
      balance_sheet$Provisions[balance_sheet$Period == period]  # Added provisions here
    
    prev_nwc <- balance_sheet$Inventories[balance_sheet$Period == prev_period] +
      balance_sheet$Trade_receivables[balance_sheet$Period == prev_period] +
      balance_sheet$Other_current_assets[balance_sheet$Period == prev_period] -
      balance_sheet$Trade_payables[balance_sheet$Period == prev_period] -
      balance_sheet$Other_current_liabilities[balance_sheet$Period == prev_period] -
      balance_sheet$Provisions[balance_sheet$Period == prev_period]  # Added provisions here
    
    df$Change_in_NWC[i] <- -(current_nwc - prev_nwc)
  }
  
  # Calculate FCFF
  df$FCFF <- df$NOPAT + df$CapEx + df$Change_in_NWC
  
  # Calculate Discount Factors and Discounted FCFF
  df$Discount_factor <- 1 / (1 + wacc)^(1:nrow(df))
  df$Discounted_FCFF <- df$FCFF * df$Discount_factor
  
  # Calculate Terminal Value
  terminal_fcff <- df$FCFF[nrow(df)] * (1 + dcf_inputs$terminal_growth_rate)
  terminal_value <- terminal_fcff / (wacc - dcf_inputs$terminal_growth_rate)
  discounted_terminal_value <- terminal_value * df$Discount_factor[nrow(df)]
  
  enterprise_value <- sum(df$Discounted_FCFF) + discounted_terminal_value

  latest_cash <- balance_sheet$Cash_and_equivalents[balance_sheet$Period == "2024_Q3"]
  
  # Calculate interest bearing debt manually if column doesn't exist
  latest_interest_bearing_debt <- if ("Total_Interest_Bearing_Debt" %in% names(balance_sheet)) {
    balance_sheet$Total_Interest_Bearing_Debt[balance_sheet$Period == "2024_Q3"]
  } else {
    # Manual calculation as fallback
    q3_idx <- which(balance_sheet$Period == "2024_Q3")
    balance_sheet$Short_term_loans[q3_idx] +
      balance_sheet$Long_term_loans[q3_idx] +
      balance_sheet$Lease_liabilities_current[q3_idx] +
      balance_sheet$Lease_liabilities_non_current[q3_idx]
  }
  
  equity_value <- enterprise_value - latest_interest_bearing_debt + latest_cash
  price_per_share <- equity_value / dcf_inputs$shares_outstanding
  
  # Return results
  return(list(
    wacc = wacc,
    cost_of_equity = cost_equity,
    cost_of_debt = cost_debt_posttax,
    levered_beta = levered_beta,
    fcff_calculations = df,
    enterprise_value = enterprise_value,
    equity_value = equity_value,
    price_per_share = price_per_share,
    terminal_value = terminal_value,
    discounted_terminal_value = discounted_terminal_value
  ))
}

# Run the model
dcf_valuation <- calculate_dcf_valuation(
  income_statement = income_statement,
  balance_sheet = balance_sheet,
  dcf_inputs = dcf_inputs
)

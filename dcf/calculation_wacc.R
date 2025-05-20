calculate_wacc <- function(dcf_inputs) {
  total_capital <- dcf_inputs$book_value_equity + dcf_inputs$interest_bearing_debt
  weight_equity <- dcf_inputs$book_value_equity / total_capital
  weight_debt <- dcf_inputs$interest_bearing_debt / total_capital
  
  cost_equity <- dcf_inputs$risk_free_rate + dcf_inputs$country_risk_premium + 
    dcf_inputs$market_risk_premium * dcf_inputs$beta
  
  cost_debt_pretax <- dcf_inputs$risk_free_rate + dcf_inputs$country_risk_premium + 
    dcf_inputs$credit_spread
  cost_debt_posttax <- cost_debt_pretax * (1 - dcf_inputs$tax_rate)
  
  wacc <- weight_equity * cost_equity + weight_debt * cost_debt_posttax
  
  return(list(
    wacc = wacc,
    weight_equity = weight_equity,
    weight_debt = weight_debt,
    cost_equity = cost_equity,
    beta = beta,
    cost_debt_posttax = cost_debt_posttax
  ))
}

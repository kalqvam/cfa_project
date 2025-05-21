create_correlation_matrix <- function(print_comparison = TRUE) {
  variable_names <- c(
    "growth_nordics", "growth_roe", "growth_na", "growth_row",
    "ratio_materials", "ratio_employee", "ratio_other",
    "growth_intangibles"
  )
  
  n_vars <- length(variable_names)
  
  # Initialize with identity matrix
  corr_matrix <- diag(n_vars)
  rownames(corr_matrix) <- colnames(corr_matrix) <- variable_names
  
  # Nordics correlations
  corr_matrix["growth_nordics", "growth_roe"] <- 0.94
  corr_matrix["growth_roe", "growth_nordics"] <- 0.94
  
  corr_matrix["growth_nordics", "growth_na"] <- 0.6
  corr_matrix["growth_na", "growth_nordics"] <- 0.6
  
  corr_matrix["growth_nordics", "growth_row"] <- 0.94
  corr_matrix["growth_row", "growth_nordics"] <- 0.94
  
  corr_matrix["growth_nordics", "ratio_materials"] <- 0.03
  corr_matrix["ratio_materials", "growth_nordics"] <- 0.03
  
  corr_matrix["growth_nordics", "ratio_employee"] <- -0.42
  corr_matrix["ratio_employee", "growth_nordics"] <- -0.42
  
  corr_matrix["growth_nordics", "ratio_other"] <- 0.7
  corr_matrix["ratio_other", "growth_nordics"] <- 0.7
  
  corr_matrix["growth_nordics", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "growth_nordics"] <- 0
  
  # Rest of Europe correlations
  corr_matrix["growth_roe", "growth_na"] <- 0.57
  corr_matrix["growth_na", "growth_roe"] <- 0.57
  
  corr_matrix["growth_roe", "growth_row"] <- 0.98
  corr_matrix["growth_row", "growth_roe"] <- 0.98
  
  corr_matrix["growth_roe", "ratio_materials"] <- -0.28
  corr_matrix["ratio_materials", "growth_roe"] <- -0.28
  
  corr_matrix["growth_roe", "ratio_employee"] <- -0.4
  corr_matrix["ratio_employee", "growth_roe"] <- -0.4
  
  corr_matrix["growth_roe", "ratio_other"] <- 0.79
  corr_matrix["ratio_other", "growth_roe"] <- 0.79
  
  corr_matrix["growth_roe", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "growth_roe"] <- 0
  
  # North America correlations
  corr_matrix["growth_na", "growth_row"] <- 0.63
  corr_matrix["growth_row", "growth_na"] <- 0.63
  
  corr_matrix["growth_na", "ratio_materials"] <- -0.26
  corr_matrix["ratio_materials", "growth_na"] <- -0.26
  
  corr_matrix["growth_na", "ratio_employee"] <- -0.54
  corr_matrix["ratio_employee", "growth_na"] <- -0.54
  
  corr_matrix["growth_na", "ratio_other"] <- 0.3
  corr_matrix["ratio_other", "growth_na"] <- 0.3
  
  corr_matrix["growth_na", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "growth_na"] <- 0
  
  # Rest of the World correlations
  corr_matrix["growth_row", "ratio_materials"] <- -0.22
  corr_matrix["ratio_materials", "growth_row"] <- -0.22
  
  corr_matrix["growth_row", "ratio_employee"] <- -0.29
  corr_matrix["ratio_employee", "growth_row"] <- -0.29
  
  corr_matrix["growth_row", "ratio_other"] <- 0.85
  corr_matrix["ratio_other", "growth_row"] <- 0.85
  
  corr_matrix["growth_row", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "growth_row"] <- 0
  
  # Materials and Services correlations
  corr_matrix["ratio_materials", "ratio_employee"] <- 0.23
  corr_matrix["ratio_employee", "ratio_materials"] <- 0.23
  
  corr_matrix["ratio_materials", "ratio_other"] <- -0.21
  corr_matrix["ratio_other", "ratio_materials"] <- -0.21
  
  corr_matrix["ratio_materials", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "ratio_materials"] <- 0
  
  # Employee benefits correlations
  corr_matrix["ratio_materials", "ratio_other"] <- 0.24
  corr_matrix["ratio_other", "ratio_materials"] <- 0.24
  
  corr_matrix["ratio_materials", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "ratio_materials"] <- 0
  
  # Other operating expenses correlations
  corr_matrix["ratio_materials", "growth_intangibles"] <- 0
  corr_matrix["growth_intangibles", "ratio_materials"] <- 0
  
  if(print_comparison) {
    cat("\nOriginal correlation matrix:\n")
    print(round(corr_matrix, 3))
  }
  
  # Find nearest positive definite matrix
  library(Matrix)
  nearest_pd <- as.matrix(nearPD(corr_matrix, corr = TRUE)$mat)
  
  # Ensure proper names are maintained
  rownames(nearest_pd) <- colnames(nearest_pd) <- variable_names
  
  if(print_comparison) {
    cat("\nNearest positive definite matrix:\n")
    print(round(nearest_pd, 3))
    
    # Calculate and print differences
    cat("\nDifferences (Nearest PD - Original):\n")
    diff_matrix <- round(nearest_pd - corr_matrix, 3)
    print(diff_matrix)
    
    # Print maximum absolute difference
    cat("\nMaximum absolute difference:", max(abs(diff_matrix)), "\n")
  }
  
  return(nearest_pd)
}

create_nwc_correlation_matrix <- function(print_comparison = TRUE) {
  variable_names <- c(
    "inventory_ratio", "receivables_ratio", "payables_ratio", "other_current_ratio"
  )
  
  n_vars <- length(variable_names)
  
  # Initialize with identity matrix
  nwc_corr_matrix <- diag(n_vars)
  rownames(nwc_corr_matrix) <- colnames(nwc_corr_matrix) <- variable_names
  
  # Define correlations between NWC components
  # Inventory and receivables (moderate positive correlation)
  nwc_corr_matrix["inventory_ratio", "receivables_ratio"] <- 0.31
  nwc_corr_matrix["receivables_ratio", "inventory_ratio"] <- 0.31
  
  # Inventory and payables (strong positive correlation)
  nwc_corr_matrix["inventory_ratio", "payables_ratio"] <- 0.69
  nwc_corr_matrix["payables_ratio", "inventory_ratio"] <- 0.69
  
  # Inventory and other current (weak positive correlation)
  nwc_corr_matrix["inventory_ratio", "other_current_ratio"] <- 0.78
  nwc_corr_matrix["other_current_ratio", "inventory_ratio"] <- 0.78
  
  # Receivables and payables (moderate positive correlation)
  nwc_corr_matrix["receivables_ratio", "payables_ratio"] <- 0.65
  nwc_corr_matrix["payables_ratio", "receivables_ratio"] <- 0.65
  
  # Receivables and other current (weak positive correlation)
  nwc_corr_matrix["receivables_ratio", "other_current_ratio"] <- 0.04
  nwc_corr_matrix["other_current_ratio", "receivables_ratio"] <- 0.04
  
  # Payables and other current (moderate positive correlation)
  nwc_corr_matrix["payables_ratio", "other_current_ratio"] <- 0.45
  nwc_corr_matrix["other_current_ratio", "payables_ratio"] <- 0.45
  
  if(print_comparison) {
    cat("\nOriginal NWC correlation matrix:\n")
    print(round(nwc_corr_matrix, 3))
  }
  
  # Find nearest positive definite matrix
  library(Matrix)
  nearest_pd <- as.matrix(nearPD(nwc_corr_matrix, corr = TRUE)$mat)
  
  # Ensure proper names are maintained
  rownames(nearest_pd) <- colnames(nearest_pd) <- variable_names
  
  if(print_comparison) {
    cat("\nNearest positive definite matrix:\n")
    print(round(nearest_pd, 3))
    
    # Calculate and print differences
    cat("\nDifferences (Nearest PD - Original):\n")
    diff_matrix <- round(nearest_pd - nwc_corr_matrix, 3)
    print(diff_matrix)
  }
  
  return(nearest_pd)
}

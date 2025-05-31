# Financial Modeling and DCF Valuation Project

This repository contains R code for building a comprehensive financial model and performing discounted cash flow (DCF) valuation analysis. The model projects financial statements and calculates enterprise and equity values using Monte Carlo simulation techniques.

## Project Structure

### Core Financial Model
- **Historical Data**: Income statement and balance sheet data from 2021-2024 Q3
- **Projections**: Forward-looking projections through 2030 with regional revenue breakdowns (Nordics, Rest of Europe, North America, Rest of World)
- **Pro Forma Statements**: Automated generation of projected income statements and balance sheets

### Key Features
- **Revenue Modeling**: Regional growth rate projections with different growth phases
- **Expense Modeling**: Dynamic expense ratios that converge over time
- **Balance Sheet Modeling**: Working capital, fixed assets, and capital structure projections
- **DCF Valuation**: WACC calculation and free cash flow to firm (FCFF) discounting

### Monte Carlo Simulation
- **Stochastic Modeling**: Correlated random variables for key assumptions
- **Scenario Analysis**: 100+ simulation scenarios with correlation matrices
- **Shock Modeling**: Poisson-distributed external shocks (policy changes, competition, technology)
- **Risk Assessment**: Statistical distribution of valuation outcomes

### Model Components
- `data/`: Historical financials and projection parameters
- `pro_forma/`: Core financial statement projection logic
- `dcf/`: Discounted cash flow and WACC calculations
- `monte_carlo/`: Simulation framework and correlation modeling
- `execution/`: Main model execution scripts

## Note

These codes were developed for participation in the **CFA Institute Equity Research Challenge 2025** and have not been reviewed or revised after the challenge completion.

## License

MIT License

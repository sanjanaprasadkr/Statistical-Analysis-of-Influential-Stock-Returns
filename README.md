# Statistical-Analysis-of-Influential-Stock-Returns

## Overview
This project performs a detailed statistical analysis of three influential stocks: Apple Inc. (AAPL), Tesla Inc. (TSLA), and Boeing Co. (BA), focusing on key performance metrics, risk assessment, and returns analysis. The R-based analysis includes technical indicators, volatility modeling, and risk-adjusted performance evaluation, providing actionable insights for financial decision-making.

## Key Analysis Components

1. **Return Metrics**:  
   - **Daily Returns**: Computes percentage changes in stock prices on a day-to-day basis, capturing the volatility of short-term price movements.
   - **Cumulative Returns**: Tracks the compound growth of stock investments over time, useful for understanding long-term trends.

2. **Volatility and Risk Assessment**:  
   - **Rolling Volatility**: Implements a rolling window approach to evaluate stock price fluctuations over time, providing a dynamic view of risk.
   - **Standard Deviation of Returns**: Calculates the total variability in returns to give an overall risk measure.

3. **Performance Evaluation**:  
   - **Sharpe Ratio**: Computes the risk-adjusted return by comparing excess return over a risk-free rate to the stock's volatility. This metric helps assess whether the stock's returns compensate for the associated risk.
   - **Drawdown Analysis**: Measures the peak-to-trough decline in stock prices to evaluate the downside risk over the investment period.

4. **Moving Averages**:  
   - **Simple Moving Average (SMA)** and **Exponential Moving Average (EMA)**: Smooths price series to highlight underlying trends and potential buy/sell signals. SMA gives equal weight to all data points, while EMA gives more significance to recent prices.

5. **Portfolio Risk and Return**:  
   The code also supports the analysis of multi-asset portfolios, providing metrics like overall portfolio return and risk using mean-variance analysis.


## Visualization

- **Line Graphs**: Track stock price movements, overlaid with moving averages for trend analysis.
- **Histograms**: Display the distribution of daily returns, offering a snapshot of risk profiles.
- **Rolling Volatility**: Line plots capturing the evolution of volatility and its implications on risk over time.

## Requirements

To run the analysis, ensure the following R packages are installed:
- `ggplot2` for visualization.
- `PerformanceAnalytics` for financial performance metrics.
- `quantmod` for handling stock time series data.
- `dplyr` for data manipulation.

Install these packages by running:

```r
install.packages(c("ggplot2", "PerformanceAnalytics", "quantmod", "dplyr"))

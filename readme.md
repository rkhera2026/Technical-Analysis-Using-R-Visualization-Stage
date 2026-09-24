Assignment: Technical Analysis Using R
Portfolio Visualization Dashboard
Name: Rubeena Khera
Course: Data Science Tools and Techniques
Assignment: Technical Analysis using R, Visualization Phase
 

Project Description
In this project, an interactive portfolio visualization dashboard is developed using R Shiny.

The dashboard extracts historical data of the stock market from Yahoo Finance and allows interactive features for analyzing stock price trends, technical analysis, and signals.

A user can enter the stock symbol, date range, time frame, and technical indicators that need to be displayed.

Used Technologies
R
Shiny
ggplot2
quantmod
Yahoo Finance
Features
The dashboard includes:

Historical stock data retrieval from Yahoo Finance
Interactive stock symbol selection
Interactive date-range selection
Daily, weekly, and monthly time-frame options
Line chart visualization
Area chart visualization
Candlestick chart visualization
Moving Average indicators
Relative Strength Index (RSI)
Moving Average Convergence Divergence (MACD)
Buy, Sell, and Hold trading signals
Trading signal annotations on the stock chart
Technical Indicators
Moving Averages
The dashboard uses short-term and long-term moving averages to help identify trends in stock prices.

A moving-average crossover is used as part of the trading strategy.

Short-term moving average: 20 periods
Long-term moving average: 50 periods
When the short-term moving average is above the long-term moving average, the strategy produces a Buy condition.

When the short-term moving average is below the long-term moving average, the strategy produces a Sell condition.

Otherwise, the strategy produces a Hold condition.

RSI
The Relative Strength Index measures momentum and helps identify potentially overbought and oversold conditions.

The dashboard uses a 14-period RSI.

RSI above 70: potentially overbought
RSI below 30: potentially oversold
MACD
The Moving Average Convergence Divergence indicator is used to analyze momentum and trend direction.

The dashboard displays:

MACD line
Signal line
Histogram
Trading Signals
The dashboard generates basic trading signals using moving-average comparisons.

Condition	Signal
Short MA > Long MA	Buy
Short MA < Long MA	Sell
Short MA = Long MA	Hold

The generated signals are displayed on the stock price visualization.

How to Run the Application
1. Install R
Make sure R is installed on your computer.

2. Install Required Packages
Run the following commands in R:

install.packages("shiny")
install.packages("ggplot2")
install.packages("quantmod")

3. Open the Project
Open the Assignment 6 project folder in RStudio.

Make sure app.R is located in the project directory.

4. Run the Application
Open app.R and click:

Run App

Alternatively, run:

shiny::runApp()

5. Use the Dashboard
After the application opens:

Select a stock symbol.
Select the desired date range.
Select a time frame.
Select the desired technical indicators.
Choose a chart type.
Click the update button if provided.
Review the generated trading signals and visualizations.
Repository Structure
BDA400_Assignment6/

── app.R
── README.md
── Cover_Page.pdf

Data Source
The historical data of stocks is fetched from Yahoo Finance using quantmod package.

Repository
Github Repository: https://github.com/rkhera2026/Technical-Analysis-Using-R-Visualization-Stage.git
Conclusion
This assignment showcases the use of R Shiny for visualizing financial data interactively. The dashboard includes the historical data of stocks, technical indicators, chart visualization, and trading rules.

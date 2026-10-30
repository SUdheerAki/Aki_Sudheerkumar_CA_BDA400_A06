# BDA400 Assignment 6 — Technical Analysis using R (Visualization Phase)

**Student:** Sudheer Kumar Aki  
**Course:** BDA400 / Data Science Tools and Techniques  
**Assignment:** A06 — Visualization Phase (15%)  
**Repository:** https://github.com/SUdheerAki/Aki_Sudheerkumar_CA_BDA400_A06

## Project
An R Shiny stock dashboard built using **shiny**, **ggplot2**, **quantmod**, **TTR** and **zoo**. It retrieves historical OHLC prices from Yahoo Finance; allows ticker, date range, daily/weekly/monthly aggregation and line/candlestick/area chart choices; provides switchable 20/50 SMA, RSI and MACD indicators with customizable periods; marks buy and sell **crossovers**; lists recent signals; and exports data as CSV.

## Run
1. Install R and RStudio.
2. Run `install.packages(c("shiny","ggplot2","quantmod","TTR","zoo"))` in the R console.
3. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp(".")` from the project folder.
4. Internet access is required for Yahoo Finance.

## Tasks
- Step 1: Yahoo Finance data retrieval, missing-record handling, and validation.
- Step 2: Shiny UI, date and time-frame controls, and OHLC price charts.
- Step 3: Optional SMA overlays, RSI subplot, and MACD subplot.
- Step 4: Configurable SMA crossover rules; Buy/Sell labels on bars and signal table.

## Validation checklist
- [ ] Run App starts without console error.
- [ ] AAPL data and a price chart load.
- [ ] Daily, Weekly and Monthly time frames render.
- [ ] Line, Candlestick and Area styles render.
- [ ] Moving Averages, RSI and MACD individually toggle.
- [ ] Buy/Sell annotations display on a range with crossovers.
- [ ] CSV download works.
- [ ] Invalid ticker displays a readable error.

> This is coursework and not investment advice. The dashboard needs an internet connection and actual RStudio execution should be verified before final submission.

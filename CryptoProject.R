# Libraries called 

library(tidyverse)
library(ggfortify)
library(quantmod)
library(timeSeries)
library(fPortfolio)
library(caTools)
library(PerformanceAnalytics)

# Collecting the close price data for the 4 cryptocurrencies, from the 1/1/2020 to 6/5/2022

Bitcoin <- getSymbols.yahoo("BTC-EUR", 
                            from = "2020-1-01", to = "2022-05-9" ,
                            periodicity = "daily", auto.assign = F)[,4]
Ethereum <- getSymbols.yahoo("ETH-EUR",
                             from = "2020-1-01", to = "2022-05-9" ,
                             periodicity = "daily", auto.assign = F)[,4]
Binance <- getSymbols.yahoo("BNB-EUR",
                            from = "2020-1-01", to = "2022-05-9" ,
                            periodicity = "daily", auto.assign = F)[,4]
Dogecoin <- getSymbols.yahoo("DOGE-EUR", 
                             from = "2020-1-01", to = "2022-05-9" ,
                             periodicity = "daily", auto.assign = F)[,4]

# Calculating Rate Of Chance for said cryptocurrencies 
Bit_returns <- ROC(Bitcoin)
Eth_returns <- ROC(Ethereum)
Bina_returns <- ROC(Binance)
Doge_returns <- ROC(Dogecoin)

# Calculating the green and the red days 
bit_days <- summary(Bit_returns >0)
Eth_days <- summary(Eth_returns >0)
bina_days <- summary(Bina_returns >0)
Doge_days <- summary(Doge_returns >0)

# Calculating discriptive statistics for the ROC

bi_roc_stats <- summary(Bit_returns)
eth_roc_stats <- summary(Eth_returns)
bina_roc_stats <- summary(Bina_returns)
doge_roc_returns <- summary(Doge_returns)

# Plotting the Coins
autoplot.zoo(Bitcoin)+
  ylab("BitCoin Price")
  
  autoplot.zoo(Ethereum)+
  ylab("Eth Price")

autoplot.zoo(Binance)+
  ylab("Bina Price")

autoplot.zoo(Dogecoin)+
  ylab("Doge Price")

# Plotting the ROC
bit_plot <- autoplot.zoo(Bit_returns)+
  eth_plot <- autoplot.zoo(Eth_returns)
bina_plot <- autoplot.zoo(Bina_returns)
doge_plot <- autoplot.zoo(Doge_returns)


# Calculating Avarage return per year for the various cryptocurrencies 

Bit_table <- table.AnnualizedReturns(Bit_returns)
Eth_table <- table.AnnualizedReturns(Eth_returns)
Bina_table <- table.AnnualizedReturns(Bina_returns)
Doge_table <- table.AnnualizedReturns(Doge_returns)

# Combining all the  tables 

Coin_annualised <- cbind(Bit_table, Eth_table, Bina_table, Doge_table)

# Calculating discriptive statistics for the cyprocurrencies

Bit_stats <- summary(Bitcoin)
Eth_stats <- summary(Ethereum)
Bina_stats <- summary(Binance)
doge_stats <- summary(Dogecoin)


#things to do
#integrate confidence intervals
#do pairwise comparisons

#Libraries used
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(car)
library(tidyverse)
library(zoo)

#Function to calculate daily returns
calculate_daily_returns <- function(df, stock_name) {
  df <- df %>%
    mutate(
      !!paste0("Daily_Returns") := (Close - lag(Close)) / lag(Close)
    )
    colnames(df)[-1] <- paste0(colnames(df)[-1], "_", stock_name)
  return(df)
}

#Stocks have been downloaded from yahoo finance and a daily return column is added
AAPL <- calculate_daily_returns(AAPL, "AAPL")
TSLA <- calculate_daily_returns(TSLA, "TSLA")
BA <- calculate_daily_returns(BA, "BA")
SPY <- calculate_daily_returns(SPY, "SPY")

datasets <- list(AAPL, TSLA, BA, SPY)
#merging all the separate stocks into one dataframe by the "Date" column
merged_data <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), datasets)

selected_columns <- c(
  "Date",
  "Close_AAPL", "Volume_AAPL", "Daily_Returns_AAPL",
  "Close_TSLA", "Volume_TSLA", "Daily_Returns_TSLA",
  "Close_BA", "Volume_BA", "Daily_Returns_BA",
  "Close_SPY", "Daily_Returns_SPY"
)

selected_columns_df <- merged_data[selected_columns]
df <- data.frame(selected_columns_df)

#SLR

df <- df[complete.cases(df$Volume_AAPL, df$Daily_Returns_AAPL), ]
cor(df$Volume_AAPL, df$Daily_Returns_AAPL)

df <- df[complete.cases(df$Volume_TSLA, df$Daily_Returns_TSLA), ]
cor(df$Volume_TSLA, df$Daily_Returns_TSLA)

df <- df[complete.cases(df$Volume_BA, df$Daily_Returns_BA), ]
cor(df$Volume_BA, df$Daily_Returns_BA)

m1 <- lm(df$Daily_Returns_AAPL ~ df$Volume_AAPL)
plot(df$Volume_AAPL, df$Daily_Returns_AAPL, xlab = 'Volume', ylab = 'Daily_Returns', main = "Daily Returns vs Volume for AAPL")
abline(m1 , col = "blue")
hist(resid(m1))
plot(fitted(m1), resid(m1), axes = TRUE, frame.plot = TRUE, main = 'Residual plot for AAPL')
summary(m1)

#hypothesis testing 
qf(.95, df1=1, df2=753) #F_critical = 3.853838
#F = 0.2609 from summary table
#therefore fail to reject H0, no linear association

m2 <- lm(df$Daily_Returns_TSLA ~ df$Volume_TSLA)
plot(df$Volume_TSLA, df$Daily_Returns_TSLA, xlab = 'Volume', ylab = 'Daily_Returns', main = "Daily Returns vs Volume for TSLA")
abline(m2 , col = "blue")
hist(resid(m2))
plot(fitted(m2), resid(m2), axes = TRUE, frame.plot = TRUE, xlab = 'Fitted values', ylab = 'Residual values', main = 'Residual plot for TSLA')
summary(m2)

#hypothesis testing 
qf(.95, df1=1, df2=753) #F_critical = 3.853838
#F = 16.96 from summary table
#therefore fail to reject H0, there is a linear association

m3 <- lm(df$Daily_Returns_BA ~ df$Volume_BA)
plot(df$Volume_BA, df$Daily_Returns_BA, xlab = 'Volume', ylab = 'Daily_Returns', main = "Daily Returns vs Volume for BA")
abline(m3 , col = "blue")
hist(resid(m3))
plot(fitted(m3), resid(m3), axes = TRUE, frame.plot = TRUE, main = "Residual plot for BA")
summary(m3)

#hypothesis testing 
qf(.95, df1=1, df2=753) #F_critical = 3.853838
#F = 10.94 from summary table
#therefore fail to reject H0, there is a linear association


#MLR
#For assumption of indepedence
# Autocorrelation plots for returns
par(mar = c(3, 3, 2, 1), xlab = "Days", ylab = "Returns")  
acf_AAPL <- acf(df$Daily_Returns_AAPL[-1], lag.max = 20, col = "blue") #since first entry is NA
abline(h = c(-1.96 / sqrt(length(df$Daily_Returns_AAPL)), 1.96 / sqrt(length(df$Daily_Returns_AAPL))), col = "green")
mtext("Autocorrelation Plot - APPL Returns", side = 3, line = 0.8, cex = 1.2, font = 1)
#9 days

par(mar = c(3, 3, 2, 1))  
acf_TSLA <- acf(df$Daily_Returns_TSLA[-1], lag.max = 20, col = "blue") #since first entry is NA
abline(h = c(-1.96 / sqrt(length(df$Daily_Returns_TSLA)), 1.96 / sqrt(length(df$Daily_Returns_TSLA))), col = "green")
mtext("Autocorrelation Plot - TSLA Returns", side = 3, line = 0.8, cex = 1.2, font = 1)
#9 days

par(mar = c(3, 3, 2, 1))  
acf_BA <- acf(df$Daily_Returns_BA[-1], lag.max = 20, col = "blue") #since first entry is NA
abline(h = c(-1.96 / sqrt(length(df$Daily_Returns_BA)), 1.96 / sqrt(length(df$Daily_Returns_BA))), col = "green")
mtext("Autocorrelation Plot - BA Returns", side = 3, line = 0.8, cex = 1.2, font = 1)
#9 days


df <- df %>%
  mutate(
    lagged_returns_AAPL = lag(Daily_Returns_AAPL, 9),
    lagged_returns_TSLA = lag(Daily_Returns_TSLA, 9),
    lagged_returns_SPY = lag(Daily_Returns_SPY, 9),
    lagged_returns_BA = lag(Daily_Returns_BA, 9)
  )


#check assumption of linearity, constant variance, normality of residuals
data1 <- data.frame(df$Daily_Returns_AAPL, df$lagged_returns_AAPL, df$lagged_returns_SPY, df$Volume_AAPL)
cor(data1, use = "complete.obs")

mlr_m1 <- lm(df$Daily_Returns_AAPL ~ df$lagged_returns_AAPL + df$lagged_returns_SPY + df$Volume_AAPL)
plot(fitted(mlr_m1), resid(mlr_m1), axes = TRUE, frame.plot = TRUE, xlab = 'Fitted values', ylab = 'Residual', main = "Residual plot")
hist(resid(mlr_m1))
summary(mlr_m1)

#hypothesis testing 
qf(.95, df1=3, df2=742) #F_critical = 2.616905
#F = 7.644 from anova table
#therefore reject H0, there is a linear association


data2 <- data.frame(df$Daily_Returns_TSLA, df$lagged_returns_TSLA, df$lagged_returns_SPY, df$Volume_TSLA)
cor(data2, use = "complete.obs")

mlr_m2 <- lm(df$Daily_Returns_TSLA ~ df$lagged_returns_TSLA + df$lagged_returns_SPY + df$Volume_TSLA)
plot(fitted(mlr_m2), resid(mlr_m2), axes = TRUE, frame.plot = TRUE, xlab = 'Fitted values', ylab = 'Residual', main = "Residual plot")
hist(resid(mlr_m2))
summary(mlr_m2)

#hypothesis testing 
qf(.95, df1=3, df2=742) #F_critical = 2.616905
#F = 7.513  from anova table
#therefore reject H0, there is a linear association


data3 <- data.frame(df$Daily_Returns_BA, df$lagged_returns_BA, df$lagged_returns_SPY, df$Volume_BA)
cor(data3, use = "complete.obs")

mlr_m3 <- lm(df$Daily_Returns_BA ~ df$lagged_returns_BA + df$lagged_returns_SPY + df$Volume_BA)
plot(fitted(mlr_m3), resid(mlr_m3), axes = TRUE, frame.plot = TRUE, xlab = 'Fitted values', ylab = 'Residual', main = "Residual plot")
hist(resid(mlr_m3))
summary(mlr_m3)

#hypothesis testing 
qf(.95, df1=3, df2=742) #F_critical = 2.616937
#F = 4.201 from summary table
#therefore reject H0, there is a linear association

#pairwise t tests
qt(0.025, 742) #-1.963166
2.153e-02/4.734e-02  #0.4547951 #lagged returns BA
2.961e-01/1.151e-0 #0.2572546 #lagged_returns SPY
3.090e-10/9.520e-11 #3.245798 #volume BA
#therefore all have an affect on BA returns


#ANOVA

stacked_data <- data.frame(
  stock = rep(c("AAPL", "TSLA", "BA"), each = nrow(df)),
  return = c(df$Daily_Returns_AAPL, df$Daily_Returns_TSLA, df$Daily_Returns_BA),
  volume = c(df$Volume_AAPL, df$Volume_TSLA, df$Volume_BA)
)

aggregate(stacked_data$return, by=list(stacked_data$stock), summary)

# one-way ANOVA
anova_result <- aov(return ~ stock, data = stacked_data)
summary(anova_result) #F = 1.267
qf(.95, df1=2, df2=2262) #2.999703
#therefore there is no difference in means between returns of different stocks

#pairwise t test
TukeyHSD(anova_result)
#Tukey multiple comparisons of means 95% family-wise confidence level
#no groups are different from each other


#Two sample test for proportions
#add SMA column
calculate_sma <- function(data, price_column, window, suffix) {
  sma_column <- paste0("SMA_", suffix, "_", window)
  
  data[, sma_column] <- rollmean(data[, price_column], k = window, align = "right", fill = NA)
  
  return(data)
}

df <- calculate_sma(df, 'Close_AAPL', 10, "AAPL")
df <- calculate_sma(df, 'Close_TSLA', 10, "TSLA")
df <- calculate_sma(df, 'Close_BA', 10, "BA")

# Generate trading signals
df$Signal_AAPL <- ifelse(df$Close_AAPL > df$SMA_AAPL_10, 1, 0)
df$Signal_TSLA <- ifelse(df$Close_TSLA > df$SMA_TSLA_10, 1, 0)
df$Signal_BA <- ifelse(df$Close_BA > df$SMA_BA_10, 1, 0)

stacked_data2 <- data.frame(
  stock = rep(c("TSLA", "BA"), each = nrow(df)),
  signal = c(df$Signal_TSLA, df$Signal_BA),
  volume = c(df$Volume_TSLA, df$Volume_BA)
)

stacked_data2 <- na.omit(stacked_data2)

summary_data <- stacked_data2 %>%
  group_by(stock, signal) %>%
  summarise(count = n())

print(summary_data)

p1_cap = 375/746
p2_cap = 420/746
pcap = (375+420) / (746+746)
n1 = 746
n2= 746

z_stat <- (p1_cap - p2_cap) / sqrt((pcap * (1 - pcap)) * ((1/n1) + (1/n2)))
print(z_stat) #-2.335054

prop.test(c(375, 420), c(746, 746), conf.level=0.95, correct=FALSE) #p = 0.01954
#therefore reject H0,  the proportion of stocks with buy signals
#is not the same across BA and TSLA

#Multiple Logistic Regression
#predict signal from stock and volume
#changing TSLA = 1, BA = 2
stacked_data2 <- stacked_data2 %>%
  mutate(stock = ifelse(stock == "TSLA", 1, ifelse(stock == "BA", 2, stock)))

m_logreg<- glm(stacked_data2$signal ~ stacked_data2$stock + stacked_data2$volume, family=binomial)
summary(m_logreg)

library(pROC)
stacked_data2$prob <-predict(m_logreg, type=c("response"))
g <- roc(stacked_data2$signal ~ stacked_data2$prob)
print(g)

plot(g, main = "ROC curve")



### Basic Visualization

# Import necessary libraries
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)

# Download SPY data
getSymbols('SPY')

# Get closing prices
prices <- Cl(SPY)
head(prices)
tail(prices)

# Plot the price series
plot(prices, main = "SPY Price")

# Calculate yearly returns
ret_yearly <- prices %>%
  Return.calculate() %>%
  apply.yearly(Return.cumulative) %>%
  round(4) %>%
  fortify.zoo() %>%
  mutate(Index = as.numeric(format(Index, "%Y")))

# print(ret_yearly)

# Plot yearly returns as a bar graph
ggplot(ret_yearly, aes(x = Index, y = SPY.Close)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = ret_yearly$Index, expand = c(0.01, 0.01)) +
  geom_text(aes(label = paste0(round(SPY.Close * 100, 2), "%"),
                vjust = ifelse(SPY.Close >= 0, -0.5, 1.5)),
            size = 3) +
  xlab(NULL) + ylab(NULL)





### Calculating Market Beta

# Import libraries
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(openxlsx)
library(stringr)

# Read tickers file
tickers <- read.xlsx("KOR_ticker_2023-04-20.xlsx")

# Find appropriate tickers
afreeca_ticker <- tickers %>%
  filter(str_detect(종목명, "아프리카")) %>%
  select(종목코드) %>%
  pull()

# Set symbols
symbols <- c("067160.KQ", "102110.KS")

# Import price series
getSymbols(symbols)

prices <- do.call(cbind, lapply(symbols, function(x) Cl(get(x))))

# Calculate returns
ret <- Return.calculate(prices)

# Extract returns for the desired period
ret_3y <- ret['2021-01::2023-11']

# Separate returns
af_ret_3y <- ret_3y[, 1]
market_ret_3y <- ret_3y[, 2]

# Fit linear model
market_beta_3y <- lm(af_ret_3y ~ market_ret_3y)

summary(market_beta_3y)

# 5-year market beta
ret_5y <- ret['2018-11::2023-11']
af_ret_5y <- ret_5y[, 1]
market_ret_5y <- ret_5y[, 2]
market_beta_5y <- lm(af_ret_5y ~ market_ret_5y)
summary(market_beta_5y)

# 2-year market beta
ret_2y <- ret['2021-11::2023-11']
af_ret_2y <- ret_2y[, 1]
market_ret_2y <- ret_2y[, 2]
market_beta_2y <- lm(af_ret_2y ~ market_ret_2y)
summary(market_beta_2y)

# 1-year market beta
ret_1y <- ret['2022-11::2023-11']
af_ret_1y <- ret_1y[, 1]
market_ret_1y <- ret_1y[, 2]
market_beta_1y <- lm(af_ret_1y ~ market_ret_1y)
summary(market_beta_1y)

# 6-month market beta
ret_6m <- ret['2023-05::2023-11']
af_ret_6m <- ret_6m[, 1]
market_ret_6m <- ret_6m[, 2]
market_beta_6m <- lm(af_ret_6m ~ market_ret_6m)
summary(market_beta_6m)

# Define the rolling window size (days)
window_size <- 126

# Calculate rolling beta
rolling_beta <- rollapply(
  ret_2y,
  width = window_size,
  FUN = function(x) coef(lm(x[, 1] ~ x[, 2]))[2],
  by.column = FALSE,
  align = 'right'
)

# Create a time series object for plotting
rolling_beta_zoo <- zoo(rolling_beta, order.by = index(ret_2y)[window_size:length(ret_2y)])

# Plot the rolling beta
ggplot(data = fortify.zoo(rolling_beta_zoo), aes(x = Index, y = rolling_beta_zoo)) +
  geom_line() +
  theme_minimal() +
  labs(title = 'Rolling Beta of AfreecaTV', x = 'Date', y = 'Rolling Beta')

# Plot scatter plot of returns
plot(as.numeric(ret_2y[, 2]), as.numeric(ret_2y[, 1]), pch = 4, cex = 0.3,
     xlab = "KOSPI 200", ylab = "AfreecaTV",
     xlim = c(-0.02, 0.02), ylim = c(-0.02, 0.02))
abline(a = 0, b = 1, lty = 2)
abline(market_beta_2y, col = 'red')





### Constructing Low Volatility Portfolio Using Daily Returns

# Import libraries
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)
library(openxlsx)

# Import ticker Excel file
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")

# Import consolidated adjusted closing price file
KOR_price <- read.csv("KOR_price/KOR_3Yadjclose_combined.csv")

# Convert date column to Date type
KOR_price$X <- as.Date(KOR_price$X)

# Define start and end dates
end_date <- max(KOR_price$X)
start_date <- end_date - 365

# Subset data
KOR_price_y1 <- KOR_price[KOR_price$X >= start_date & KOR_price$X <= end_date, ]

# Calculate returns
ret_y1 <- Return.calculate(as.xts(KOR_price_y1[, -1], order.by = KOR_price_y1$X))

## Handle NAs

# Remove columns with excessive NAs
na_col_idx <- which(colSums(is.na(ret_y1)) > 1)
ret_y1 <- ret_y1[-1, ]
ret_y1 <- ret_y1[, -na_col_idx]

# Check for NAs
sum(is.na(ret_y1))

# Calculate standard deviations and annualize
std_12m_daily <- apply(ret_y1, 2, sd) * sqrt(252)

# Plot histogram of standard deviations
std_12m_daily_df <- data.frame(std = std_12m_daily)

ggplot(std_12m_daily_df, aes(x = std)) +
  geom_histogram(binwidth = 0.01) +
  xlab("Annualized Volatility")

# Remark: We see some stocks have zero standard deviation. We remove them.

# Replace zero standard deviations with NA
std_12m_daily[std_12m_daily == 0] <- NA

# Re-plot histogram
std_12m_daily_df <- data.frame(std = std_12m_daily)

ggplot(std_12m_daily_df, aes(x = std)) +
  geom_histogram(binwidth = 0.01) +
  xlab("Annualized Volatility")

# Identify top 30 low-volatility stocks
invest_lowvol <- rank(std_12m_daily, na.last = "keep") <= 30

# Extract information
lowvol_stocks <- KOR_ticker[invest_lowvol, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  mutate(변동성 = round(std_12m_daily[invest_lowvol], 4))

# print(lowvol_stocks)





### Constructing low vol portfolio using weekly returns

# Calculate standard deviation based on weekly returns
std_12m_weekly <- ret_y1 %>%
  apply.weekly(Return.cumulative) %>%
  apply(2, sd) * sqrt(52)

# Replace zero standard deviations with NA
std_12m_weekly[std_12m_weekly == 0] <- NA

# Identify top 30 low-volatility stocks
invest_lowvol_weekly <- rank(std_12m_weekly, na.last = "keep") <= 30

# Extract information
lowvol_stocks_weekly <- KOR_ticker[invest_lowvol_weekly, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  mutate(변동성 = round(std_12m_weekly[invest_lowvol_weekly], 4))

# print(lowvol_stocks_weekly)





## Intersection of Daily and Weekly low-vol stocks
# Find common stocks
common_lowvol_stocks <- intersect(
  KOR_ticker[invest_lowvol, "종목명"],
  KOR_ticker[invest_lowvol_weekly, "종목명"]
)

common_lowvol_stocks





### Constructing Momentum factor portfolio

# Import libraries
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)
library(openxlsx)
library(tidyr)
library(ggplot2)

# Import data
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")
KOR_price <- read.csv("KOR_price/KOR_3Yadjclose_combined.csv")

KOR_price_xts <- as.xts(KOR_price[, -1], order.by = as.Date(KOR_price$X))

## Recent 12-month returns

# Calculate returns for the last 252 trading days
ret <- Return.calculate(KOR_price_xts) %>% xts::last(252)

# Calculate 12-month cumulative returns
cumret_12m <- apply(ret, 2, function(x) prod(1 + x) - 1)

# Identify top 30 momentum stocks
invest_mom_12m <- rank(-cumret_12m, na.last = "keep") <= 30

# Extract information
mom_stocks <- KOR_ticker[invest_mom_12m, ] %>%
  select(종목명, 종목코드, 시가총액, 업종명) %>%
  mutate(`12-month Return` = round(cumret_12m[invest_mom_12m], 4))

# mom_stocks

## Risk adjusted 12 month returns

# Calculate annualized volatility
sdv_12m <- apply(ret, 2, sd) * sqrt(252)

# Calculate Sharpe ratio
sharpe_mom_12m <- cumret_12m / sdv_12m

# Identify top 30 stocks based on Sharpe ratio
invest_mom_sharpe_12m <- rank(-sharpe_mom_12m, na.last = "keep") <= 30

# Extract information
mom_sharpe_stocks <- KOR_ticker[invest_mom_sharpe_12m, ] %>%
  select(종목명, 종목코드, 시가총액, 업종명) %>%
  mutate(
    `12-month Return` = round(cumret_12m[invest_mom_sharpe_12m], 2),
    `12-month Volatility` = round(sdv_12m[invest_mom_sharpe_12m], 2),
    `12-month Sharpe` = round(sharpe_mom_12m[invest_mom_sharpe_12m], 2)
  )

mom_sharpe_stocks

## Intersection of momentum portfolios

# Find common stocks
common_mom_stocks <- intersect(
  KOR_ticker[invest_mom_12m, "종목명"],
  KOR_ticker[invest_mom_sharpe_12m, "종목명"]
)

common_mom_stocks

## Plot

# Plot price series
KOR_price_xts[, invest_mom_sharpe_12m] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) %>%
  ggplot(aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap(~ ticker, scales = 'free') +
  xlab(NULL) + ylab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())





### Constructing Value Factor
# Import libraries
library(stringr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(corrplot)

# Import data
KOR_value <- read.csv('KOR_valuation_clean.csv', row.names = 1)
KOR_ticker <- read.xlsx('KOR_ticker_2023-04-20.xlsx')

## Constructing Value Portfolio using PBR

# Identify top 30 value stocks based on PBR
invest_PBR <- rank(KOR_value$PBR, na.last = "keep") <= 30

# Extract information
value_PBR_stocks <- KOR_ticker[invest_PBR, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  mutate(PBR = round(KOR_value$PBR[invest_PBR], 2))

# value_PBR_stocks

## Constructing Value Portfolio using Multiple Valuation Ratios

# Rank transform valuation matrix
rank_value <- KOR_value %>%
  mutate_all(list(~ min_rank(.)))

# Correlation matrix
cor_matrix <- cor(rank_value, use = 'complete.obs')

# Plot correlation matrix
corrplot(cor_matrix, method = 'color', type = 'lower',
         addCoef.col = 'black', number.cex = 1,
         tl.cex = 1, tl.srt = 0, tl.col = 'black',
         col = colorRampPalette(c('blue', 'white', 'red'))(200),
         mar = c(0, 0, 0.5, 0))

# Sum rankings
rank_sum <- rowSums(rank_value, na.rm = TRUE)

# Identify top 30 value stocks based on aggregate rankings
invest_value <- rank(rank_sum, na.last = "keep") <= 30

# Extract information
value_stocks <- KOR_ticker[invest_value, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  mutate(
    PBR = round(KOR_value$PBR[invest_value], 2),
    PER = round(KOR_value$PER[invest_value], 2),
    PCR = round(KOR_value$PCR[invest_value], 2),
    PSR = round(KOR_value$PSR[invest_value], 2)
  )

# value_stocks

## Intersection of Value Portfolios

# Find common stocks
common_value_stocks <- intersect(
  KOR_ticker[invest_PBR, '종목명'],
  KOR_ticker[invest_value, '종목명']
)

common_value_stocks





### Constructing Quality Factor Portfolios

# Import libraries
library(stringr)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(tidyr)
library(corrplot)

# Import data
KOR_fs <- readRDS("KOR_fs_clean.Rds")
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")

## Construct quality portfolio based on F-score

# Determine the correct column based on the date
if (lubridate::month(Sys.Date()) %in% c(1, 2, 3, 4)) {
  col_num <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  col_num <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

# Profitability ratios
ROA <- KOR_fs$'당기순이익'[, col_num] / KOR_fs$'자산'[, col_num]
CFO <- KOR_fs$'영업활동으로인한현금흐름'[, col_num] / KOR_fs$'자산'[, col_num]
ACCRUAL <- CFO - ROA

# Financial performance ratios
LEV <- KOR_fs$'장기차입금'[, col_num] / KOR_fs$'자산'[, col_num]
LIQ <- KOR_fs$'유동자산'[, col_num] / KOR_fs$'유동부채'[, col_num]
OFFER <- KOR_fs$'유상증자'[, col_num]

# Operating efficiency ratios
MARGIN <- KOR_fs$'매출총이익'[, col_num] / KOR_fs$'매출액'[, col_num]
TURN <- KOR_fs$'매출액'[, col_num] / KOR_fs$'자산'[, col_num]

# F-Score components
F_1 <- as.integer(ROA > 0)
F_2 <- as.integer(CFO > 0)
F_3 <- as.integer(ROA - KOR_fs$'당기순이익'[, col_num - 1] / KOR_fs$'자산'[, col_num - 1] > 0)
F_4 <- as.integer(ACCRUAL > 0)
F_5 <- as.integer(LEV - KOR_fs$'장기차입금'[, col_num - 1] / KOR_fs$'자산'[, col_num - 1] <= 0)
F_6 <- as.integer(LIQ - KOR_fs$'유동자산'[, col_num - 1] / KOR_fs$'유동부채'[, col_num - 1] > 0)
F_7 <- as.integer(is.na(OFFER) | OFFER <= 0)
F_8 <- as.integer(MARGIN - KOR_fs$'매출총이익'[, col_num - 1] / KOR_fs$'매출액'[, col_num - 1] > 0)
F_9 <- as.integer(TURN - KOR_fs$'매출액'[, col_num - 1] / KOR_fs$'자산'[, col_num - 1] > 0)

# Combine F-Score components
F_table <- data.frame(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9)

# Calculate F-Score
F_score <- rowSums(F_table, na.rm = TRUE)
names(F_score) <- KOR_ticker$종목명

# Plot F-Score distribution
F_dist <- prop.table(table(F_score)) %>% round(3)

F_dist_df <- as.data.frame(F_dist)

ggplot(F_dist_df, aes(x = F_score, y = Freq)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = paste0(Freq * 100, '%')), vjust = -0.4) +
  scale_y_continuous(expand = c(0, 0, 0, 0.05), labels = scales::percent) +
  ylab(NULL) +
  theme_classic()

# Select stocks with F-Score of 9
invest_F_score <- F_score == 9

# Extract information
F_score_stocks <- KOR_ticker[invest_F_score, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  mutate(`F-Score` = F_score[invest_F_score])

# F_score_stocks

## Construct quality portfolio using ranking of aggregate quality indices
# Specifically, we use ROE, gross profit, operating cash flow and choose stucks with lowest rank sum.

# First we calculate the quality measures

# Determine the correct column based on the date (avoid look ahead bias)
if (lubridate::month(Sys.Date()) %in% c(1, 2, 3, 4)) {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}

# Calculate quality measures
ROE <- KOR_fs$'당기순이익'[, num_col] / KOR_fs$'자본'[, num_col]
GPA <- KOR_fs$'매출총이익'[, num_col] / KOR_fs$'자산'[, num_col]
CFO <- KOR_fs$'영업활동으로인한현금흐름'[, num_col] / KOR_fs$'자산'[, num_col]

# Combine quality measures
quality_profit <- data.frame(ROE = ROE, GPA = GPA, CFO = CFO)

# Now we rank quality measures

# Rank in descending order
rank_quality <- quality_profit %>%
  mutate_all(list(~ min_rank(desc(.))))

# Correlation matrix
cor_matrix_quality <- cor(rank_quality, use = 'complete.obs')

# Plot correlation matrix
corrplot(cor_matrix_quality, method = 'color', type = 'lower',
         addCoef.col = 'black', number.cex = 1,
         tl.cex = 1, tl.srt = 0, tl.col = 'black',
         col = colorRampPalette(c('blue', 'white', 'red'))(200),
         mar = c(0, 0, 0.5, 0))

# Now we sum rankings and select top stocks

# Sum rankings
rank_sum <- rowSums(rank_quality, na.rm = TRUE)

# Identify top 30 quality stocks
invest_quality <- rank(rank_sum, na.last = "keep") <= 30

# Extract information
quality_stocks <- KOR_ticker[invest_quality, ] %>%
  select(종목코드, 종목명, 시가총액, 업종명) %>%
  cbind(round(quality_profit[invest_quality, ], 4))

# quality_stocks




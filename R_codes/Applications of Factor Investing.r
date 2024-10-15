# Chapter 10 - Applications of Quantitative Factor Investing

# Load necessary libraries
library(dplyr)
library(xts)
library(PerformanceAnalytics)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyverse)

# Set knitr options (adjust root directory as needed)
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
# knitr::opts_knit$set(root.dir = 'Your/Working/Directory/Path')

# Import data files
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")
KOR_price <- read.csv("KOR_price/KOR_3Yadjclose_combined.csv", row.names = 1)
KOR_ind <- read.xlsx("KOR_industry_cmp_xl.xlsx")

# Convert price data to xts object
KOR_price <- as.xts(KOR_price, order.by = as.Date(rownames(KOR_price)))

# Calculate returns
ret <- Return.calculate(KOR_price) %>% xts::last(252)

# Calculate 12-month returns
ret_12m <- ret %>% sapply(function(x) prod(1 + x) - 1)

# Select top 30 stocks based on momentum
invest_mom_12m <- rank(-ret_12m) <= 30

# Pad zeros in 'CMP_CD' column
KOR_ind$CMP_CD <- str_pad(KOR_ind$CMP_CD, 6, "left", pad = "0")

# Left join ticker and industry data
dt <- left_join(KOR_ticker, KOR_ind,
                by = c('종목코드' = 'CMP_CD',
                       '종목명' = 'CMP_KOR'))

# Compute sector-neutral z-scores
sector_neutral <- dt %>%
  select(종목코드, 종목명, SEC_NM_KOR) %>%
  mutate(ret = ret_12m, z_score = scale(ret)) %>%
  group_by(SEC_NM_KOR) %>%
  mutate(z_score_sector = scale(ret),
         z_score_sector = ifelse(is.na(SEC_NM_KOR), NA, z_score_sector))

# Rank using sector-neutral z-score
invest_mom_neutralized <- rank(-sector_neutral$z_score_sector) <= 30

# Ungroup data frame
sector_neutral_ungrouped <- ungroup(sector_neutral)

# Create overall rankings
sector_neutral_constraint <- sector_neutral_ungrouped %>%
  mutate(ranked = rank(-z_score_sector))

# Select top 5 stocks per sector
sector_neutral_constraint <- sector_neutral_constraint %>%
  group_by(SEC_NM_KOR) %>%
  slice_min(ranked, n = 5) %>%
  ungroup()

# Select top 30 stocks overall
sector_neutral_constraint <- sector_neutral_constraint %>%
  filter(rank(ranked) <= 30)

# Import valuation and financial statement data
KOR_value <- read.csv("KOR_valuation_clean.csv")
KOR_fs <- readRDS("KOR_fs_clean.Rds")

# Pad zeros in stock codes
KOR_value$X <- str_pad(KOR_value$X, 6, "left", pad = "0")

# Set the correct financial statement column based on the date
if (lubridate::month(Sys.Date()) %in% c(1,2,3,4)) {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 2))
} else {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(lubridate::year(Sys.Date()) - 1))
}
num_col <- num_col - 1

# Calculate GPA
gpa_score <- (KOR_fs[['매출총이익']][-1] / KOR_fs[['자산']][-1])[num_col]

# Create data frame with PBR and GPA
dt <- data.frame(
  stock_code = KOR_value$X,
  PBR = KOR_value$PBR,
  GPA = gpa_score[,1]
)

# Calculate Spearman correlation
correlation <- cor(dt$PBR, -dt$GPA, method = 'spearman', use = 'complete.obs')

# Calculate EBIT
magic_ebit <- (KOR_fs$"당기순이익"[-1] + KOR_fs$"법인세비용"[-1] + KOR_fs$"이자비용"[-1])[num_col]

# Calculate Market Value of Equity
magic_MV_equity <- KOR_value$PER * KOR_fs$'당기순이익'[num_col + 1]

# Calculate Net Debt
magic_debt <- KOR_fs$"부채"[num_col + 1]

# Calculate Excess Cash
magic_excess_cash1 <- (KOR_fs$'현금및현금성자산'[-1] - (KOR_fs$"유동자산"[-1] - KOR_fs$"유동부채"[-1]))[num_col]
magic_excess_cash1[magic_excess_cash1 < 0] <- 0
magic_excess_cash2 <- KOR_fs$현금및현금성자산[num_col + 1] - magic_excess_cash1

# Calculate Enterprise Value (EV)
magic_EV <- magic_MV_equity + magic_debt - magic_excess_cash2

# Calculate Earnings Yield
magic_EY <- magic_ebit / magic_EV

# Calculate Invested Capital (IC)
magic_ic <- ((KOR_fs$'유동자산'[-1] - KOR_fs$'유동부채'[-1]) +
               (KOR_fs$'비유동자산'[-1] - KOR_fs$'감가상각비'[-1]))[num_col]

# Calculate ROC
magic_roc <- magic_ebit / magic_ic

# Rank the stocks based on ROC and Earnings Yield
invest_magic <- (rank(rank(-magic_roc) + rank(-magic_EY))) <= 30

# Handle outliers
value_trim <- KOR_value %>% select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) < 0.01, NA, PBR),
         PBR = ifelse(percent_rank(PBR) > 0.99, NA, PBR))

value_winsorize <- KOR_value %>% select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) < 0.01, quantile(PBR, 0.01, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) > 0.99, quantile(PBR, 0.99, na.rm = TRUE), PBR))

# Combine multiple factors
ranked_values <- KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  gather()

# Multi-Factor Portfolio Construction
# Import all data sheets
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")
KOR_price <- read.csv("KOR_price/KOR_3Yadjclose_combined.csv", row.names = 1) %>% as.xts()
KOR_value <- read.csv("KOR_valuation_clean.csv")
KOR_fs <- readRDS("KOR_fs_clean.Rds")

# Pad zeros and set stock code column as row names
KOR_value$X <- str_pad(KOR_value$X, 6, side = "left", pad = "0")
rownames(KOR_value) <- KOR_value$X
KOR_value <- KOR_value %>% select(-X)

# Remove 'stockcode' column from financial statements
KOR_fs <- KOR_fs %>% lapply(function(x) {
  x %>% select(-stockcode)
})

# Set the correct financial statement column based on the date
if (month(Sys.Date()) %in% c(1,2,3,4)) {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(Sys.Date()) - 2))
} else {
  num_col <- str_which(colnames(KOR_fs[[1]]), as.character(year(Sys.Date()) - 1))
}

# Calculate quality measures
quality_roe <- (KOR_fs$'당기순이익' / KOR_fs$'자본')[num_col]
quality_gpa <- (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]
quality_cfo <- (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

# Combine the quality measures
quality_comb <- tibble(
  ROE = quality_roe[, 1],
  GPA = quality_gpa[, 1],
  CFO = quality_cfo[, 1]
)

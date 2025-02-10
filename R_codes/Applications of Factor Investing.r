# Load Required Libraries
library(dplyr)
library(xts)
library(PerformanceAnalytics)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(corrplot)


# Import Data Files
KOR_ticker <- read.xlsx("KOR_ticker_2023-04-20.xlsx")
KOR_price <- read.csv("KOR_price/KOR_3Yadjclose_combined.csv", row.names = 1)
KOR_ind <- read.xlsx("KOR_industry_cmp_xl.xlsx")

# Convert Price Data to XTS Object
KOR_price <- as.xts(KOR_price, order.by = as.Date(rownames(KOR_price)))

# Calculate Returns
ret <- Return.calculate(KOR_price) %>% xts::last(252)
ret_12m <- ret %>% sapply(function(x) prod(1 + x) - 1)

# Select Top 30 Stocks Based on Momentum (12m Returns)
invest_mom_12m <- rank(-ret_12m) <= 30

# Pad CMP_CD with Leading Zeros
KOR_ind$CMP_CD <- str_pad(KOR_ind$CMP_CD, 6, "left", pad= 0)

# Merge Ticker and Industry Data
dt <- left_join(KOR_ticker, KOR_ind, by = c('종목코드' = 'CMP_CD', '종목명' = 'CMP_KOR'))

# Count Stocks by Sector in Momentum Portfolio
dt[invest_mom_12m,] %>%
  select('SEC_NM_KOR') %>%
  group_by(SEC_NM_KOR) %>%
  summarize(n = n())

# Sector Distribution Visualization
dt[invest_mom_12m, ] %>%
  select(SEC_NM_KOR) %>%
  group_by(SEC_NM_KOR) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(SEC_NM_KOR, -n), y = n, label = n)) +
  geom_col() +
  geom_text(hjust = -0.5) +
  coord_flip() +
  theme_classic()

# Sector-Neutralizing Momentum Factor
sector_neutral <- dt %>% 
  select(종목코드, 종목명, SEC_NM_KOR) %>% 
  mutate(ret = ret_12m, z_score = scale(ret)) %>% 
  group_by(SEC_NM_KOR) %>% 
  mutate(z_score_sector = scale(ret), 
         z_score_sector = ifelse(is.na(SEC_NM_KOR), NA, z_score_sector))

# Rank Portfolio Using Sector-Neutralized Z-Score
invest_mom_neutralized <- rank(-sector_neutral$z_score_sector) <= 30

# Apply Maximum Constraint (5 stocks per sector)
sector_neutral_ungrouped <- ungroup(sector_neutral)
sector_neutral_constraint <- sector_neutral_ungrouped %>% 
  mutate(ranked = rank(-sector_neutral$z_score_sector)) %>%
  group_by(SEC_NM_KOR) %>%
  slice_min(ranked, n = 5) %>%
  ungroup()

# Final Top 30 Stocks After Constraint
sector_neutral_constraint <- sector_neutral_constraint[rank(sector_neutral_constraint$ranked) <= 30,]

# Sector Count in Final Portfolio
sector_neutral_constraint %>% 
  group_by(SEC_NM_KOR) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(SEC_NM_KOR, n), y = n, label = n)) +
  geom_col() +
  geom_text(hjust = -0.5) +
  coord_flip() +
  theme_minimal()

# Magic Formula: Earnings Yield Calculation
num_col <- ifelse(month(Sys.Date()) %in% c(1,2,3,4), 
                  str_which(colnames(KOR_fs[[1]]), as.character(year(Sys.Date()) - 2)), 
                  str_which(colnames(KOR_fs[[1]]), as.character(year(Sys.Date()) - 1))) - 1

magic_ebit <- (KOR_fs$"당기순이익"[-1] + KOR_fs$"법인세비용"[-1] + KOR_fs$"이자비용"[-1])[num_col]
magic_EV <- KOR_value$PER * KOR_fs$'당기순이익'[num_col + 1] + KOR_fs$"부채"[num_col + 1] - 
            (KOR_fs$'현금및현금성자산'[num_col + 1] - pmax(0, KOR_fs$"유동자산"[num_col + 1] - KOR_fs$"유동부채"[num_col + 1]))

magic_EY = magic_ebit / magic_EV

# Magic Formula: Return on Capital Calculation
magic_ic = ((KOR_fs$'유동자산'[-1] - KOR_fs$'유동부채'[-1]) +
            (KOR_fs$'비유동자산'[-1] - KOR_fs$'감가상각비'[-1]))[num_col]
magic_roc = magic_ebit / magic_ic

# Rank Portfolio Using Magic Formula
invest_magic <- (rank(rank(-magic_roc) + rank(-magic_EY))) <= 30

# Factor Portfolio Construction
factor_qvm <- cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate(across(everything(), ~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%
  rowSums()

invest_qvm = rank(factor_qvm) <= 30

# Visualization: Distribution of Multi-Factor Portfolio
cbind(quality_comb, invest_qvm) %>%
  pivot_longer(cols = -invest_qvm, names_to = "key", values_to = "value") %>%
  mutate(value = ifelse(percent_rank(value) > 0.95, NA, value),
         value = ifelse(percent_rank(value) < 0.05, NA, value)) %>%
  ggplot(aes(x = value, fill = as.factor(invest_qvm))) +
  geom_histogram(aes(y = ..density..), bins = 30) +
  facet_wrap(~ key, scales = "free", ncol = 1) +
  theme_minimal()

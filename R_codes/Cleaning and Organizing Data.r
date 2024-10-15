### Organize historical stock prices

# Import libraries
library(stringr)
library(xts)
library(magrittr) 
library(readxl)

# Import ticker excel. If csv file, remember to add back 0s in front of "종목코드" using below code
# KOR_ticker$'종목코드' =
# str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

KOR_ticker <- read_excel("KOR_ticker_2023-04-20.xlsx") 
head(KOR_ticker)

# Initialize a list to store the count of NA values for each stock
na_list <- list()

# Loop through each stock and count NA values in its price data
for (i in seq_len(nrow(KOR_ticker))) {
  name <- KOR_ticker[i, 1]
  na_count <- read.csv(paste0("KOR_price/", name, "_price.csv"), row.names = 1) %>%
    is.na() %>% sum()
  na_list[[i]] <- na_count
}

# Calculate total NA values across all stocks
total_na <- sum(unlist(na_list))
# Result: zero NA values


# Results: zero na values

# Initialize a list to store price data for each stock
price_list <- list()

# Loop through each stock to read and process its price data
for (i in seq_len(nrow(KOR_ticker))) {
  name <- KOR_ticker[i, 1]
  data <- read.csv(paste0("KOR_price/", name, "_price.csv"), row.names = 1)
  
  # Convert 'Date' column to Date format
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
  
  # Convert data to xts format
  price_list[[i]] <- as.xts(data)
}


# Combine all price data into one data frame
price_comb_list <- do.call(cbind, price_list)
colnames(price_comb_list) <- KOR_ticker$'종목코드'

# Use Last Observation Carried Forward to fill missing values
price_comb_list_filled <- na.locf(price_comb_list)

# Count total NA values after filling
total_na2 <- sum(is.na(price_comb_list_filled))
# Total NA values are due to stocks that were listed after August 2020

head(price_comb_list_filled[, 1:5])
tail(price_comb_list_filled[, 1:5])

# Save as csv
write.csv(data.frame(price_comb_list_filled), "KOR_price/KOR_3Yadjclose_combined.csv")


## Organize financial statements

# Import libraries
library(stringr)
library(magrittr)
library(dplyr)
library(readxl)


# Import tickers
KOR_ticker <- read_excel("KOR_ticker_2023-04-20.xlsx") 
head(KOR_ticker)

# Initialize a list to store financial statements
KOR_fs <- list()

# Loop through each stock to read its financial statements
for (i in seq_len(nrow(KOR_ticker))) {
  name <- KOR_ticker[i, '종목코드']
  data <- read_excel(paste0('KOR_fs/', name, '_fs.xlsx'))
  
  # Set row names manually
  row.names(data) <- data$...1
  KOR_fs[[i]] <- data
}


# Save vector of financial account names
fs_item <- rownames(KOR_fs[[1]])
length(fs_item)

## Organizing philosophy: Create one dataframe that contains a single financial account for all listed stocks
# For example, create a dataframe that contains all "매출액" data for all stocks

# Select "매출액" data for each stock
select_fs <- lapply(KOR_fs, function(x) {
  # If the item exists, select the data
  if ('매출액' %in% rownames(x)) {
    x[which(rownames(x) == '매출액'), ]
  } else {
    # If the item does not exist, create a data frame with NA
    data.frame(NA)
  }
})

# Bind the selected data into one data frame
KOR_revenue <- bind_rows(select_fs)
print(head(KOR_revenue))


# When looking at merged data, we see NA columns and NA data. There are mainly two reasons
# 1. Stock doesn't have data regarding that specific financial account
# 2. years aren't in order.

# Remove columns with names "." and "NA."
KOR_revenue <- KOR_revenue[, !colnames(KOR_revenue) %in% c('.', 'NA.')]

# Order columns by year
KOR_revenue <- KOR_revenue[, order(names(KOR_revenue))]

# Set stock codes as row names
rownames(KOR_revenue) <- KOR_ticker$'종목코드'

# Add a column for stock code (since tibbles do not support row names)
KOR_revenue <- tibble::rownames_to_column(KOR_revenue, var = "stockcode")

# Remove unwanted columns
KOR_revenue <- KOR_revenue %>% select(-"...1", -'X2019.12')
print(head(KOR_revenue))


## Handling NAs

# Identify rows where all values are NA (excluding the 'stockcode' column)
all_na_rows <- apply(KOR_revenue[, -1], 1, function(x) all(is.na(x)))

# Get indices of rows that are all NA
all_na_row_indices <- which(all_na_rows)

# Save tickers that have all NA values for "매출액"
Null_revenue <- KOR_revenue[all_na_row_indices, 'stockcode']

# Convert to character vector
Null_revenue <- Null_revenue$stockcode

# Get stock names corresponding to the tickers with NA values
Null_name <- KOR_ticker[match(Null_revenue, KOR_ticker$종목코드), "종목명"]

# Combine tickers and names into one data frame
Null_rev_dt <- cbind(Null_revenue, Null_name)

# Remove rows with all NA values from KOR_revenue
KOR_revenue <- KOR_revenue[-all_na_row_indices, ]


# Comment: For "매출액," financial stocks had "순영업수익" instead of 매출액... maybe convert later?

## Use a for loop to create dataframes for all stocks for each financial account

# Initialize a list to store data frames for each financial account
fs_list <- list()

# Loop through each financial account item
for (i in seq_len(length(fs_item))) {
  select_fs <- lapply(KOR_fs, function(x) {
    # If the item exists, select the data
    if (fs_item[i] %in% rownames(x)) {
      x[which(rownames(x) == fs_item[i]), ]
    } else {
      # If the item does not exist, create a data frame with NA
      data.frame(NA)
    }
  })
  
  # Bind the selected data into one data frame
  select_fs <- bind_rows(select_fs)
  
  # Remove columns with names "." and "NA."
  select_fs <- select_fs[, !colnames(select_fs) %in% c('.', 'NA.')]
  
  # Order columns by year
  select_fs <- select_fs[, order(names(select_fs))]
  
  # Set stock codes as row names
  rownames(select_fs) <- KOR_ticker$'종목코드'
  select_fs <- tibble::rownames_to_column(select_fs, var = "stockcode")
  
  # Remove unwanted columns if they exist
  if ('X2019.12' %in% colnames(select_fs)) {
    select_fs <- select_fs %>% select(-'X2019.12')
  }
  
  if ('...1' %in% colnames(select_fs)) {
    select_fs <- select_fs %>% select(-'...1')
  }
  
  # Save the data frame in the list
  fs_list[[i]] <- select_fs
}

# Name the list elements using financial item names
names(fs_list) <- fs_item

# Save the list as an RDS file
saveRDS(fs_list, 'KOR_fs_clean.Rds')



## Organize Valuation Ratios
library(stringr)
library(magrittr)
library(dplyr)
library(readxl)


KOR_ticker <- read_excel("KOR_ticker_2023-04-20.xlsx") 
head(KOR_ticker)

# Initialize a list to store valuation data
value_list <- list()

# Loop through each stock to read its valuation data
for (i in seq_len(nrow(KOR_ticker))) {
  name <- KOR_ticker[i, '종목코드']
  value_data <- read.csv(paste0('KOR_value/', name, '_valuation.csv'), row.names = 1) %>%
    t() %>% data.frame()
  value_list[[i]] <- value_data
}


# Bind all valuation data into one data frame
value_df <- bind_rows(value_list)
print(head(value_df))

# Remove unnecessary column and set stock codes as row names
value_df <- value_df %>% select(-X1)
rownames(value_df) <- KOR_ticker$'종목코드'


# Detect Inf values in the data frame
inf_values <- sum(is.infinite(value_df))
# Result: 14 infinity values

# Get row indices that contain Inf values
inf_row_indices <- which(apply(value_df, 1, function(x) any(is.infinite(x))))

# Replace Inf values with NA in all columns
value_df <- value_df %>%
  mutate_all(list(~na_if(., Inf)))

# Set stock codes as row names again (if necessary)
rownames(value_df) <- KOR_ticker$'종목코드'
print(head(value_df))


# Save as csv file
write.csv(value_df, 'KOR_valuation_clean.csv')
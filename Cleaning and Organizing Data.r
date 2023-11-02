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

# Count how many NA values price data contains
na_list = list()
for (i in seq_len(nrow(KOR_ticker))) {
  name = KOR_ticker[i , 1]
  na_list[[i]] = read.csv(paste0("KOR_price/", name, "_price.csv"), row.names = 1) %>%
    is.na() %>% sum()
}

# Count NA values
total_na <- sum(unlist(lapply(na_list, function(x) is.na(x))))

# Results: zero na values

# Start for loop to create a price list that contains all stock prices from 2020 august to 2023 august
price_list = list()
for (i in seq_len(nrow(KOR_ticker))) {
  name = KOR_ticker[i , 1]
  data = read.csv(paste0("KOR_price/", name, "_price.csv"), row.names = 1) 

    # convert date column (in character) to date formate
  data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

    # convert data to xts format
  price_list[[i]] <- data %>% as.xts()
}

# Merge into single data frame with stock codes as col names. Use last observation carried forward to fill in missing values
price_comb_list <- do.call(cbind, price_list) 
price_comb_list2 <- do.call(cbind, price_list) %>% na.locf()

total_na2 <- sum(unlist(lapply(price_comb_list2, function(x) is.na(x))))
# total NA values are 69016. This is mainly due to stocks that were listed after 2020 August.

colnames(price_comb_list) <- KOR_ticker$'종목코드'

head(price_list[, 1:5])
tail(price_list[, 1:5])

# save as csv

write.csv(data.frame(price_list_comb_list2), "KOR_price/KOR_3Yadjclose_combined.csv")


### Organize financial statements

# Import libraries
library(stringr)
library(magrittr)
library(dplyr)

# Import tickers
KOR_ticker <- read_excel("KOR_ticker_2023-04-20.xlsx") 
head(KOR_ticker)

KOR_fs = list()

for (i in seq_len(nrow(KOR_ticker))) {
  name = KOR_ticker[i, '종목코드']
  data <- read_excel(paste0('KOR_fs/', name,
                                 '_fs.xlsx'))
  # For read_excel, there is no row.names paramter like read.csv. Thus, use the following code to manually set row names
  row.names(data) <- data$...1
  KOR_fs[[i]] <- data
}

# Save vector of financial account names
fs_item = KOR_fs[[1]] %>% rownames()
length(fs_item)

## Organizing philosophy: Create one dataframe that contains a single financial account for all listed stocks
# For example, create a dataframe that contains all "매출액" data for all stocks
select_fs = lapply(KOR_fs, function(x) {
    # 해당 항목이 있을시 데이터를 선택
    if ( '매출액' %in% rownames(x) ) {
          x[which(rownames(x) == '매출액'), ]
      
    # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
      } else {
      data.frame(NA)
    }
  })

# rbind() doesn't work for dataframe that has different number of columns. Thus, use bind_rows() from dplyr.
KOR_revenue = bind_rows(select_fs)
print(head(KOR_revenue))

# When looking at merged data, we see NA columns and NA data. There are mainly two reasons
# 1. Stock doesn't have data regarding that specific financial account
# 2. years aren't in order.

# Subset data to remove NA columns
KOR_revenue = KOR_revenue[!colnames(KOR_revenue) %in%
                        c('.', 'NA.')]
KOR_revenue = KOR_revenue[, order(names(KOR_revenue))]
rownames(KOR_revenue) <- KOR_ticker$'종목코드'

# For tibble, rownames are not explicit as dataframe. Thus, add a column for stock code
KOR_revenue <- tibble::rownames_to_column(KOR_revenue, var = "stockcode")

# Extract "매출액" column and year 2019 col
KOR_revenue <- KOR_revenue %>% select(-"...1", -'X2019.12')
print(head(KOR_revenue))

## Handling NAs

# Find rows that are all NA. margin 1 refers to rows
all_na_rows <- apply(KOR_revenue[,-1], 1, function(x) all(is.na(x)))

# Get row indices that are all NA
all_na_row_indices <- which(all_na_rows)

# Save tickers that return NA values for "매출액"
Null_revenue <- KOR_revenue[all_na_row_indices, 'stockcode']

# Save it as a character vector
Null_revenue <- Null_revenue$stockcode

# Return stock names that have Null values for 매출액
Null_name <- KOR_ticker[match(Null_revenue, KOR_ticker$종목코드), "종목명"] 

# cbind 
Null_rev_dt <- cbind(Null_revenue, Null_name)

# After saving null_data, remove rows that are all NA
KOR_revenue <- KOR_revenue[-all_na_row_indices,]

# Comment: For "매출액," financial stocks had "순영업수익" instead of 매출액... maybe convert later?

## Use a for loop to create dataframes for all stocks for each financial account

fs_list = list()

for (i in 1 : length(fs_item)) {
  select_fs = lapply(KOR_fs, function(x) {
    # 해당 항목이 있을시 데이터를 선택
    if ( fs_item[i] %in% rownames(x) ) {
          x[which(rownames(x) == fs_item[i]), ]
      
    # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
      } else {
      data.frame(NA)
    }
  })

  # bind list data using bind_rows()
  select_fs = bind_rows(select_fs)

  # delete columns with name "." and "NA."
  select_fs = select_fs[!colnames(select_fs) %in%
                          c('.', 'NA.')]
  
  # order columns by year
  select_fs = select_fs[, order(names(select_fs))]

  # add stock code as rownames
  rownames(select_fs) <- KOR_ticker$'종목코드'
  select_fs <- tibble::rownames_to_column(select_fs, var = "stockcode")

  # Extract financial account column and year 2019 col using if statements

  if ('X2019.12' %in% colnames(select_fs)) {
    select_fs = select_fs %>% select(-'X2019.12')
  }

   if ('...1' %in% colnames(select_fs)) {
    select_fs = select_fs %>% select(-'...1')
  }
  
  
  # final save in list
  fs_list[[i]] = select_fs

}

# change list name to financial item names
names(fs_list) = fs_item

# Save as a list using saveRDS().
saveRDS(fs_list, 'KOR_fs_clean.Rds')


### Organize Valuation Ratios

library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker <- read_excel("KOR_ticker_2023-04-20.xlsx") 
head(KOR_ticker)

value_list = list()

for (i in seq_len(nrow(KOR_ticker))) {
  
  name = KOR_ticker[i, '종목코드']
  value_list[[i]] =
    read.csv(paste0('KOR_value/', name,
                    '_valuation.csv'), row.names = 1) %>%
    t() %>% data.frame()

}

# Use bind_rows() to bind list of dataframes
value_df = bind_rows(value_list)
print(head(value_df))

# Delete X1 column and change rownames to stock code
value_df = value_df %>% select(-X1)
rownames(value_df) <- KOR_ticker$'종목코드'

# Detect Inf values
apply(value_df, c(1, 2), is.infinite) %>% sum

# Result: 14 infinity values

# Return Row indices that contain Inf values
which(apply(value_df, 1, function(x) any(is.infinite(x))))

# Apply na_if() function to all columns to replace Inf with NA
value_df = value_df %>%
  mutate_all(list(~na_if(., Inf)))

rownames(value_df) <- KOR_ticker$'종목코드'
print(head(value_df))

# Save as csv file
write.csv(value_df, 'KOR_valuation_clean.csv')




### Comment on codes used

## `mutate_all()`
# The `mutate_all()` function is part of the `dplyr` package in R and is used to apply a function to all columns in a dataframe. 
# It returns a new dataframe with the modified columns.

## `list(~na_if(., Inf))`
# The `list(~na_if(., Inf))` part is essentially creating a list of function(s) to be applied to all columns. 
# The `~` is shorthand for defining a function in the `dplyr` package and `.` represents the column values that the function will be applied to.
# In this case, the function to be applied is `na_if(., Inf)`.

## `na_if(., Inf)`
#`na_if()` is a function in the `dplyr` package that replaces specified values in a vector with `NA`. 
# In this case, it's used to replace all instances of `Inf` (Infinity) in a column with `NA` (Missing Value).

## Putting It All Together

# When you combine all these components, `data_value %>% mutate_all(list(~na_if(., Inf)))` does the following:

# 1. Takes the `data_value` dataframe.
# 2. Pipes it into `mutate_all()`.
# 3. `mutate_all()` then applies the `na_if(., Inf)` function to each column in `data_value`.
# 4. All instances of `Inf` in all columns are replaced with `NA`.
# 5. A new dataframe with these changes is returned and stored back in `data_value`.
# In essence, this line of code replaces all instances of `Inf` in all columns of the `data_value` dataframe with `NA`.
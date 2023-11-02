# Elementary-Quantitative-Research-and-Portfolio-Management-on-R
This is a repository of codes and notebooks displaying my ongoing journey of completing the [Quant Cookbook](https://hyunyulhenry.github.io/quant_cookbook/) written by KAIST MFE professor LEE Hyunyul.

There are mainly four sections to this journey.

1. Data collection via web scraping (Completed)
2. Data cleaning & organizing (Completed)
3. EDA & data visualization (Ongoing)
4. Quantitative Research and Bactktesting, Portfolio Management, and Performance Measurement (Not Started)

## Data collection via web scraping
I have learned basics of web scraping, html navigation and collected varieties of financial data from different websites using POST and GET requests. Below are some of the tasks that I've done.

- Scrape basic stock data such as ticker data, stock name, and valuation ratios from krx.co.kr 
- Scrape Historical OHLC data of publicly listed stocks from NAVER finance using public API
- Scrape WISE sector classification data from fnguide.com
- Scrape financial statement data from fnguide.com
- Issue API key for using API of DART, the official website for publishing company filings, and scraping various company filings by learning to request data properly

## Data cleaning & organizing
I learned how to manipulate large sized dataframes mostly using **dplyr** and **tidyverse** packages. I also learned the importance of coming up with a philosophy for organizing/cleaning data before manipulation and transformation. Below are some of the tasks I have done. I also extensively used the **stringr** package to navigate data and implemented various ways of dealing with NA values and other errors.

- Combine individual csv files containing historical price data into a single data frame
- Combine individual csv files of valuation ratios into a single data frame
- Combine individual csv files of financial statements into a list consisted of data frames comparing financial items of all publicly listed stocks

## EDA & data visualization

## Quantitative Research and Bactktesting, Portfolio Management, and Performance Measurement


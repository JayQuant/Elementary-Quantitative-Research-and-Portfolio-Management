# How to View my Work
Please click on the links below to view the html page containing codes.

1. [Data collection via web scraping
](https://htmlpreview.github.io/?https://github.com/JayQuant/Elementary-Quantitative-Research-and-Portfolio-Management-on-R/blob/main/Financia-Data-Scraping%2C-Collection%2C-and-Organization.html)
2. [Data cleaning & organizing](https://htmlpreview.github.io/?https://github.com/JayQuant/Elementary-Quantitative-Research-and-Portfolio-Management-on-R/blob/main/Cleaning-Data.html)
3. [Basic Factor Investing](https://htmlpreview.github.io/?https://github.com/JayQuant/Elementary-Quantitative-Research-and-Portfolio-Management-on-R/blob/main/Elementary-Factor-Investing.html)
4. [Applied Factor Investing]()


# Elementary-Quantitative-Research-and-Portfolio-Management-on-R
This is a repository of codes and notebooks displaying my ongoing journey of completing the [Quant Cookbook](https://hyunyulhenry.github.io/quant_cookbook/) written by KAIST MFE professor LEE Hyunyul.

There are mainly three sections to this journey.

1. Data collection via web scraping (Completed)
2. Data cleaning & organizing (Completed)
3. Basic Factor Construction (Completed)
4. Applied Factor Investing (Completed)
5. Portfolio Theory (Ongoing)

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

## Quantitative Research and Bactktesting, Portfolio Management, and Performance Measurement

### Basic Factor Investing
I attempted to build long-only factor portfolios based on the following factors. I mainly used the `rank()` function to rank stocks based on factors and then screened the top 30 stocks to include in the long-only portfolio.

- Value (PBR)
- Time Series Momentum (12m returns based on daily returns + risk-adjusted returns)
- Quality (Based on F-score)
- Low-vol (Based on standard deviation of 12m returns; calculated using daily + weekly returns)

### Market Neutral Portfolios and Multi-Factor Portfolios
In this section, I explored methods to neutralize sector exposure by using a group_by-z-score method. That is, instead of applying a z-score to a raw feature, we group the feature by sectors and then apply z-score to each group. Then, we essentially force ourself to choose stocks whose *relative performance* within each sector are good.

Another simple yet effective method is to apply a hard constraint (e.g., five names per sector) to neutralize sector exposure.

Next, we investigated how Joel Greenblatt's magic formulate performed in the Korean markets.

Finally, We looked into the construction of multi-factor portfolio based on following factors.

1. Quality (ROE, GPA, ROC)
2. Value (PER, PCR, PBR, PSR)
3. Momentum (Short-term, Mid-term)

Naively ranking and aggregating the subsequent ranks cross-sectionally can be misleading. This problem is magnified especially when the range of ranks is widely different between features, since aggregation will lead to one feature influencing the overall forecast asymmetrically. To cope with this issue, we also apply standardization to the ranks themselves before doing cross-aggregation.

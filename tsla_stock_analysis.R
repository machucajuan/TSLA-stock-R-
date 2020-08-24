#TSLA stock analysis

#Read data from local CSV (source: yahoo finance)
library(tidyverse)
tsla <- read.csv("C:/Users/Juan/Downloads/TSLA.csv")
str(tsla)

#Change format from character(strings) to date 
tsla$Date <- as.Date(tsla$Date, "%Y-%m-%d")
summary(tsla)

#Build linear model
tsla_lm <- lm(Close ~ Date, data=tsla)

#Summary of statistics, p_value and testing 
summary(tsla_lm)

#Scatter plot using ggplot2 
library(ggplot2)
ggplot(tsla, aes(Date, Close))+
  geom_point() +
  geom_smooth() +
  ylab("TSLA Close Stock Value") +
  xlab("Date")

#LINEAR 
ggplot(tsla, aes(y=Close, x=Date))+
  geom_point() +
  geom_smooth(method="lm") +
  ylab("TSLA Close Stock Value") +
  xlab("Date")

#Predictive linear model 
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(tsla), 0.8*nrow(tsla))  # row indices for training data
trainingData <- tsla[trainingRowIndex, ]  # model training data
testData  <- tsla[-trainingRowIndex, ]   # test data

# Build the model on training data -
lm_mod_tsla <- lm(Close ~ Date, data=trainingData)  # build the model
price_pred <- predict(lm_mod_tsla, testData)  # predict price
summary(lm_mod_tsla)

#Another way using GetBatchSymbol
install.packages("BatchGetSymbols")
library(BatchGetSymbols)
# set dates
first.date <- Sys.Date() - 120
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('FB','TSLA','XOM','BA')

#Downnload stock data 
l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') )

#DF stock values 
df_stocks <- l.out$df.tickers
#Plot stocks 
p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)
df_stocks

s <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
s <- s + geom_point()
s <- s + geom_smooth(method="lm")
s <- s + facet_wrap(~ticker, scales = 'free_y') 
print(s)

b <- ggplot(df_stocks, aes(ref.date, y=price.close)) +
geom_boxplot()
b <- b + facet_wrap(~ticker, scales = 'free_y') 

#Summary statistics 
fb <- df_stocks[df_stocks$ticker=="FB",]
summary(fb)



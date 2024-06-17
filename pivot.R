library (readr)
library(dplyr)
library(tidyr)
library(lubridate)

#data extracted from https://www.kaggle.com/datasets/prasad22/daily-transactions-dataset

df<- read_csv("Daily Household Transactions.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))

unique(df$Mode)

df<-df %>% select(Date, Mode, Amount) %>% drop_na()

pivoted<-df %>% group_by(Mode) %>%
                     summarise(Freq = sum(Amount))

pivoted_with_dates<-df %>% group_by(Mode, year = lubridate::floor_date(Date, 'year')) %>%
                     summarise(Freq = sum(Amount))


pivoted_with_dates_wider<-df %>% group_by(Mode, year = lubridate::floor_date(Date, 'year')) %>%
  summarise(Freq = sum(Amount)) %>% pivot_wider(names_from = year, 
                                                values_from = Freq, values_fill = 0)  %>% ungroup() %>%  { x <- . 
                                                bind_rows(
                                                  x,
                                                  summarise(
                                                    x ,
                                                    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
                                                    across(where(is.character), ~"Total")
                                                  )
                                                )}
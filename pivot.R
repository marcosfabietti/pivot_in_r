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

#Load plot data

library(readr)
library(janitor)
library(tibble)

budget_plot_all<- read_csv("230707-def-exp-2023-TABLES-en.csv")

budget_plot_1<- budget_plot_all %>% slice(6:10)
budget_plot_1<-budget_plot_1 %>% 
  t %>% 
  as.data.frame %>%
  row_to_names(row_number = 1)%>% 
  mutate_if(is.character, as.numeric)

budget_plot_2<- budget_plot_all %>% slice(1:4)
budget_plot_2<-budget_plot_2 %>% 
  t %>%
  as.data.frame %>% 
  tibble::rownames_to_column("Year") %>% 
  row_to_names(row_number = 1)%>% 
  mutate_if(is.character, as.numeric)

budget_plot_3<- budget_plot_all[c(12:43),c(1:2)] %>%
  row_to_names(row_number = 1)%>% 
  mutate_at('DFS%', as.numeric) %>% 
  mutate(HL = ifelse(Country=='United Kingdom', "yes", "no")) #add plot label


# GG PLOTS
library(ggplot2)

#plot 1, ggplot, 2 line plots
#Title: UK is spending proportionally more on equipment and less on personnel
#y axis title: % of UK  defense spending
#blue and orange lines

ggplot() + 
  geom_line(data = budget_plot_1, aes(x = Spending, y=Equipment , color = "blue")) + #add line and colour
  geom_line(data = budget_plot_1, aes(x = Spending, y=Personnel , color = "orange") )+ #add line and colour
  labs(title = "UK is spending proportionally more on equipment and less on personnel") + #add title
  ylab("% of UK  defense spending") + #add y axis name
  ylim(c(0, 40))+ #change y axis scale
  scale_color_manual(name='Label',labels = c("Equipment", "Personnel"), values = c('blue','orange'))+ #add labels
  theme(legend.position="bottom") #change position

#plot 2, ggplot, bars with 2 line plots
#Title: Real defense spending remains constant over the last decade but as a % of GDP it is in decline  
#y axis title left: £(000s)
#y axis title right:%real GDP
#blue columns, red and orange lines



#plot 3, ggplot, bars with 1 line plot and 1 highlight (UK)
#Title: UK one of 10 NATO countries forecasted to meet the 2% in 2023
#y axis title: 2023 Defense spending as % real GDP
#grey columns, red lines

ggplot(budget_plot_3, aes(x=reorder(Country,-`DFS%`), y=`DFS%`, fill=HL)) + 
  geom_bar(stat = "identity") +
  labs(title = "UK one of 10 NATO countries forecasted to meet the 2% in 2023") + #add title
  ylab("2023 Defense spending as % real GDP") +#add y axis name
  ylim(c(0, 4.5))+ #change y axis scale
  theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=0.9)) +
  scale_fill_manual( values = c( "yes"="blue", "no"="gray" ), guide = FALSE ) + #add highlihgt
  geom_line(y=2, group=1, color='red')  #add line and colour


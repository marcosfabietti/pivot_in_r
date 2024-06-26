library (readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)
library(tibble)
library(ggplot2)
library(plotly)


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
# ggplot only allows a 2d axis as scale of the first, you must also scale the line plots by the same factor

ggplot(budget_plot_2, aes(x=Spending, y=`Real  UK defence spending`)) + 
  geom_bar(stat = "identity", fill='#5793EB') +
  geom_line( aes(x=Spending,y=`UK defence spending as % Real GDP`/0.00006), size=1, color='orange') +
  geom_line( y=2/0.00006, size=2, color='red') +
  labs(title = "Real defense spending remains constant over the last decade but as a % of GDP it is in decline") + #add title
  ylim(c(0, 50000))+ #change y axis scale
  theme(axis.title.x=element_blank())+
  theme(axis.text.x=element_text(angle=90, hjust=0.9))+
  scale_y_continuous(
    n.breaks = 15,
    # Features of the first axis
    name = "£(000s)",
    # Add a second axis (multiple of first one) and specify its features
    sec.axis = sec_axis(~.*0.00006, name="%real GDP")
  )

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

# PLOTLY 

#plot 1, ggplot, 2 line plots
#Title: UK is spending proportionally more on equipment and less on personnel
#y axis title: % of UK  defense spending
#blue and orange lines

fig <- plot_ly(budget_plot_1, x = ~Spending, y = ~Equipment,
               name = 'Equipment', type = 'scatter', mode = 'lines',line = list(color = 'blue'))
fig <- fig %>% add_trace(y = ~Personnel, name = 'Personnel',mode = 'lines',line = list(color = 'orange') )
fig <- fig %>% layout(title = "UK is spending proportionally more on equipment and less on personnel",
                      xaxis = list(title = "Year"),
                      yaxis = list(range=c(0,40),title = "% of UK  defense spending"))
fig


#plot 2, ggplot, bars with 2 line plots
#Title: Real defense spending remains constant over the last decade but as a % of GDP it is in decline  
#y axis title left: £(000s)
#y axis title right:%real GDP
#blue columns, red and orange lines
# ggplot only allows a 2d axis as scale of the first, you must also scale the line plots by the same factor

fig <- plot_ly(budget_plot_2, x = ~Spending, y = ~`Real  UK defence spending`,
               type = 'bar', name = 'Real  UK defence spending', marker = list(color = '#5793EB'))
fig <- fig %>% layout(title = "Real defense spending remains constant over the last decade but as a % of GDP it is in decline",
                      xaxis = list(title = "Year"),
                      yaxis = list(range=c(0,50000),title = "£(000s)"))
fig <- fig %>% add_trace(budget_plot_2, x = ~Spending, y = ~`UK defence spending as % Real GDP`,
                         yaxis = "y2",
                         name = 'UK defence spending as % Real GDP', type = 'scatter',  
                         mode = 'lines+markers', marker = list(
                           color = 'orange',size = 5,
                           line = list(
                             color = 'orange',
                             width = 1
                           )))
fig <- fig %>% add_trace(y=2, 
                         yaxis = "y2",
                         name = 'Target', type = 'scatter',  
                         mode = 'lines', marker = list(
                           color = 'red',size = 5,
                           line = list(
                             color = 'red',
                             width = 1
                           )))
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))

fig


#plot 3, ggplot, bars with 1 line plot and 1 highlight (UK)
#Title: UK one of 10 NATO countries forecasted to meet the 2% in 2023
#y axis title: 2023 Defense spending as % real GDP
#grey columns, red lines

fig <- plot_ly(budget_plot_3, x = ~reorder(Country,-`DFS%`), y = ~`DFS%`,
               type = 'bar',name='dfsp%', marker = list(color = ifelse(budget_plot_3$HL == 'yes', "blue", "grey")))
fig <- fig %>% layout(title = "UK one of 10 NATO countries forecasted to meet the 2% in 2023",
                      yaxis = list(range=c(0,4.5),title = "2023 Defense spending as % real GDP"))
fig <- fig %>% add_trace(y=2, 
                         name = 'Target', type = 'scatter',  
                         mode = 'lines', marker = list(
                           color = 'red',size = 5,
                           line = list(
                             color = 'red',
                             width = 1
                           )))
fig




## flagging

#Load and structure data

budget_plot_all<- read_csv("230707-def-exp-2023-TABLES-en.csv")

budget_plot_1<- budget_plot_all %>% slice(6:10)

budget_plot_1<-budget_plot_1 %>% 
  t %>% 
  as.data.frame %>%
  row_to_names(row_number = 1)%>% 
  mutate_if(is.character, as.numeric)

results <- data.frame(matrix(NA, nrow = nrow(budget_plot_1), 
                             ncol = length(colnames(budget_plot_1))))
names(results) <- names(budget_plot_1)

for (col in colnames(budget_plot_1)){
  for (j in 1:nrow(budget_plot_1)) {
    results[j,col]<-ifelse(budget_plot_1[j,col]>1.1* budget_plot_1[1,col] | budget_plot_1[j,col]< 0.9 * budget_plot_1[1,col],
                           'YES','NO')
  }
}

budget_plot_1 <- cbind(data=budget_plot_1,lables=results)
results_p<-  names (results)[ sapply( results, function(x) any (grepl("YES",x) ))]


for (names in results_p){
  
  fig <- plot_ly(budget_plot_1, x = ~Spending, y =  ~get(names),
                 name = names, type = 'scatter', mode = 'lines', line = list(size=5, color = 'blue'))
  
  fig <- fig %>% add_trace(budget_plot_1, x = ~Spending, y =  ~get(names),
                           name = 'outliers', type = 'scatter', mode = 'marker', 
                           marker = list(size= 10, color = ifelse(results[,names] == 'YES', "red", "blue")
                           ))
  
  fig <- fig %>% layout(title = "UK spending per area oustide a variation +/- 10 %",
                        xaxis = list(title = "Year"),
                        yaxis = list(title = "% of UK  defense spending"))
  print(fig)
}



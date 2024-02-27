## ev.sales.and.prices.r
library(tidyverse)
library(scales)

## Market share data
df = data.frame(year = as.character(rep(2015:2023, 2)),
                type = c(rep('ev', 9), 
                         rep('hev', 9)), 
                perc = c(0.3, 0.4, 0.5, 1.2, 1.3, 
                         1.6, 2.6, 5.2, 6.9, 
                         1.8, 2, 2.1, 2.5, 3.5, 
                         4, 5, 5.5, 8.3))

g = ggplot(df, 
       aes(x = perc, 
           y = year, 
           fill = type, 
           group = type,
           label = percent(perc/100))) + 
  geom_col(position = position_dodge(width = 0.9)) + 
  geom_text(aes(group = type), hjust = 0, 
            position = position_dodge(width = 0.9)) + 
  lims(x = c(0, 9)) + 
  labs(title = 'EV and HEV Market Share, 2015-2023')


## pubtheme
g %>% 
  pub(type = 'bar')

## Average sales price data

df = data.frame(date = c('2022-04-01', 
                         '2022-08-01', 
                         '2022-10-01', 
                         '2022-12-01', 
                         '2023-03-01', 
                         '2023-07-01'), 
                price = c(39974, 
                          46974, 
                          51974, 
                          55974, 
                          59974, 
                          49995))
df$date = as.Date(df$date)

g = ggplot(df, 
       aes(x = date, 
           y = price, 
           label = dollar(price))) + 
  geom_point() + 
  geom_line() + 
  geom_text(hjust = -0.1) + 
  scale_y_continuous(labels = dollar) +
  scale_x_date(limits = as.Date(c('2021-12-01', '2024-01-01'))) +
  lims(x = c(min(df$date), max(df$date) + 60)) + 
  labs


## pubtheme
g %>% 
  pub(type = 'line')

title = "F-150 Lightning Price Over Time" 
g = ggplot(df, 
           aes(x     = date, 
               y     = price, 
               label = dollar(price))) +
  geom_line() +
  geom_point() + 
  geom_text(hjust = -0.1) + 
  labs(title    = title,
       x     = 'Date', 
       y     = 'Price') 

g %>% 
  pub(type = 'line', 
      xlim = as.Date(c('2022-04-01', '2024-01-01'))) + 
  scale_y_continuous(labels = dollar, 
                     limits = c(0, 60000)) +
  scale_x_date(limits = as.Date(c('2022-04-01', '2023-10-01'))) 

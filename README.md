# working-with-dates-and-times-in-R
# working with dates and times
### make sure to install all the packages below. 
pacman::p_load(datasets, pacman,lubridate, tidyverse, stable,tsibble)
library(lubridate)

### Eustock market dataset
?EuStockMarkets
view(EuStockMarkets)

### saving the data in time series tibble 
data <-EuStockMarkets %>% 
  as_tsibble() %>% mutate(year = year(index), 
                          month = month(index), 
                          day = day(index), 
                          ) %>% 
  print()
glimpse(data)
 
### Graphing each index over time 
data %>% ggplot(mapping = aes(index, value, col = key)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = 'gam',
  formula = y~s(x, bs = "cs")) +
  labs(title = "EU stock indices", 
       x = "Date Time", 
       y = "Closing price")


### graphing growth by index

data %>% group_by_key()%>% 
  index_by(~max(data$index))%>% 
  summarize(ROI = max(value)/min(value)) %>%
  select(Index = key, ROI) %>%
  mutate(Index = fct_reorder(
    Index, ROI,
    .desc = T
  )) %>%
  ggplot(mapping = aes(x = Index, 
                       y= ROI, fill = Index)) +
  geom_text(aes(label = round(ROI, 2)), 
            vjust = -0.5) +
  geom_col() +
  theme(legend.position = "none")
  
  
  
  
  
  
  

  
  
  
  

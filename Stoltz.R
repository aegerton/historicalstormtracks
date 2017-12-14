library(tidyverse)
library(ggthemes)
library(ggmap)
library(htmlwidgets)
df = data3 %>%
  filter(Name != 'UNNAMED' & Name != 'SUBTROP1') %>%
  group_by(Season) %>%
  summarise(Distinct_Storms = n_distinct(Name))

p = ggplot(df, aes(x = Season, y = Distinct_Storms)) + theme_economist()
p + geom_line(size = 1.1) + 
  ggtitle("Number of Storms Per Year") + 
  geom_smooth(method='lm', se = FALSE) + 
  ylab("Storms")

pct.diff = function(x){round((x-lag(x))/lag(x),2)}
act.diff = function(x){round((x-lag(x)),2)}
df = data3 %>%
  arrange(Season) %>%
  filter(Name != 'UNNAMED' & Name != 'SUBTROP1') %>%
  group_by(Season) %>%
  summarise(Distinct_Storms = n_distinct(Name)) %>%
  mutate(Distinct_Storms_Change = act.diff(Distinct_Storms),
         Distinct_Storms_Pct_Change = pct.diff(Distinct_Storms)) %>%
  na.omit() %>%
  arrange(Season)
df$Season = factor(df$Season)
summary(df %>% select(-Season))

df = data3 %>%
  filter(Name != 'UNNAMED' & Name != 'SUBTROP1') %>%
  filter(grepl("H", CAT)) %>%
  group_by(Season,CAT) %>%
  summarise(Distinct_Storms = n_distinct(Name))
df$CAT = factor(df$CAT)

p = ggplot(df, aes(x = Season, y = Distinct_Storms, col = CAT)) + theme_economist()
p + geom_line(size = 1.1) + 
  scale_color_brewer(direction = -1, palette = "Spectral") + 
  ggtitle("Number of Storms Per Year By Category (H)") + 
  facet_wrap(~CAT, scales = "free_x") + 
  geom_smooth(method = 'lm', se = FALSE, col = 'black') +
  theme(axis.text.x = element_text(angle=90), legend.position = 'none') + 
  ylab('Storms')

df = data3 %>%
  arrange(Season) %>%
  filter(grepl("H", CAT)) %>%
  filter(Name != 'UNNAMED' & Name != 'SUBTROP1') %>%
  group_by(Season) %>%
  summarise(Distinct_Storms = n_distinct(Name)) %>%
  mutate(Distinct_Storms_Change = act.diff(Distinct_Storms),
         Distinct_Storms_Pct_Change = pct.diff(Distinct_Storms)) %>%
  na.omit() %>%
  arrange(Season)
summary(df %>% select(-Season))

df = data3 %>%
  filter(Name != 'UNNAMED' & Name != 'SUBTROP1') %>%
  filter(grepl("H", CAT)) %>%
  group_by(Season) %>%
  summarise(Distinct_Storms = n_distinct(Name)) %>%
  mutate(Distinct_Storms_Pct_Change = pct.diff(Distinct_Storms))

p = ggplot(df,aes(x = Distinct_Storms_Pct_Change)) + theme_economist()

p1 = p + geom_histogram(bins = 20) +
  ggtitle("YoY % Change Density") +
  scale_x_continuous(labels = scales::percent) +
  ylab('') + xlab('YoY % Change in Hurricanes')

p2 = p + geom_density(fill='darkgrey',alpha=0.5) +
  ggtitle("YoY % Change Density") +
  scale_x_continuous(labels = scales::percent) +
  ylab('') + xlab('YoY % Change in Hurricanes')

gridExtra::grid.arrange(p1,p2,ncol=2)

big_map <- get_googlemap(c(lon=165, lat=-25), zoom = 5, maptype = "terrain")
ggmap(big_map, extent='panel') + 
  geom_point(data = df, mapping = aes(x = Longitude, y = Latitude),col='red',alpha=0.25)

df = data3 %>% filter(grepl("H", CAT))
ggmap(big_map) + 
  geom_density_2d(data = df, mapping = aes(x = Longitude, y = Latitude), size = 0.5) + 
  stat_density2d(data = df, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.1, 
                 bins = 20, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
                                                                    guide = FALSE) + scale_alpha(range = c(0.1, 0.5), guide = FALSE) + 
  facet_wrap(~CAT)

df = data3 %>% filter(!grepl("H", CAT) & !grepl("W", CAT))
ggmap(big_map) + 
  geom_density_2d(data = df, mapping = aes(x = Longitude, y = Latitude), size = 0.5) + 
  stat_density2d(data = df, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.1, 
                 bins = 20, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 
                                                                    guide = FALSE) + scale_alpha(range = c(0.1, 0.5), guide = FALSE) + 
  facet_wrap(~CAT)

df = data3 %>% 
  filter(Pres(WMO) > 0) %>%
  filter(grepl("H", CAT)) %>%
  group_by(CAT,YEAR,MONTH,DAY,LAT,LONG) %>%
  summarise(MEAN_WIND_KTS = mean(WIND_KTS), MEAN_PRESSURE = mean(PRESSURE)) %>%
  arrange(MEAN_WIND_KTS)
df$CAT = factor(df$CAT)

p = ggplot(df,aes(x=MEAN_WIND_KTS, y = MEAN_PRESSURE, fill = CAT)) + theme_economist()
p + 
  geom_hex(alpha = 0.8) +
  scale_fill_brewer(direction = -1, palette = "Spectral") + 
  scale_y_continuous(labels = scales::comma)+ 
  theme(legend.position = 'right') + 
  ggtitle("Wind KTS vs. Pressure by Category (H)") 
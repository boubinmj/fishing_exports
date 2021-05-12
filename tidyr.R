library('dplyr')
library('tidyr')

billboard <- readr::read_csv("billboard.csv")
head(billboard)

str(billboard)

billboard2 <- billboard %>% 
  gather(key=week, value=rank, wk1:wk76, na.rm = TRUE)
head(billboard2)

billboard3 <- billboard2 %>% 
  mutate(
    week = readr::parse_number(week),
    date = date.entered + 7*(week-1)) %>% 
  select(-date.entered)
head(billboard3)

weather <- readr::read_csv("weather.csv")
head(weather)

weather2 <- weather %>% 
  gather(day, value, d1:d31, na.rm = TRUE) %>% 
  mutate(day=readr:parse_number(day))
head(weather2)

library(stringr)
bigfile %>% 
mutate(Moves =  str_trim(Moves)) %>%
  separate(col = Moves, into = paste0("M",as.character(0:20)),
           sep = "\\s*[:digit:]*\\.\\s*|\\s+") %>% 
  select(-M0) %>%
  drop_na() %>% 
  mutate(Open = paste(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10)) %>% 
  group_by(Open) %>% 
  summarize(count = n()) %>%
  ungroup() %>% 
  top_n(wt = count, n = 10) %>% 
  arrange(desc(count))  -> pop_openings

bigfile %>% 
  mutate(Moves =  str_trim(Moves)) %>%
  separate(col = Moves, into = paste0("M",as.character(0:20)),
           sep = "\\s*[:digit:]*\\.\\s*|\\s+") %>% 
  select(-M0) %>%
  mutate(Open = paste(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10)) %>% 
  select(Open, Date) %>% 
  drop_na() %>% 
  mutate(Date = as.numeric(substr(start = 1, stop = 4,x = Date))) %>%
  filter(Open %in% pop_openings$Open) %>% 
  group_by(Date, Open) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(Open) %>% 
  arrange(Date) -> pop_openings_dist
pop_openings_dist %>% 
  hchart(type = "line", hcaes(x = Date, y = count, group = Open),
       marker = list(radius = 0)) %>% 
  hc_tooltip(valueDecimals = 0) %>% 
  hc_xAxis(title = list(text = "Year"), crosshair = T,min = 1990) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_tooltip(shared = T)  %>%
  hc_add_theme(hc_theme_sandsignika())
  
pop_openings_dist %>%
  filter(Date > 1990) %>%
  spread(key = Open, value = count) %>%
  select(-Date) %>%
  drop_na() %>%
  chisq.test()
         
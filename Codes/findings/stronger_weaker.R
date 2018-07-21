bigfile %>% mutate(WElo = as.numeric(WElo),
                   BElo = as.numeric(BElo)) %>%
  select(WElo, BElo, Result) %>% 
  mutate(
    Ww = (Result == "1-0"),
    Wd = (Result == "1/2-1/2"),
    Wl = (Result == "0-1")
  ) %>%
  mutate(
    WRating = if_else(Ww, true = WElo, false = BElo),
    LRating = if_else(Ww, true = BElo, false = WElo)
  ) %>%
  select(Draw = Wd, WRating, LRating) %>% drop_na() %>% 
  mutate(StrongerStatus = if_else(Draw, "Draw", if_else(WRating > LRating, "Win", "Lose"))) %>% 
  select(StrongerStatus) %>% 
  group_by(StrongerStatus) %>% 
  summarize(count = n()) %>% 
  mutate(percent = round(count / sum(.$count), 3) * 100) %>% 
  hchart(type = "pie",
         hcaes(
           x = StrongerStatus,
           y = count,
           name = as.factor(StrongerStatus)
         ),
         tooltip = list(pointFormat ="count: {point.count} <br/> 
                           percent: {point.percent} % <br/>")
         ) %>%
  hc_title(text = "Stronger Person Results Ratio") %>%
  hc_add_theme(hc_theme_sandsignika())

##### Rating Difference Distribution
####################################
bigfile %>% mutate(WElo = as.numeric(WElo),
                   BElo = as.numeric(BElo)) %>%
  select(WElo, BElo, Result) %>% 
  mutate(
    Ww = (Result == "1-0"),
    Wd = (Result == "1/2-1/2"),
    Wl = (Result == "0-1")
  ) %>% 
  mutate(Wdif = WElo - BElo) %>% 
  mutate(WResult = if_else(Ww, "W", if_else(Wd, "D", "L"))) %>% 
  mutate(WResult = factor(WResult, levels = c("W", "D", "L"))) %>% 
  select(WResult, Wdif) %>% 
  mutate(Wdif = round(Wdif, -1)) %>% 
  group_by(WResult, Wdif) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(Wdif) %>% 
  mutate(percent = round(count / sum(count),3) * 100) %>% 
  ungroup() %>% 
  drop_na() %>% 
  hchart(type = "area", hcaes(x = Wdif, y = count, group = WResult),
         marker = list(radius = 0),
         tooltip = list(pointFormat ="<b>{point.WResult}</b> <br/> count: {point.count} <br/> 
                           percent: {point.percent} % <br/>")) %>% 
  hc_plotOptions(area = list(stacking = "percent")) %>% 
  hc_tooltip(valueDecimals = 0) %>% 
  hc_xAxis(title = list(text = "Match Rating"), crosshair = T, min = -600, max = 600) %>% 
  hc_yAxis(title = list(text = "Percent (%)"), max = 100) %>% 
  hc_tooltip(shared = T)  %>%
  hc_title(text = "Results Distribution by Rating Difference") %>%
  hc_add_theme(hc_theme_sandsignika())

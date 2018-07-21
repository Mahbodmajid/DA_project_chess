library(tidyr)
library(dplyr)
library(ggplot2)
library(highcharter)
bigfile %>% mutate(WElo = as.numeric(WElo),
                        BElo = as.numeric(BElo)) %>%
  select(WElo, BElo, Result) %>%
  mutate(
    Ww = (Result == "1-0"),
    Wd = (Result == "1/2-1/2"),
    Wl = (Result == "0-1")
  ) %>%
  mutate(matchElo = (WElo + BElo) / 2) %>%
  mutate(matchElo = round(matchElo, -2)) %>%
  group_by(matchElo) %>% 
  summarize(White_Winnigs = sum(Ww),
            White_Draws = sum(Wd),
            White_Loses = sum(Wl)) %>% 
  drop_na() -> color_sig
  
color_sig %>% filter(White_Winnigs+White_Draws+White_Loses > 10)%>%
  gather(Partition, Count, White_Winnigs:White_Loses) %>% 
  mutate(Partition = factor(Partition, levels = c("White_Winnings", "White_Draws", "White_Loses"))) %>% 
  hchart(type = "area", hcaes(x = matchElo, y = Count, group = Partition),
         marker = list(radius = 0)) %>% 
  hc_plotOptions(area = list(stacking = "percent")) %>% 
  hc_tooltip(valueDecimals = 0) %>% 
  hc_xAxis(title = list(text = "Match Rating"), crosshair = T) %>% 
  hc_yAxis(title = list(text = "Percent (%)"), max = 100) %>% 
  hc_title(text = "Match Rating effect on White's Result") %>%
  hc_tooltip(shared = T)  %>%
  hc_add_theme(hc_theme_sandsignika())

  color_sig %>% select(-matchElo)%>% chisq.test()
  
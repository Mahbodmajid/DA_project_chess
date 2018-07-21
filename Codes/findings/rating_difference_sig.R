library(tidyr)
library(dplyr)
library(ggplot2)

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
  ggpubr::ggscatter(x = "WRating", y = "LRating", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "Winner Rating", ylab = "Loser Rating",
                    color = "Draw", alpha = 0.5)+
  guides(color = F) +
  geom_abline(slope = 1, intercept = 0)
  
bigfile %>%
  mutate(WElo = as.numeric(WElo), BElo = as.numeric(BElo)) %>%
  select(WElo, BElo, Result) %>% 
  mutate(
    Ww = (Result == "1-0"),
    Wd = (Result == "1/2-1/2"),
    Wl = (Result == "0-1")
  ) %>%
  mutate(
    Bigger = if_else(WElo >= BElo, true = WElo, false = BElo),
    Smaller = if_else(WElo >= BElo, true = BElo, false = WElo)
  ) %>% 
  mutate(
    Bigger_w = ((WElo >= BElo) & Ww) | ((WElo <= BElo) & Wl),
    Bigger_d = Wd
  ) %>% 
  mutate(
    Bigger_l = !(Bigger_w | Bigger_d)
  ) %>% 
  mutate(difference = Bigger - Smaller) %>% 
  mutate(Bigger = round(Bigger, -2),
         difference = round(difference, -2)) %>%
  group_by(Bigger, difference) %>% 
  summarize(Bigger_w = sum(Bigger_w), Bigger_l = sum(Bigger_l), Bigger_d = sum(Bigger_d)) %>% 
  filter(difference == 200) %>% 
  filter((Bigger_w +  Bigger_l + Bigger_d) > 10)-> distribution_for_test


chisq.test(distribution_for_test %>% select(-Bigger, -difference))

distribution_for_test %>%
  mutate(Bigger_win_rate = (Bigger_w)/(Bigger_w+Bigger_l+Bigger_d)) %>% 
  select(Bigger, Bigger_win_rate) %>% 
  arrange(desc(Bigger)) %>% 
  ggplot(aes(x = Bigger, y = Bigger_win_rate))+
  geom_area()+
  ggtitle("Versus 200 Less in Rating Opponents")+
  xlab("Stronger Rating")+
  ylab("Win Rate")+
  ylim(c(0,1))

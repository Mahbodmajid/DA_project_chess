###########ATTENTION: uncomment to use original file
# meta_data_df <- read_delim(file = "integrated/meta",delim = "\t")
# df_rating <- read_delim(file = "integrated/rating",delim = "\t")
# 
# library(dplyr)
# 
# df_rating %>%
#   select(ID, R_TIME, RATING) %>%
#   inner_join(meta_data_df %>% select(ID, R_TIME),
#              by = c("ID", "R_TIME")) -> ID_RATING_TIME
# 
# ID_RATING_TIME %>%
#   group_by(ID) %>%
#   top_n(n = 1, wt = RATING) %>%
#   ungroup() %>% 
#   select(-RATING) %>%
#   inner_join(meta_data_df %>% 
#                select(ID, BDAY) %>%
#                filter(BDAY !="0000", !is.na(BDAY)) %>% 
#                unique()) -> BDAY_BEST_R_TIME
# 
# write_csv(x = BDAY_BEST_R_TIME,path = "integrated/BDAY_BEST_R_TIME.csv")
# 
# BDAY_BEST_R_TIME <- read_csv("integrated/BDAY_BEST_R_TIME.csv")
# 
# BDAY_BEST_R_TIME %>%
#   mutate(age = as.numeric(format.Date(R_TIME, "%Y")) - as.numeric(BDAY)) %>% 
#   .$age -> ages
# ages[(ages < 100) &(!is.na(ages))& ages > 0] -> best_ages
# 
# table(best_ages) %>% 
#   as.data.frame() %>% 
#   mutate(best_ages = as.numeric(best_ages)) -> best_ages_table
# 
# write_csv(best_ages_table, path = "integrated/BEST_AGES_TABLE.csv")

best_ages_table <- read_csv("integrated/BEST_AGES_TABLE.csv")


best_ages_table %>% 
  mutate(percent = round(Freq/sum(Freq),3)* 100) %>% 
hchart(type = "area", hcaes(x = best_ages, y = Freq),
       marker = list(radius = 0),
       tooltip = list(pointFormat =
                        "Count: {point.Freq}<br/> 
                         Percent: {point.percent}%<br/>")
       ) %>% 
  hc_tooltip(valueDecimals = 0) %>% 
  hc_xAxis(title = list(text = "Age"), crosshair = T) %>% 
  hc_yAxis(title = list(text = "Count")) %>% 
  hc_title(text = "Best Age in Chess Players' Career") %>% 
  hc_tooltip(shared = T)  %>%
  hc_add_theme(hc_theme_sandsignika())
                            
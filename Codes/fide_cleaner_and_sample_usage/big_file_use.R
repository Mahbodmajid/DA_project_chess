meta_data_df <- read_delim(file = "../integrated/meta",delim = "\t")
df_rating <- read_delim(file = "../integrated/rating",delim = "\t")

#df_rating %>% arrange(-RATING) %>% head()
#meta_data_df %>% filter(ID == 5000017)
#df_rating %>% filter(ID == 1503014) %>% select(RATING, R_TIME) -> rating_over_time
# 
# IDs <- c(5000017, 1503014)
# df_rating %>%select(-K) %>% filter(ID %in% IDs) -> rating_over_time
# meta_data_df %>% filter(ID %in% IDs) %>% select(ID, NAME) %>% unique -> id_names
# left_join(rating_over_time, id_names) -> result
# 
# result %>% arrange(R_TIME) %>% 
#   hchart("line", hcaes(x = R_TIME, y = RATING, group = NAME)) %>% 
#   hc_xAxis(title = list(text = "Time")) %>%
#   hc_yAxis(title = list(text = "Fide Rating"))  %>%
#   hc_tooltip(crosshairs = T) %>%
#   hc_add_theme(hc_theme_sandsignika())


library(highcharter)
draw_time_series <- function(IDs, shared,start = as.Date("1900-05-01"), end = as.Date("2030-05-01")){
  df_rating %>%select(-K) %>% filter(ID %in% IDs, R_TIME <= end, R_TIME >= start) -> rating_over_time
  meta_data_df %>% filter(ID %in% IDs) %>% select(ID, NAME) %>% unique -> id_names
  left_join(rating_over_time, id_names) -> result
  
  return(result %>% arrange(R_TIME) %>% 
    hchart("line", hcaes(x = R_TIME, y = RATING, group = NAME)) %>% 
    hc_xAxis(title = list(text = "Time")) %>%
    hc_yAxis(title = list(text = "FIDE Rating"))  %>%
    hc_tooltip(crosshairs = T, shared = shared, sort = T) %>%
    hc_add_theme(hc_theme_sandsignika()))
}
draw_time_series(c(5000017, 1503014), T)

meta_data_df %>% filter(R_TIME == as.Date("2018-05-01")) -> current_players

library(countrycode)
current_players %>%
  group_by(FED) %>%
  summarize(value = n()) %>%
  mutate(FED = substr(FED, nchar(FED) - 2, nchar(FED))) %>% 
  rename(Abrv = FED) %>% 
  full_join(country_codes_df) %>% 
  mutate(Abrv = countrycode(Full, "country.name", "iso2c" )) %>% 
  na.omit() %>% 
  rename(`hc-key` = Abrv)-> by_country 

hcmap("custom/world-robinson", 
      data = by_country, 
      value = "value",
      joinBy = c("iso-a2", "hc-key"), ## edited was wrong
      name = "No. of FIDE chess players",
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = " players")) 

iranians <- current_players %>% filter(FED == "IRI") %>% select(ID) %>% unique()
top_iranians <- df_rating %>% filter(R_TIME == as.Date("2018-05-01"),
                                     ID %in% iranians$ID) %>% top_n(n = 15, wt = RATING) %>% .$ID %>% unique()

draw_time_series(top_iranians, T, start = as.Date("2010-01-01"))

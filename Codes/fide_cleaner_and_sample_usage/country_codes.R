read_lines("../country_codes/country_codes.txt") -> country_codes

flatten_quick <-  function(list) {
  unlist(lapply(list, stri_flatten), use.names = FALSE)
}
rename(country_codes %>%
  trimws() %>% 
  flatten_quick() %>% 
  as.data.frame(stringsAsFactors = F), name = `.`) %>%
  mutate(Abrv = substr(name, 1, 3), Full = substr(name, 4, nchar(name))) %>% 
  select(-name) %>% 
  mutate(Full = trimws(Full)) -> country_codes_df

write_csv(country_codes_df, "../country_codes/country_codes.csv")

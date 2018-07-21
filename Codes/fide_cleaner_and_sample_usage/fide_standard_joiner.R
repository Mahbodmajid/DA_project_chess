#setwd("../fide_standard_good/")

all_files <- list.files(pattern = ".[Cc][Ss][Vv]")
all_files %>% substr(1,5) -> all_dates

df <- read_delim(file = all_files[length(all_files)], delim = "\t")
df

colnames(df) <- colnames(df) %>% toupper()
df %>% colnames() -> cnames
cnames[cnames %in% all_dates] -> cur_date
cur_date_as_date <- 
  as.Date(
    paste0(
      "01",
      "/",
      toupper(substr(cur_date, 1, 1)),
      tolower(substr(cur_date, 2, 3)),
      "/",
      substr(cur_date, 4, 5)
    ),
  "%d/%b/%y")

meta_data_df <- df %>% select(ID, NAME, SEX, FED, BDAY, FLAG, TIT, GMS) %>% 
  mutate(R_TIME = cur_date_as_date)


df %>% select(ID, K, contains(cur_date)) %>% 
  gather("R_TIME","RATING", cur_date, 3)  -> df_rating
df_rating$R_TIME <- cur_date_as_date
df_rating

mixer <- function(Year_num){
  new_df <- read_delim(file = Year_num, delim = "\t")
  
  colnames(new_df) <- colnames(new_df) %>% toupper()
  substr(Year_num, 1, 5) -> cur_date
  cur_date_as_date <- 
    as.Date(
      paste0(
        "01",
        "/",
        toupper(substr(cur_date, 1, 1)),
        tolower(substr(cur_date, 2, 3)),
        "/",
        substr(cur_date, 4, 5)
      ),
      "%d/%b/%y")
  
  meta_data_new_df <- new_df %>% select(ID, NAME, SEX, FED, BDAY, FLAG, TIT, GMS) %>% 
    mutate(R_TIME = cur_date_as_date)
  
  
  new_df %>% select(ID, K, contains(cur_date)) %>% 
    gather("R_TIME","RATING", cur_date, 3)  -> new_df_rating
  
  new_df_rating$R_TIME <- cur_date_as_date
  
  meta_data_df <<- rbind(meta_data_new_df, meta_data_df)
  df_rating <<- rbind(new_df_rating,df_rating)
  
} #end of function
# curdates <- c()
# containing <- c()
# mixer <- function(Year_num){
#   new_df <- read_delim(file = Year_num, delim = "\t")
#   substr(Year_num, 1, 5) -> cur_date
#   c(curdates, cur_date) ->> curdates
#   c(containing, new_df %>% select(contains(cur_date)) %>% colnames()) ->> containing
# }
# cbind(curdates, containing)
invisible(mapply(mixer, all_files[-length(all_files)]))

filename <- "rating"
if (file.exists(filename)) {unlink(filename)}
write_delim(x = df_rating, path = filename, delim = "\t")
filename <- "meta"
if (file.exists(filename)) {unlink(filename)}
write_delim(x = meta_data_df, path = filename, delim = "\t")
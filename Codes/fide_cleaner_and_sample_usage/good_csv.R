library(dplyr)
library(knitr)
library(stringi)
library(data.table)
library(readr)
library(tidyr)

Year_num <- list.files(pattern = ".[Cc][Ss][Vv]")

df <- read_delim(file = all_files[length(all_files)], delim = "\t")
colnames(df) <- trimws(colnames(df))
as.data.frame(apply(df,2,function(x)trimws(x)), stringsAsFactors = F) -> df
colnames(df) <- colnames(df) %>% toupper()
colnames(df)[colnames(df) == "B-DAY"] <- "BDAY"
df %>% head() %>% View()
if(!("BDAY" %in% colnames(df))){
  df %>% mutate(BDAY = NA) -> df
}
df %>% select(BDAY) %>% unique()

All_files <- function(Year_num){
  
  df <- read_delim(file = Year_num, delim = "\t")
  
  colnames(df) <- trimws(colnames(df))
  as.data.frame(apply(df,2,function(x)trimws(x)), stringsAsFactors = F) -> df1
  
  colnames(df1) <- colnames(df1) %>% toupper()
  colnames(df1)[colnames(df1) == "ID NUMBER"] <- "ID"
  colnames(df1)[colnames(df1) == "ID_NUMBER"] <- "ID"
  colnames(df1)[colnames(df1) == "BIRTHDAY"] <- "BDAY"
  colnames(df1)[colnames(df1) == "B-DAY"] <- "BDAY"
  colnames(df1)[colnames(df1) == "B_DAY"] <- "BDAY"
  colnames(df1)[colnames(df1) == "BORN"] <- "BDAY"
  
  colnames(df1)[colnames(df1) == "TITLE"] <- "TIT"
  colnames(df1)[colnames(df1) == "COUNTRY"] <- "FED"
  
  colnames(df1)[colnames(df1) == "GAMES"] <- "GMS"
  colnames(df1)[colnames(df1) == "GAME"] <- "GMS"
  colnames(df1)[colnames(df1) == "GM"] <- "GMS"
  
  colnames(df1)[colnames(df1) == "RATING"] <- toupper(substr(Year_num, 1, 5))
  
  if("V1" %in% colnames(df1)){
    df1 %>% select(-V1) -> df1
  }
  if(!("SEX" %in% colnames(df1))){
    df1 %>% mutate(SEX = NA) -> df1
  }
  if(!("K" %in% colnames(df1))){
    df1 %>% mutate(K = NA) -> df1
  }
  if(!("BDAY" %in% colnames(df1))){
    df1 %>% mutate(BDAY = NA) -> df1
  }
  
  filename <- paste0("../fide_standard_good/", Year_num)
  if (file.exists(filename)) {unlink(filename)}
  write_delim(x = df1, path = filename, delim = "\t")
} #end of function

invisible(mapply(All_files, Year_num))


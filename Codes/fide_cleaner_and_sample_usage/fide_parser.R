library(dplyr)
library(knitr)
library(stringi)
library(data.table)
library(readr)

Year_num <- list.files(pattern = "[Ff][Rr][Ll].[Tt][Xx][Tt]")

readChar("JAN01FRL.TXT", 300)

##########


read.csv2("JAN01FRL.TXT", header=TRUE, sep="")%>%
  head()%>%
  kable()

#########

df <- readLines("JAN01FRL.TXT")
head(df)


#########

column_vector<- gsub("ID NUMBER", "ID_NUMBER", toupper(df[1]))
indexes <- rep(0, nchar(column_vector))

column_vector


#########

for(i in 1:nchar(column_vector)){
  index = grep("\\s[A-z]", substr(column_vector, i, i+1))
  if (identical(index, integer(0)) == TRUE){indexes[i] = 0}
  else {indexes[i] = 1}
}

indexes = which(indexes == 1)

indexes



#best function

flatten_quick <-  function(list) {
  unlist(lapply(list, stri_flatten), use.names = FALSE)
}

df <- df%>%
  strsplit(split = "")%>%
  lapply(function(x) replace(x, indexes, c(" ", rep("\t", length(indexes)-1))))%>%
  flatten_quick()

head(df)

########

filename <- paste0(toupper(substr("JAN01FRL.TXT", 
                                 nchar("JAN01FRL.TXT")-11, 
                                 nchar("JAN01FRL.TXT")-7)), 
                  ".csv")

if (file.exists(filename)) {unlink(filename)}
sink(filename, append = TRUE)
invisible(sapply(df, function(x) cat(x, "\n")))
sink()


########
read_delim("JAN01.csv", delim = "\t") %>% head()

#######

######## LARGER DATA
mydata <- readLines("standard_jan18frl.txt")

column_vector<- gsub("ID NUMBER", "ID_NUMBER", toupper(mydata[1]))
indexes <- rep(0, nchar(column_vector))

for(i in 1:nchar(column_vector)){
  index = grep("\\s[A-z]", substr(column_vector, i, i+1))
  if (identical(index, integer(0)) == TRUE){indexes[i] = 0}
  else {indexes[i] = 1}
}

indexes = which(indexes == 1)

mydata_2 <- mydata %>%
  strsplit(split = "")%>%
  lapply(function(x) replace(x, indexes, "\t"))%>%
  flatten_quick()

mydata_2[1]

filename <- paste0(toupper(substr("standard_jan18frl.txt", 
                                 nchar("standard_jan18frl.txt")-11, 
                                 nchar("standard_jan18frl.txt")-7)), 
                  ".csv")

if (file.exists(filename)) {unlink(filename)}
sink(filename, append = TRUE)
invisible(sapply(mydata_2, function(x) cat(x, "\n")))
sink()

View(read_delim("JAN18.csv",delim =  "\t")%>%
  head())


#############

All_files <- function(Year_num){
  
  df <- readLines(Year_num)
  
  
  #########################################################################################################
  
  #Reformat individual columns
  
  
  #Reformat October 2002
  if(Year_num == "OCT02FRL.TXT"){df[1] <- df[1]%>%
    gsub("COUNTRY", "  Fed ", .)%>%
    gsub("GAMES", "Gms", .)%>%
    gsub("BIRTHDAY", " BIRTHDAY", .)}
  
  
  #Reformat April 2003
  else if(Year_num == "APR03FRL.TXT"){df[1] <- df[1]%>%
    gsub("   CODE  ","ID_NUMBER",.)%>%
    gsub("COUNTRY","  FED  ",.)%>%
    gsub(" APR03", "APR03 ", .)%>%
    gsub(" GAMES", "GMS   ", .)%>%
    gsub("  BIRTHDAY", "BIRTHDAY  ", .)%>%
    gsub(" FLAG", "FLAG ", .)}
  
  
  #Insert column names for July 2003, October 2003, January 2004, April 2005
  else if(Year_num %in% c("jul03frl.txt","OCT03FRL.TXT", "JAN04FRL.TXT", "APR05FRL.TXT") && nchar(df[1]) != 0) {
    cat("", df, file = Year_num, sep = "\n")
    df <- readLines(Year_num)
    df[1] <- "ID_NUMBER NAME                            TITLE FED  RATING GM  Bday      Flag "
  }
  
  if(Year_num %in% c("jul03frl.txt","OCT03FRL.TXT", "JAN04FRL.TXT", "APR05FRL.TXT") && nchar(df[1]) == 0) {
    df <- readLines(Year_num)
    df[1] <- "ID_NUMBER NAME                            TITLE FED  RATING GM  Bday      Flag "
  }
  
  
  #Reformat April, July, October 2004, January, July 2005
  else if(Year_num %in% c("APR04FRL.TXT",
                          "JUL04FRL.TXT",
                          "OCT04FRL.TXT",
                          "JAN05FRL.TXT",
                          "JUL05FRL.TXT")){df[1] <- df[1]%>%
                            gsub("COUNTRY", "   FED ", . )%>%
                            gsub("GAMES", "GAME ", .)%>%
                            gsub(" BIRTHDAY", "BIRTHDAY", .)}
  
  
  #Reformat January 2006 to July 2012
  else if(Year_num == "JAN06FRL.TXT"){df <- df[-2]}
  else if(Year_num %in% c("APR06FRL.TXT", "JUL06FRL.TXT", "OCT06FRL.TXT",
                          list.files(pattern = "[0][7-9][Ff][Rr][Ll].[Tt][Xx][Tt]"),
                          list.files(pattern = "[1][0-1][Ff][Rr][Ll].[Tt][Xx][Tt]"),
                          list.files(pattern = "[1][2][Ff][Rr][Ll].[Tt][Xx][Tt]")[1:5])){
    df[1] <- df[1] %>% gsub("Titl", "Tit ", .)%>%
      gsub("Games", "Game ", .)%>%
      gsub("July", "Jul", .)
  }
  
  #########################################################################################################
  
  #Get indexes
  column_vector<- gsub("ID NUMBER", "ID_NUMBER", toupper(df[1]))
  indexes <- rep(0, nchar(column_vector))
  for(i in 1:nchar(column_vector)){
    index = grep("\\s[A-z]", substr(column_vector, i, i+1))
    if (identical(index, integer(0)) == TRUE){indexes[i] = 0}
    else {indexes[i] = 1}
  }
  indexes = which(indexes == 1)
  
  #delim
  df <- df%>%
    strsplit(split = "")%>%
    lapply(function(x) replace(x, indexes, "\t"))%>%
    flatten_quick()
  
  #Export as csv
  filename <- paste0(toupper(substr(Year_num,nchar(Year_num)-11, nchar(Year_num)-7)), ".csv")
  if (file.exists(filename)) {unlink(filename)}
  sink(filename, append = TRUE)
  invisible(sapply(df, function(x) cat(x, "\n")))
  sink()
  
} #end of function

invisible(mapply(All_files, Year_num))

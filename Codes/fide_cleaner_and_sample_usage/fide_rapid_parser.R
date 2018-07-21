library(dplyr)
library(knitr)
library(stringi)
library(data.table)
library(readr)

Year_num <- list.files(pattern = "[Ff][Rr][Ll].[Tt][Xx][Tt]")


#best function

flatten_quick <-  function(list) {
  unlist(lapply(list, stri_flatten), use.names = FALSE)
}
#############

All_files <- function(Year_num){
  
  df <- readLines(Year_num)
  
  #Get indexes
  column_vector<- gsub("ID NUMBER", "ID_NUMBER", toupper(df[1]))
  indexes <- rep(0, nchar(column_vector))
  for(i in 1:nchar(column_vector)){
    index = grep("\\s[A-z]", substr(column_vector, i, i+1))
    if (identical(index, integer(0)) == TRUE){indexes[i] = 0}
    else {indexes[i] = 1}
  }
  indexes = which(indexes == 1)
  
  #Insert delimeters at indexes
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


#Exceute below to begin 20 minute long process.

invisible(mapply(All_files, Year_num))

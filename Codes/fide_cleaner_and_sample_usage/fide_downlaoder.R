#setwd("fide/")


year_vector <- as.character(1:as.numeric(substr(Sys.Date(), 3, 4)))
month_vector <- tolower(substr(month.name, 1, 3))
url_vector = url_vect_destfile = rep(0,length(month_vector)*length(year_vector))


latest <- format(Sys.Date(), format="%b%Y")
latest <- paste(tolower(substr(latest, 1, 1)), substr(latest, 2, 3), substr(latest, nchar(latest)-1, nchar(latest)), "frl.zip", sep = "")

latest

year_vector

for(i in 1:length(year_vector)){
  if(nchar(year_vector[i]) == 1){
    year_vector[i] = paste("0", year_vector[i], sep = "")
  }
}

year_vector

for(i in seq_along(year_vector)){
  for(j in seq_along(month_vector)){
    if(12*(i-1)+j <= 140){
      url_vector[12*(i-1)+j] = paste("http://ratings.fide.com/download/", month_vector[j], year_vector[i], "frl.zip", sep = "")
    }
    else if(12*(i-1)+j > 140){
      url_vector[12*(i-1)+j] = paste("http://ratings.fide.com/download/standard_", month_vector[j], year_vector[i], "frl.zip", sep = "")
    }
    url_vect_destfile[12*(i-1)+j] <- substr(url_vector[12*(i-1)+j], nchar(url_vector[12*(i-1)+j])- 11 , nchar(url_vector[12*(i-1)+j]))
  }
}

url_vect_destfile <- url_vect_destfile[1:which(url_vect_destfile == latest)]
url_vector <- url_vector[1:which(url_vect_destfile == latest)]


library(knitr)
library(dplyr)
library(downloader)


data.frame(URL = url_vector,
           Destination = url_vect_destfile)%>%
  slice(c(1:5, (n()-4):n()))%>% #first 5 and last 5 observations
  kable()

rm(list=setdiff(ls(), c("url_vector", "url_vect_destfile")))

download_all <- function(link, dest){
  if (!file.exists(dest)) {
    tryCatch({
      download.file(link, dest, method="auto") 
    }, error=function(e){})
  }
}

old <- getOption("warn"); options(warn = -1)
invisible(mapply(download_all, url_vector, url_vect_destfile))
options(warn = old)


invisible(sapply(list.files(pattern = "*.zip"), function(x) unzip(x, exdir = getwd())))

unlink(list.files(pattern = "*.zip"))

list.files(pattern = "*.txt")%>%
  head()


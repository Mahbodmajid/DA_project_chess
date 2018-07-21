no_lines <- 134342951

chunk_size <-  1000000
read_size <- 10000

indexes <- 1:(no_lines/chunk_size)
files <- list()
library(readr)
for(i in indexes){
    read_csv(paste0("chunked_data/chunk_",i,".csv")) -> files[[i]]
    files[[i]]$Round = as.character(files[[i]]$Round)
}
bind_rows(files) -> new_file
new_file %>% View()
write_csv(new_file, path = "chunk_integrated/file.csv")

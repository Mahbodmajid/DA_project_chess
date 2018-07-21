library(readr)
library(dplyr)
library(stringr)

#setwd("Desktop/96-97-2/Data Analysis/project")
no_lines <- 134342951

pgn_cleaner <- function(data) {
  data %>% str_which("\\[Event") -> starting_points
  new_game <- list()
  
  for (i in 1:(length(starting_points) - 1)) {
    start <- starting_points[i]
    end <- starting_points[i + 1] - 1
    game <- data[start:end]
    myregex <- "\\[\\w+ \\\"(.+)\\\"\\]"
    
    eventid = game %>% str_which("\\[Event")
    ecoid = game %>% str_which("\\[ECO")
    siteid = game %>% str_which("\\[Site")
    dateid = game %>% str_which("\\[Date")
    roundid = game %>% str_which("\\[Round")
    whiteid = game %>% str_which("\\[White ")
    blackid = game %>% str_which("\\[Black ")
    resultid = game %>% str_which("\\[Result")
    weloid = game %>% str_which("\\[WhiteElo")
    beloid = game %>% str_which("\\[BlackElo")
    
    event = sub(myregex, "\\1", game[eventid])
    if (identical(event, character(0))) {
      event = NA
    }
    
    site = sub(myregex, "\\1", game[siteid])
    if (identical(site, character(0))) {
      site = NA
    }
    
    date = sub(myregex, "\\1", game[dateid])
    if (identical(date, character(0))) {
      date = NA
    }
    
    round = sub(myregex, "\\1", game[roundid])
    if (identical(round, character(0))) {
      round = NA
    }
    
    white = sub(myregex, "\\1", game[whiteid])
    if (identical(white, character(0))) {
      white = NA
    }
    
    black = sub(myregex, "\\1", game[blackid])
    if (identical(black, character(0))) {
      black = NA
    }
    
    result = sub(myregex, "\\1", game[resultid])
    if (identical(result, character(0))) {
      result = NA
    }
    
    welo = sub(myregex, "\\1", game[weloid])
    if (identical(welo, character(0))) {
      welo = NA
    }
    
    belo = sub(myregex, "\\1", game[beloid])
    if (identical(belo, character(0))) {
      belo = NA
    }
    
    eco = sub(myregex, "\\1", game[ecoid])
    if (identical(eco, character(0))) {
      eco = NA
    }
    
    btid = game %>% str_which("\\[BlackTitle")
    wtid = game %>% str_which("\\[WhiteTitle")
    vaid = game %>% str_which("\\[Variation")
    wfid = game %>% str_which("\\[WhiteFideId")
    bfid = game %>% str_which("\\[BlackFideId")
    wTid = game %>% str_which("\\[WhiteTeam")
    bTid = game %>% str_which("\\[BlackTeam")
    opid = game %>% str_which("\\[Opening")
    
    bt = sub(myregex, "\\1", game[btid])
    if (identical(bt, character(0))) {
      bt = NA
    }
    
    wt = sub(myregex, "\\1", game[wtid])
    if (identical(wt, character(0))) {
      wt = NA
    }
    
    va = sub(myregex, "\\1", game[vaid])
    if (identical(va, character(0))) {
      va = NA
    }
    
    wf = sub(myregex, "\\1", game[wfid])
    if (identical(wf, character(0))) {
      wf = NA
    }
    
    bf = sub(myregex, "\\1", game[bfid])
    if (identical(bf, character(0))) {
      bf = NA
    }
    
    bT = sub(myregex, "\\1", game[bTid])
    if (identical(bT, character(0))) {
      bT = NA
    }
    
    wT = sub(myregex, "\\1", game[wTid])
    if (identical(wT, character(0))) {
      wT = NA
    }
    
    op = sub(myregex, "\\1", game[opid])
    if (identical(op, character(0))) {
      op = NA
    }
    
    movid = max(eventid,
                ecoid,
                siteid,
                dateid,
                roundid,
                whiteid,
                blackid,
                resultid,
                weloid,
                beloid,
                btid,
                wtid,
                bfid,
                wfid,
                wTid,
                bTid,
                vaid,
                opid,
                vaid) + 1
    
    moves <- game[movid:length(game)] %>% paste(collapse = '')
    
    new_game[[i]] <- data.frame(
      Event = event,
      Site = site,
      Date = date,
      Round = round,
      White = white,
      Black = black,
      Result = result,
      WElo = welo,
      BElo = belo,
      Eco = eco,
      Moves = moves,
      
      Opening = op,
      Variation = va,
      BlackTeam = bT,
      WhiteTeam = wT,
      BlackTitle = bt,
      WhiteTitle = wt,
      BlackFideId = bf,
      WhiteFideId = wf,
      
      stringsAsFactors = F
    )
  }
  return(bind_rows(new_game))
}



chunk_size <-  1000000
read_size <- 10000
write_files <- function(i){
  data_read <- scan("project_data/AepliBase.pgn", '', skip = i * chunk_size, nlines = read_size, sep = '\n')
  write_csv(pgn_cleaner(data_read),path = paste0("chunked_data/","chunk_",i, ".csv"))
}


invisible(mapply(write_files, (1:(no_lines/chunk_size))))

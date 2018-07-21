
pgn_cleaner <- function(data) {
  library(dplyr)
  library(stringr)
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
    
    movid = max(eventid,
                ecoid,
                siteid,
                dateid,
                roundid,
                whiteid,
                blackid,
                resultid,
                weloid,
                beloid) + 1
    
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
      stringsAsFactors = F
    )
  }
  return(bind_rows(new_game))
}

data_read <- scan("project_data/AepliBase.pgn", '', skip = 10000000, nlines = 10000, sep = '\n')
sample_clean <- pgn_cleaner(data_read)

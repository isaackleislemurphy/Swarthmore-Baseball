get_pbp_conf <- function(minSeason, maxSeason = 2019, conf = "Centennial"){
  
  
  load("master_ncaa_team_lu.rda")
  load("ncaa_season_id_lu.rda")
  
  suppressMessages(require(dplyr))
  suppressMessages(require(xml2))
  suppressMessages(require(tidyr))
  suppressMessages(require(rvest))
  suppressMessages(require(lubridate))
  
  source("ncaa_scrape.R")
  source("get_ncaa_schedule_info.R")
  source("get_ncaa_baseball_pbp.R")
  
  confIds = master_ncaa_team_lu%>%
    filter(conference == conf,
           year>=minSeason,
           year<=maxSeason)%>%
    dplyr::select(school_id, year)
  
  #get urls for those ids/seasons
  scheduleUrls <- lapply(1:nrow(confIds), 
                         function(x) get_ncaa_schedule_info(confIds$school_id[x], 
                                                            confIds$year[x]))%>%
    do.call("bind_rows", .)
  
  pbpData <- lapply(unique(scheduleUrls$game_info_url), 
                    function(x) get_ncaa_baseball_pbp(x)%>%
                      mutate(game_url = x))%>%
    do.call("bind_rows", .)
  
  return(pbpData)
  
}
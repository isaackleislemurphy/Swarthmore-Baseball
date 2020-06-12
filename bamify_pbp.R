bamify_pbp <- function(pbpData){
  
  centConf = c("Swarthmore", "Haverford", "McDaniel", "Gettysburg", "Franklin & Marshall",
               "Dickinson", "Muhlenberg", "Johns Hopkins", "Washington Col.")
  suppressMessages(require(dplyr))
  suppressMessages(require(tidyr))
  suppressMessages(require(lubridate))
  
  
  pbpDataWr = pbpData%>%
    separate(score, into = c("away_score_post", "home_score_post"), sep = "-", convert = T)%>%
    mutate(game_pk = str_replace_all(game_url, 
                                     "https://stats.ncaa.org/contests/|/box_score", 
                                     replacement = ""),
           bat_score_post = ifelse(inning_top_bot=="bot", home_score_post, away_score_post),
           pit_score_post = ifelse(inning_top_bot=="bot", away_score_post, home_score_post),
           result = ifelse(grepl("homered", description), "hr",
                           ifelse(grepl("tripled", description), "x3b",
                                  ifelse(grepl("doubled", description), "x2b",
                                         ifelse(grepl("singled", description), "x1b", 
                                                ifelse(grepl("walked", description), "bb",
                                                       ifelse(grepl("hit by pitch", description), "hbp", NA))))))
           #runs_on_play = str_count(string = description, pattern = "scored")
    )%>%
    group_by(game_pk)%>%
    mutate(rec_seq = row_number())%>%
    ungroup()%>%
    filter(!grepl(paste("to", c("c", "p", "1b", "2b", "3b", "ss",
                                "lf", "rf", "cf", "dh"), "for", collapse = "|"),
                  description),
           !(grepl(paste("to", c("c", "p", "1b", "2b", "3b", "ss", "lf", "rf", "cf", "dh"), collapse = "|"), description) & 
             str_count(description, "\\s")==3),
           !grepl("pinch hit|pinch ran", description),
           !grepl("/for ", description),
           !grepl("\\ for", description))%>%
    group_by(game_pk, inning_top_bot)%>%
    arrange(game_pk, inning_top_bot, rec_seq)%>%
    mutate(pa_seq = row_number())%>%
    ungroup()%>%
    group_by(game_pk, inning, inning_top_bot)%>%
    do(get_base_state(.))%>%
    ungroup()%>%
    group_by(game_pk, inning, inning_top_bot)%>%
    mutate(sloppy_scoring = ifelse(max(outs_post)>3, 1, 0))%>%
    ungroup()%>%
    filter(sloppy_scoring==0)%>%
    mutate(on_1b = on_1b_post, on_2b = on_2b_post, on_3b = on_3b_post)
  
  
  
  pbpDataFull = left_join(pbpDataWr, 
                          pbpDataWr%>%
                            dplyr::select(game_pk, inning_top_bot, pa_seq, 
                                          away_score_post, home_score_post, bat_score_post, pit_score_post)%>%
                            rename_at(c("away_score_post", "home_score_post", "bat_score_post", 
                                        "pit_score_post"), 
                                      function(x) str_replace_all(x, "_post", ""))%>%
                            mutate(pa_seq = pa_seq + 1),
                          by = c("game_pk", "inning_top_bot", "pa_seq"),
                          suffix = c("_xxx", ""))%>%
    left_join(., 
              pbpDataWr%>%
                dplyr::select(game_pk, inning, inning_top_bot, pa_seq, 
                              on_1b, on_2b, on_3b, outs_post, base_out_state_post)%>%
                rename_at(c("outs_post", "base_out_state_post"), 
                          function(x) str_replace_all(x, "_post", ""))%>%
                mutate(pa_seq = pa_seq + 1),
              by = c("game_pk", "inning", "inning_top_bot", "pa_seq"),
              suffix = c("_xxx", ""))%>%
    ungroup()%>%
    dplyr::select(c("game_pk", "inning", "inning_top_bot", "pa_seq", "rec_seq", "description"),
                  sort(setdiff(colnames(.), c("game_pk", "inning", "inning_top_bot", "pa_seq", "rec_seq", "description"))))%>%
    mutate(bat_score = ifelse(is.na(bat_score), 0, bat_score),
           on_1b = ifelse(is.na(on_1b), 0, on_1b),
           on_2b = ifelse(is.na(on_2b), 0, on_2b),
           on_3b = ifelse(is.na(on_3b), 0, on_3b),
           outs = ifelse(is.na(outs), 0, outs))%>%
    group_by(game_pk, inning, inning_top_bot)%>%
    mutate(base_out_state = ifelse(is.na(base_out_state) & pa_seq == min(pa_seq), "000:0", base_out_state),
           runs_to_end = max(bat_score_post) - bat_score_post,
           runs_on_play = bat_score_post - bat_score)%>%
    ungroup()%>%
    mutate(venue_conf = ifelse(grepl("Chestertown", location), "WAC",
                               ifelse(grepl("Baltimore", location), "JHU",
                               ifelse(grepl("Swarthmore", location), "SWA",
                               ifelse(grepl("Lancaster", location), "FM",
                               ifelse(grepl("Carlisle", location), "DIC",
                               ifelse(grepl("Westminster", location), "MCD",
                               ifelse(grepl("Haverford", location), "HAV",
                               ifelse(grepl("Gettysburg", location), "GET",
                               ifelse(grepl("Allentown", location), "MUH",
                               ifelse(grepl(", Fl", location), "FLA", "ALT")))))))))),
           fielding_conf = ifelse(fielding %in% centConf, fielding, "OTH"),
           batting_conf = ifelse(batting %in% centConf, batting, "OTH"))%>%
    dplyr::select(game_pk, batting, fielding, batting_conf, fielding_conf,
                  location, inning, inning_top_bot,rec_seq, pa_seq, result,
                  description, bat_score, bat_score_post, runs_on_play, runs_to_end, everything())
  
  return(pbpDataFull)
  
  
  
}
#   pbp_data = pbpDataWr[1:6, ]
#       pbp_data = pbpDataWr%>%filter(game_pk == 1648998, inning == 3, inning_top_bot == "top")
# pbp_data = pbpDataWr%>%filter(game_pk == 1653333, inning == 3, inning_top_bot == "bot") #gidp 2
get_base_state <- function(pbp_data){
  
 
  
  suppressMessages(require(stringr))
  #cat(pbp_data$game_pk[1], " | ", pbp_data$inning[1], " | ", pbp_data$inning_top_bot[1], "\n")
  
  pbp_data = arrange(pbp_data, pa_seq)
  pbp_data$on_1b = 0; pbp_data$on_2b = 0; pbp_data$on_3b = 0
  pbp_data$outs_add = 0; pbp_data$outs_post = 0;
  
  pbp_data$on_1b[1] = ifelse(grepl("singled|walked|reached|hit by pitch", pbp_data$description[1]), 1, 0)
  pbp_data$on_2b[1] = ifelse(grepl("doubled|advanced to second|stole second", pbp_data$description[1]), 1, 0)
  pbp_data$on_3b[1] = ifelse(grepl("tripled|advanced to third|stole third", pbp_data$description[1]), 1, 0)
  
  pbp_data$on_1b[1] = ifelse(grepl("out at first", pbp_data$description[1]), 0, pbp_data$on_1b[1])
  pbp_data$on_2b[1] = ifelse(grepl("out at second", pbp_data$description[1]), 0, pbp_data$on_2b[1])
  pbp_data$on_3b[1] = ifelse(grepl("out at third", pbp_data$description[1]), 0, pbp_data$on_3b[1])
  
  pbp_data$outs_add[1] = ifelse(grepl("out|popped up", pbp_data$description[1]) & 
                                  !grepl("struck out, reached", pbp_data$description[1]) & 
                                  !grepl("struck out swinging, reached", pbp_data$description[1]),
                                1, 0);
  pbp_data$outs_post[1] = pbp_data$outs_add[1]
  
  if (nrow(pbp_data)==1){
    return(pbp_data)
  }
  
  for (ii in 2:nrow(pbp_data)){
    #for (ii in 2:9){
    
    prev_state = pbp_data[ii-1, c("on_1b", "on_2b", "on_3b")]%>%as.numeric()
    pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = prev_state
    
    #add outs
    if (grepl("triple play", pbp_data$description[ii])){
      pbp_data$outs_add[ii] = 3
    }else if (grepl("double play", pbp_data$description[ii])){
      pbp_data$outs_add[ii] = 2
    }else if(grepl("out|popped up", pbp_data$description[ii]) & 
             !grepl("struck out, reached", pbp_data$description[ii]) & 
             !grepl("struck out swinging, reached", pbp_data$description[ii])){
      pbp_data$outs_add[ii] = 1
    }
    pbp_data$outs_post[ii] = sum(pbp_data$outs_add[1:ii])
    
    #clear lead runners
    if (str_count(pbp_data$description[ii], "scored")==3){
      
      pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = 0
      
    }else if(str_count(pbp_data$description[ii], "scored")==2){
      
      if(sum(prev_state[2:3] == c(1,1))==2){ #011 or #111
        pbp_data[ii, c("on_2b", "on_3b")] = 0
      }
      
      if(sum(prev_state == c(1,0,1))==3){
        pbp_data[ii, c("on_1b", "on_3b")] = 0
      }
      
      if(sum(prev_state == c(1,1,0))==3){
        pbp_data[ii, c("on_1b", "on_2b")] = 0
      }
      
    }else if(str_count(pbp_data$description[ii], "scored")==1){
      
      if(prev_state[3] == 1){ #runner on third scores
        pbp_data[ii, "on_3b"] = 0
      }else if (prev_state[2] == 1){
        pbp_data[ii, "on_2b"] = 0
      }else if (prev_state[1] == 1){
        pbp_data[ii, "on_1b"] = 0
      }
      
      
    }else{ #nobody scored
      if (sum(prev_state == c(1, 0, 0))==3 & grepl("advanced to third", pbp_data$description[ii])){
        pbp_data[ii, c("on_1b", "on_2b")] = 0
      }
      if (sum(prev_state == c(0, 1, 0))==3 & grepl("advanced to third", pbp_data$description[ii])){
        pbp_data[ii, c("on_1b", "on_2b")] = 0
      }
      if (sum(prev_state == c(1, 1, 0))==3 & grepl("advanced to third", pbp_data$description[ii])){
        pbp_data[ii, c("on_1b", "on_2b")] = 0
      }
    }
    
    
    #handle double plays
    if (grepl("double play", pbp_data$description[ii]) & sum(prev_state) == 1){
      pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = 0; #if there are advancements, these will be picked up later
    }else{
      if (grepl("grounded into double play", pbp_data$description[ii])){
        
        if (sum(prev_state == c(1, 1, 0))==3){
          pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = 0; #if there are advancements, these will be picked up later
        }
        
        if (sum(prev_state == c(1, 0, 1))==3){
          #do nothing. Infield in advancement picked up
        } 
        
        #if (0, 1, 1)
        
      }
      if (grepl("lined into double play", pbp_data$description[ii])){
        
        if (grepl("to 1b|1b unassisted|to first base", pbp_data$description[ii])){
          pbp_data[ii, c("on_1b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 2b|to ss|2b unassisted|ss unassisted", pbp_data$description[ii])){
          pbp_data[ii, c("on_2b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 3b|3b unassisted", pbp_data$description[ii])){
          pbp_data[ii, c("on_3b")] = 0; #if there are advancements, these will be picked up later
        }
      }
      if (grepl("flied into double play", pbp_data$description[ii])){
        
        if (grepl("to 1b|1b unassisted|to first base", pbp_data$description[ii])){
          pbp_data[ii, c("on_1b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 2b|to ss|2b unassisted|ss unassisted", pbp_data$description[ii])){
          pbp_data[ii, c("on_2b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 3b|3b unassisted|to c", pbp_data$description[ii])){
          pbp_data[ii, c("on_3b")] = 0; #if there are advancements, these will be picked up later
        }
      }
      if (grepl("hit into double play", pbp_data$description[ii])){
        
        if (grepl("to 1b|1b unassisted|to first base", pbp_data$description[ii])){
          pbp_data[ii, c("on_1b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 2b|to ss|2b unassisted|ss unassisted", pbp_data$description[ii])){
          pbp_data[ii, c("on_2b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("to 3b|3b unassisted|to c", pbp_data$description[ii])){
          pbp_data[ii, c("on_3b")] = 0; #if there are advancements, these will be picked up later
        }else if(grepl("interference", pbp_data$description[ii])){
          if (prev_state[3]==1){
            pbp_data[ii, c("on_3b")] = 0;
          }else if (prev_state[2]==1){
            pbp_data[ii, c("on_2b")] = 0;
          }
        }
        #INTERFERENCE
      }
    }
    
    
    
    #new reaches
    pbp_data$on_3b[ii] = ifelse(grepl("tripled|advanced to third|stole third", pbp_data$description[ii]),
                                1, pbp_data$on_3b[ii])
    
    pbp_data$on_2b[ii] = ifelse(grepl("doubled|advanced to second|stole second", pbp_data$description[ii]),
                                1, pbp_data$on_2b[ii])
    
    pbp_data$on_1b[ii] = ifelse(grepl("singled|walked|reached|hit by pitch", pbp_data$description[ii]), 
                                1, pbp_data$on_1b[ii])
    
    pbp_data$on_3b[ii] = ifelse(grepl("out at third", pbp_data$description[ii]), 0, pbp_data$on_3b[ii])
    pbp_data$on_2b[ii] = ifelse(grepl("out at second", pbp_data$description[ii]), 0, pbp_data$on_2b[ii])
    pbp_data$on_1b[ii] = ifelse(grepl("out at first", pbp_data$description[ii]), 0, pbp_data$on_1b[ii])
    
    
    #stolen bases/advancement
    pbp_data$on_3b[ii] = ifelse(grepl("stole home|scored on a balk|scored on a passed ball|scored on a wild pitch", pbp_data$description[ii])&
                                  !grepl("stole third|advanced to third on a wild pitch|advanced to third on a passed ball|advanced to third on a balk", pbp_data$description[ii]),
                                0, pbp_data$on_3b[ii])
    
    pbp_data$on_2b[ii] = ifelse(grepl("stole third|advanced to third on a wild pitch|advanced to third on a passed ball|advanced to third on a balk", pbp_data$description[ii]) &
                                  !grepl("stole second|advanced to second on a wild pitch|advanced to second on a passed ball|advanced to second on a balk", pbp_data$description[ii]),
                                0, pbp_data$on_2b[ii])
    
    pbp_data$on_1b[ii] = ifelse(grepl("stole second|advanced to second on a wild pitch|advanced to second on a passed ball|advanced to second on a balk", pbp_data$description[ii]),
                                0, pbp_data$on_1b[ii])
    
    #caught stealing/pickoffs
    pbp_data$on_3b[ii] = ifelse(grepl("out at home", pbp_data$description[ii])&grepl("caught stealing", pbp_data$description[ii])&
                                  !grepl("stole third|advanced to third", pbp_data$description[ii]),
                                0, pbp_data$on_3b[ii])
    
    pbp_data$on_2b[ii] = ifelse(grepl("out at third", pbp_data$description[ii])&grepl("caught stealing", pbp_data$description[ii])&
                                  !grepl("stole second|advanced to second", pbp_data$description[ii]),
                                0, pbp_data$on_2b[ii])
    
    pbp_data$on_1b[ii] = ifelse(grepl("out at second", pbp_data$description[ii])&grepl("caught stealing", pbp_data$description[ii]),
                                0, pbp_data$on_1b[ii])
    
    
    
    
    
    #pbp_data$on_1b[ii] = ifelse(grepl("scored|advanced to third|advanced to second", pbp_data$description[ii]) &
    #                             pbp_data$on_1b[ii]!=1 & pbp_data$on_1b[ii-1]==1,
    #                            0, pbp_data$on_1b[ii])
    
    
    
    #clear bases for various types of hits
    if(grepl("homered", pbp_data$description[ii])){
      pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = c(0,0,0)
    }
    if(grepl("tripled", pbp_data$description[ii])){
      pbp_data[ii, c("on_1b", "on_2b")] = c(0,0)
    }
    if(grepl("doubled", pbp_data$description[ii])){
      pbp_data[ii, c("on_1b")] = 0
    }
    
    
    dirs = c("", " to left field", " to left center", " to center field", " to right center", " to right field",
             " to pitcher", " to third base", " to shortstop", " to second base", " to first base", " to catcher",
             " down the lf line", " down the rf line", " through the left side", " through the right side")
    
    #advanced on error, cleanup trailing bases
    if(grepl(paste0("tripled", dirs, ", scored", collapse = "|"), pbp_data$description[ii])){
      pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = 0
    }
    if(grepl(paste0("doubled", dirs, ", advanced to third", collapse = "|"), pbp_data$description[ii])){
      pbp_data[ii, c("on_1b", "on_2b")] = 0
    }
    if(grepl(paste0("singled", dirs, ", advanced to second", collapse = "|"), pbp_data$description[ii])){
      pbp_data[ii, c("on_1b")] = 0
    }
    
    #little league home run -- clear bases
    if(str_count(pbp_data$description[ii], "scored") == sum(prev_state)+1){
      pbp_data[ii, c("on_1b", "on_2b", "on_3b")] = 0
    }
    
    
  }
  
  pbp_data%>%
    ungroup()%>%
    mutate(base_out_state_post = paste0(on_1b, 2*on_2b, 3*on_3b, ":", outs_post),
           base_out_state_post = ifelse(grepl(":3", base_out_state_post), "000:3", base_out_state_post))%>%
    rename(on_1b_post = on_1b, on_2b_post = on_2b, on_3b_post = on_3b)%>%
    return()
  
  
}
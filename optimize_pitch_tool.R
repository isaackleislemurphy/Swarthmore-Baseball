suppressMessages(library(dplyr))
suppressMessages(library(baseballr))
suppressMessages(require(caret))


setwd("~/Documents/Rapsodo2020")
bam_id_lu <- read.csv("bam_id_lu.csv")

seas_19_dates <- seq(as.Date("2019/3/28"), as.Date("2019/9/31"), "days")%>%as.character()
seas_19_dates <- seq(as.Date("2019/3/28"), as.Date("2019/8/25"), "days")%>%
  as.character()

sc_2019 <- lapply(seas_19_dates, function(x) 
  try(scrape_statcast_savant(start_date = x, end_date = x, player_type = "pitcher"))
)%>%
  do.call("rbind", .)


wrangle_first <- function(sc_data, bam_id_lu, cond_swing = 1, velo_cap = 93.5){
  
  suppressMessages(require(dplyr))
  suppressMessages(require(tidyr))
  
  sc_data%>%
    inner_join(., bam_id_lu%>%dplyr::select(mlb_id, mlb_name, mlb_pos)%>%filter(mlb_pos=="P"),
               by = c("batter" = "mlb_id"))%>%
    filter(pitch_type != "", pitch_type != "EP", pitch_type != "KN", release_speed>60)%>%
    mutate(pitch_cat = ifelse(pitch_type %in% c("SI", "FF", "FS", "FT", "FC"), "FA",
                              ifelse(pitch_type == "SL", "SL",
                                     ifelse(pitch_type %in% c("KC", "CU"), "CU",
                                            ifelse(pitch_type =="CH", "CH", "OTH")))),
           is_gb_cat = ifelse(bb_type=="ground_ball", "Y", "N"),
           is_gb_cat = ifelse(is.na(is_gb_cat), "N" , is_gb_cat),
           is_gb = ifelse(bb_type=="ground_ball", 1, 0),
           is_gb = ifelse(is.na(is_gb), 0 , is_gb),
           is_sm = ifelse(description == "swinging_strike", 1, 0),
           is_sm_cat = ifelse(description == "swinging_strike", "Y", "N"),
           is_swing = ifelse(description %in% c("called_strike")|grepl("ball", description),
                             0, 1),
           zone_dir = ifelse(zone %in% c(2, 4:6, 8), "H",
                             ifelse(zone %in% c(1, 11), "UL",
                                    ifelse(zone %in% c(3,12), "UR",
                                           ifelse(zone %in% c(9, 14), "DR",
                                                  ifelse(zone %in% c(7, 13), "DL", "UNK"))))))%>%
    dplyr::select(game_date, player_name, batter, stand, p_throws, is_swing, is_sm, is_sm_cat, is_gb, is_gb_cat,
                  pitch_cat, pitch_type, zone, zone_dir, release_speed, 
                  release_spin_rate, pfx_x, pfx_z, description, bb_type)%>%
    filter(is_swing>=cond_swing, 
           release_speed < velo_cap)%>%
    mutate_at(c(#"is_gb", "is_sm", 
      "pitch_cat"), as.factor)%>%
    filter(!is.na(is_gb), !is.na(is_sm), !is.na(release_speed), 
           !is.na(release_spin_rate), !is.na(pfx_x), !is.na(pfx_z),
           !is.na(zone))%>%
    return()
  
}


split_train_test <- function(df, train_pct = .67, seed = 2020){
  
  set.seed(seed)
  slice_vec = sample(1:nrow(df), floor(train_pct*nrow(df)), replace = F)
  train = df[slice_vec,]; test = df[-slice_vec, ]
  
  return(list("train" = train, "test" = test))
}


fit_caret_split <- function(formula, tr_te_list, val_targ, ...){
  
  
  mod_rr <- caret::train(formula,
                         data = tr_te_list$train%>%filter(p_throws == "R", stand == "R"),
                         ...)
  cat("...RR fit...\n")
  
  mod_rl <- caret::train(formula,
                         data = tr_te_list$train%>%filter(p_throws == "R", stand == "L"),
                         ...)
  cat("...RL fit...\n")
  
  mod_ll <- caret::train(formula,
                         data = tr_te_list$train%>%filter(p_throws == "L", stand == "L"),
                         ...)
  
  cat("...LL fit...\n")
  
  mod_lr <- caret::train(formula,
                         data = tr_te_list$train%>%filter(p_throws == "L", stand == "R"),
                         ...)
  
  cat("...LR fit...\n")
  
  brier_rr <- mean((tr_te_list$test%>%filter(p_throws == "R", stand == "R")%>%pull(val_targ) - 
    predict(mod_rr, newdata = tr_te_list$test%>%filter(p_throws == "R", stand == "R"), type = "prob")%>%pull(Y))^2,
    na.rm = T)
  
  brier_rl <- mean((tr_te_list$test%>%filter(p_throws == "R", stand == "L")%>%pull(val_targ) - 
    predict(mod_rl, newdata = tr_te_list$test%>%filter(p_throws == "R", stand == "L"), type = "prob")%>%pull(Y))^2)
  
  brier_ll <- mean((tr_te_list$test%>%filter(p_throws == "L", stand == "L")%>%pull(val_targ) - 
    predict(mod_ll, newdata = tr_te_list$test%>%filter(p_throws == "L", stand == "L"), type = "prob")%>%pull(Y))^2)
  
  brier_lr <- mean((tr_te_list$test%>%filter(p_throws == "L", stand == "R")%>%pull(val_targ) - 
    predict(mod_lr, newdata = tr_te_list$test%>%filter(p_throws == "L", stand == "R"), type = "prob")%>%pull(Y))^2)
  
  return(c(brier_rr, brier_rl, brier_ll, brier_lr))

  
}





swings_wr = wrangle_first(sc_2019, bam_id_lu, cond_swing = 1)[sample(1:nrow(swings_wr), nrow(swings_wr)),]
tr_te_list = split_train_test(swings_wr, .666)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random", classProbs = T)

svmR_sm <- fit_caret_split(formula = is_sm_cat ~ release_speed + release_spin_rate + pfx_x + pfx_z +zone_dir,
                           tr_te_list = tr_te_list,
                           val_targ = "is_sm",
                           method = "svmRadial",
                           preProcess = c("center", "scale"),
                           tuneLength = 15,
                           trControl = trctrl,
                           metric = "ROC")

svmR_sm <- fit_caret_split(formula = is_sm_cat ~ release_speed + release_spin_rate + pfx_x + pfx_z +zone_dir,
                           tr_te_list = tr_te_list,
                           val_targ = "is_sm",
                           method = "rf",
                           preProcess = c("center", "scale"),
                           tuneLength = 10,
                           trControl = trctrl,
                           metric = "ROC")





trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, search = "random", classProbs = T, summaryFunction=twoClassSummary)

lasso_k <- fit_caret_split(
  formula = is_sm_cat ~ release_speed + release_spin_rate + pfx_x + pfx_z + zone_dir + pfx_x*zone_dir + pfx_z*zone_dir +
      pfx_x * release_speed + pfx_z * release_speed + zone_dir*release_speed,
  tr_te_list = tr_te_list,
  val_targ = "is_sm",
  method = "glmnet",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(alpha = 1,lambda = seq(0,1,by = 0.1)),
  trControl = trctrl,
  metric = "ROC")




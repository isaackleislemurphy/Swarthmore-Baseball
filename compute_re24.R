suppressMessages(require(dplyr))
suppressMessages(require(xml2))
suppressMessages(require(tidyr))
suppressMessages(require(rvest))
suppressMessages(require(lubridate))
suppressMessages(require(stringr))
suppressMessages(require(lme4))

load("master_ncaa_team_lu.rda")
load("ncaa_season_id_lu.rda")

source("ncaa_scrape.R")
source("get_ncaa_schedule_info.R")
source("get_ncaa_baseball_pbp.R")
source("bamify_pbp.R")
source("get_pbp_conf.R")
source("get_base_state.R")


unique_venues = c("ALT", "WAC", "JHU", "GET", "HAV", "FLA", "SWA", "MCD", "DIC", "MUH", "FM")

#pbpData = get_pbp_conf(2019, conf = "Centennial")
load("pbpData19.rds")
pbpData_bam = bamify_pbp(pbpData)
pbpData_bam$venue_conf = as.factor(pbpData_bam$venue_conf)
pbpData_bam$fielding_conf = as.factor(pbpData_bam$fielding_conf)
pbpData_bam$batting_conf = as.factor(pbpData_bam$batting_conf)

re24_empirical = pbpData_bam%>%
  filter(base_out_state != "000:3")%>%
  group_by(base_out_state)%>%
  summarise(re = mean(runs_to_end),
            n = n())

re24_pois_model = glmer(runs_to_end ~ base_out_state + (1|venue_conf) + (1|fielding_conf) + (1|batting_conf), 
                        data = pbpData_bam%>%filter(base_out_state != "000:3"), family = "poisson")

# re24_park_adj = left_join(data.frame(base_out_state = re24_empirical$base_out_state,
#                                         idx = 1),
#                              data.frame(venue_conf = unique_venues,
#                                         idx = 1),
#                              by = c("idx"))%>%
#   dplyr::select(-idx)
# 
# re24_park_adj$re = predict(re24_pois_model, newdata = re24_pois_components, type = "response")%>%
#   as.vector()%>%
#   suppressMessages()
  
re24_empirical$pois_fixed_effect = getME(re24_pois_model, "fixef")%>%exp()

pois_matrix = outer(re24_empirical$pois_fixed_effect, re24_empirical$pois_fixed_effect, '-')
rownames(pois_matrix) = re24_empirical$base_out_state;
colnames(pois_matrix) = re24_empirical$base_out_state; 

emp_matrix = outer(re24_empirical$re, re24_empirical$re, '-')
rownames(emp_matrix) = re24_empirical$base_out_state;
colnames(emp_matrix) = re24_empirical$base_out_state; 



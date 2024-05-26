library(worldfootballR)
library(dplyr)

## Data Scraping ---------------------------------------------------------------
prem_url <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")

team_passing <- fb_team_match_log_stats(
  team_urls = prem_url, 
  stat_type = "passing") %>% 
  filter(ForAgainst == 'For') %>% 
  filter(Comp == 'Premier League')

team_shooting <- fb_team_match_log_stats(
  team_urls = prem_url, 
  stat_type = "shooting") %>% 
  filter(ForAgainst == 'For') %>% 
  filter(Comp == 'Premier League')

opponent_shooting <- fb_team_match_log_stats(
  team_urls = prem_url, 
  stat_type = "shooting") %>% 
  filter(ForAgainst == 'Against') %>% 
  filter(Comp == 'Premier League')

team_poss <- fb_team_match_log_stats(
  team_urls = prem_url, 
  stat_type = "possession") %>% 
  filter(ForAgainst == 'For') %>% 
  filter(Comp == 'Premier League')
  
team_defense <- fb_team_match_log_stats(
  team_urls = prem_url, 
  stat_type = "defense") %>% 
  filter(ForAgainst == 'For') %>% 
  filter(Comp == 'Premier League')


full_raw_data <- cbind(team_passing, team_shooting, team_poss, team_defense)
full_raw_data$xG_Expected <- full_raw_data$xG_Expected + 0.01
full_raw_data$xG_Allowed <- opponent_shooting$xG_Expected + 0.01
full_raw_data$Team <- gsub(' and ', ' & ', full_raw_data$Team)

## Fit box score model ---------------------------------------------------------
offense_model <- glm(
  xG_Expected ~ Venue + Cmp_Total + Att_Total + TotDist_Total + PrgDist_Total +
    Cmp_Long + Att_Long + Final_Third + PPA + CrsPA + PrgP + Sh_Standard + 
    SoT_Standard + `Att Pen_Touches` + PrgDist_Carries, 
  data = full_raw_data,
  family = Gamma(link = 'log')
)

summary(offense_model)

defense_model <- glm(
  xG_Allowed ~ Venue + Tkl_Tackles + TklW_Tackles + `Def 3rd_Tackles` + 
    Sh_Blocks + Pass_Blocks + `Def Pen_Touches` + Int + Clr + Err,
  data = full_raw_data,
  family = Gamma(link = 'log')
)

summary(defense_model)
## All player stats-------------------------------------------------------------

all_players_passing <- fb_team_player_stats(prem_url, stat_type = 'passing')
all_players_shooting <- fb_team_player_stats(prem_url, stat_type = 'shooting')
all_players_poss <- fb_team_player_stats(prem_url, stat_type = 'possession')
all_players_defense <- fb_team_player_stats(prem_url, stat_type = 'defense')

all_players_data <- cbind(all_players_passing, all_players_shooting, 
                          all_players_poss, all_players_defense)
all_players_data$Venue <- 'Away'
## Adjust for offensive/defensive role
all_players_data$role <- ifelse(
  all_players_data$Pos == 'GK', 0,
  ifelse(all_players_data$Pos == 'DF', 1,
  ifelse(all_players_data$Pos == 'DF,MF', 4/3,
  ifelse(all_players_data$Pos == 'MF,DF', 5/3,
  ifelse(all_players_data$Pos == 'MF', 2,
  ifelse(all_players_data$Pos == 'MF,FW', 7/3,
  ifelse(all_players_data$Pos == 'FW,MF', 8/3,
  ifelse(all_players_data$Pos == 'FW', 3, 0
))))))))


## OBPM
avg_off <- weighted.mean(predict(offense_model, newdata = all_players_data), 
                         all_players_data$Mins_Per_90)
obpm <- (predict(offense_model, newdata = all_players_data) - avg_off)/ 
  all_players_data$Mins_Per_90

## DBPM
avg_def <- weighted.mean(predict(defense_model, newdata = all_players_data), 
                         all_players_data$Mins_Per_90)
dbpm <- (predict(defense_model, newdata = all_players_data) - avg_def)/ 
  all_players_data$Mins_Per_90

bpm_df <- data.frame(
  player = all_players_data$Player, team = all_players_data$Squad,
  pos = all_players_data$Pos, min = all_players_data$Mins_Per_90, 
  obpm = obpm, dbpm = -dbpm
)
bpm_df$bpm <- bpm_df$obpm + bpm_df$dbpm
#bpm_df <- bpm_df[bpm_df$min > 10, ]

bpm_df$role <- ifelse(
  bpm_df$pos == 'GK', 0,
  ifelse(bpm_df$pos == 'DF', 1,
  ifelse(bpm_df$pos == 'DF,MF', 4/3,
  ifelse((bpm_df$pos == 'MF,DF') | (bpm_df$pos == 'DF,FW'), 5/3,
  ifelse(bpm_df$pos == 'MF', 2,
  ifelse((bpm_df$pos == 'MF,FW') | (bpm_df$pos == 'FW,DF'), 7/3,
  ifelse(bpm_df$pos == 'FW,MF', 8/3,
  ifelse(bpm_df$pos == 'FW', 3, 0
))))))))

## Adjust for offensive/defensive role -----------------------------------------
rolemodel_o <- lm()
rolemodel_d <- lm()


bpm_df$dbpm <- bpm_df$dbpm - (-0.100609 + 0.064723 * bpm_df$role)
bpm_df$obpm <- bpm_df$obpm - (-0.108766 + 0.064318 * bpm_df$role)
bpm_df$bpm <- bpm_df$obpm + bpm_df$dbpm


## Adjust for team quality -----------------------------------------------------
league_xg <- 38 * mean(full_raw_data$xG_Expected)
for (i in unique(bpm_df$team)){
  team_off_rtg <- log(sum(full_raw_data$xG_Expected[full_raw_data$Team == i]) / league_xg)
  team_def_rtg <- log(sum(full_raw_data$xG_Allowed[full_raw_data$Team == i]) / league_xg)
  obs_off <- sum(bpm_df$obpm[bpm_df$team == i] * 
         bpm_df$min[bpm_df$team == i], na.rm = TRUE) / 38; print(obs_off)
  obs_def <- sum(bpm_df$dbpm[bpm_df$team == i] * 
         bpm_df$min[bpm_df$team == i], na.rm = TRUE) / 38; print(obs_def)
  players <- which(bpm_df$team == i)
  bpm_df$obpm[players] <- bpm_df$obpm[players] + (team_off_rtg - obs_off) / 38
  bpm_df$dbpm[players] <- bpm_df$dbpm[players] + (team_def_rtg - obs_def) / 38
}
bpm_df$bpm <- bpm_df$obpm - bpm_df$dbpm


reduced_df <- bpm_df[bpm_df$min > 10, ]

reduced_df$obpm <- round(reduced_df$obpm, 3)
reduced_df$dbpm <- round(-reduced_df$dbpm, 3)
reduced_df$bpm <- round(reduced_df$bpm, 3)
write.csv(reduced_df[order(-reduced_df$bpm), ], 'premier_league_bpm.csv', row.names = FALSE)






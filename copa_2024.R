## copa 2024 -------------------------------------------------------------------
source('base.R')
set.seed(2024)

## DECLARATION OF TEAMS --------------------------------------------------------
copa_2024 <- c('Argentina', 'Peru', 'Chile', 'Canada',
               'Mexico', 'Ecuador', 'Venezuela', 'Jamaica',
               'United States', 'Uruguay', 'Panama', 'Bolivia',
               'Brazil', 'Colombia', 'Paraguay', 'Costa Rica')

model <- bayesglm(team_score ~ team + opponent + type,# + team_conf + opponent_conf,
                  family = 'poisson', data = newfifa, prior.mean = 0, 
                  weights = weight, prior.scale = 1, prior.df = Inf)

teams <- which(grepl('team', names(model$coefficients)))
rank_df <- data.frame(
  team = gsub('team', '', names(model$coefficients[teams])),
  offense = model$coefficients[teams],
  defense = model$coefficients[teams + length(teams)]
)
rank_df$overall <- rank_df$offense - rank_df$defense

nsim <- 10000
x <- coef(sim(model, nsim))
a <- sim_group(x, copa_2024[1:4], 2, nsim, bayesian = TRUE)
b <- sim_group(x, copa_2024[5:8], 2, nsim, bayesian = TRUE)
c <- sim_group(x, copa_2024[9:12], 2, nsim, bayesian = TRUE)
d <- sim_group(x, copa_2024[13:16], 2, nsim, bayesian = TRUE)

knockout_qualifiers <- cbind(a[, 1], b[, 2], a[, 2], b[, 1],
                             c[, 1], d[, 2], c[, 2], d[, 1]
)
knockout_results <- knockout_bayes(knockout_qualifiers, x, rounds = 3)
champs <- data.frame(Team = copa_2024, Knockout = 0, #Quarterfinal = 0,
                     Semifinal = 0, Final = 0, Champion = 0)
for(i in 1:16){
  champs$Knockout[i] <- length(which(knockout_results[, 1:8] == copa_2024[i]))
#  champs$Quarterfinal[i] <- length(which(knockout_results[, 17:24] == copa_2024[i]))
  champs$Semifinal[i] <- length(which(knockout_results[, 9:12] == copa_2024[i]))
  champs$Final[i] <- length(which(knockout_results[, 13:14] == copa_2024[i]))
  champs$Champion[i] <- length(which(knockout_results[, 15] == copa_2024[i]))
}
champs <- champs[order(champs$Champion, decreasing = TRUE), ]
champs[, 2:ncol(champs)] <- champs[, 2:ncol(champs)] / nsim
rownames(champs) <- NULL; champs


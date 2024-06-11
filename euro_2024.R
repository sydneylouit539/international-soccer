## EURO 2024 -------------------------------------------------------------------
source('base.R')
set.seed(2024)

## DECLARATION OF TEAMS --------------------------------------------------------
euro_2024 <- c('Germany', 'Scotland', 'Hungary', 'Switzerland',
               'Spain', 'Croatia', 'Italy', 'Albania',
               'Slovenia', 'Denmark', 'Serbia', 'England',
               'Poland', 'Netherlands', 'Austria', 'France',
               'Belgium', 'Slovakia', 'Romania', 'Ukraine',
               'Turkey', 'Georgia', 'Portugal', 'Czech Republic')

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
a <- sim_group(x, euro_2024[1:4], 3, nsim, bayesian = TRUE)
b <- sim_group(x, euro_2024[5:8], 3, nsim, bayesian = TRUE)
c <- sim_group(x, euro_2024[9:12], 3, nsim, bayesian = TRUE)
d <- sim_group(x, euro_2024[13:16], 3, nsim, bayesian = TRUE)
e <- sim_group(x, euro_2024[17:20], 3, nsim, bayesian = TRUE)
f <- sim_group(x, euro_2024[21:24], 3, nsim, bayesian = TRUE)

thirds <- cbind(a[, 3], b[, 3], c[, 3], d[, 3], e[, 3], f[, 3])
thirds_to_advance <- t(apply(thirds, 1, sample, size = 4))
knockout_qualifiers <- cbind(b[, 1], thirds_to_advance[, 3], a[, 1], c[, 2],
                             f[, 1], thirds_to_advance[, 1], d[, 2], e[, 2],
                             e[, 1], thirds_to_advance[, 2], d[, 1], f[, 2],
                             c[, 1], thirds_to_advance[, 4], a[, 2], b[, 2])
knockout_results <- knockout_bayes(knockout_qualifiers, x, rounds = 4)
champs <- data.frame(Team = euro_2024, Knockout = 0, Quarterfinal = 0,
                     Semifinal = 0, Final = 0, Champion = 0)
for(i in 1:24){
  champs$Knockout[i] <- length(which(knockout_results[, 1:16] == euro_2024[i]))
  champs$Quarterfinal[i] <- length(which(knockout_results[, 17:24] == euro_2024[i]))
  champs$Semifinal[i] <- length(which(knockout_results[, 25:28] == euro_2024[i]))
  champs$Final[i] <- length(which(knockout_results[, 29:30] == euro_2024[i]))
  champs$Champion[i] <- length(which(knockout_results[, 31] == euro_2024[i]))
}
champs <- champs[order(champs$Champion, decreasing = TRUE), ]
rownames(champs) <- NULL; champs
















## CODE FOR MAJOR SOCCER TOURNAMENTS -------------------------------------------
library(arm)

fifa <- read.csv('https://raw.githubusercontent.com/martj42/international_results/master/results.csv')
fifa <- fifa[fifa$date > '2022-08-01', ]

newfifa <- rbind(fifa, fifa)
p <- nrow(fifa)
newfifa[p + (1:p), c(2, 4)] <- newfifa[1:p, c(3, 5)]
newfifa[p + (1:p), c(3, 5)] <- newfifa[1:p, c(2, 4)]
names(newfifa)[2:5] = c('team', 'opponent', 'team_score', 'opponent_score')
newfifa$type <- 0
newfifa$type[1:p] <- ifelse(fifa$neutral == F, 1, 0.5)
newfifa$type[p + (1:p)] <- ifelse(fifa$neutral == F, 0, 0.5)
newfifa <- newfifa[, -(7:9)]

#exclude = c()
#for(i in unique(newfifa$team)){
#  g = sum(newfifa$team_score[newfifa$team==i],na.rm = T); if(g==0){exclude = c(exclude,i)}
#}
#newfifa = newfifa[-which(newfifa$team%in%exclude | newfifa$opponent%in%exclude),]
write.csv(newfifa, 'newfifa.csv', row.names = FALSE)

## DECLARATION OF TEAMS --------------------------------------------------------
worldcupteams <- c('Qatar', 'Ecuador', 'Senegal', 'Netherlands',
                   'England', 'Iran', 'United States', 'Wales',
                   'Argentina', 'Saudi Arabia', 'Mexico', 'Poland',
                   'France', 'Australia', 'Denmark', 'Tunisia',
                   'Spain', 'Costa Rica', 'Germany', 'Japan',
                   'Belgium', 'Canada', 'Morocco', 'Croatia',
                   'Brazil', 'Serbia', 'Switzerland', 'Cameroon',
                   'Portugal', 'Ghana', 'Uruguay', 'South Korea')

## FUNCTIONS AND HELPERS -------------------------------------------------------
fisher_scoring <- function(beta_0, y, X, family='poisson', iter=0){
  # Function to fit a Fisher scoring algorithm for the Poisson model
  eta <- X %*% beta_0 # Eta vector
  mu <- exp(eta) # Set mean vector
  dmu_deta <- exp(eta) # Derivative of score function
  A <- diag(as.vector(1 / mu * dmu_deta)) 
  W <- diag(as.vector(1 / mu * dmu_deta^2))
  fisher_inv <- solve(t(X) %*% W %*% X) # Inverse of Fisher Information matrix
  beta_1 <- beta_0 + fisher_inv %*% t(X) %*% A %*% (y - mu) # New iteration, beta_0+[I(B)^-1]*S(B)
  if(max(abs(beta_1 - beta_0)) < 0.00001){
    return(list(coefficients = data.frame(Estimate = beta_1, 
                                          SE = sqrt(diag(fisher_inv))),
                deviance = 2 * y * log(y / mu),
                iter = iter))}
  else{return(fisher_scoring(as.vector(beta_1), y = y , X = X , family = family, iter = iter + 1))}
}

predict_fifa <- function(tt,team1,team2,han=0.5,bayesian=FALSE){
  if(bayesian == TRUE){
    L <- length(tt)
  } else {
    L <- length(tt$coefficients$Estimate)
  }
  x_new <- matrix(0, length(team1), L)
  han <- rep(han, length(team1))
  for(i in 1:length(team1)){
    x_new[i, ] <- c(1, rep(0, L - 2), han[i])
    if (bayesian == FALSE){
      inds <- which(rownames(tt$coefficients) %in% paste0(c('team', 'opponent'), 
                                                          c(team1[i], team2[i])))
    } else {
      inds <- which(names(tt) %in% paste0(c('team', 'opponent'), c(team1[i], team2[i])))
    }
    x_new[i, inds] <- 1
  }
  if (!bayesian){
    return(as.vector(exp(x_new %*% tt$coefficients$Estimate)))
  } else {
    return(as.vector(exp(x_new %*% tt)))
  }
}


new_score <- function(tt, team1, team2, han=0.5, bayesian=FALSE){#Predict expected goals of each team
  han <- rep(han, 2)
  return(predict_fifa(tt, 
                      team1 = c(team1, team2), 
                      team2 = c(team2, team1), 
                      han = ifelse(han == rep(0.5, 2), han, 
                                   ifelse(han == rep(1, 2), c(1, 0), c(0, 1))),
                      bayesian))
}


sim_game <- function(tt, rtg1, rtg2, knockout = FALSE, 
                     han = 0.5, legs = FALSE, bayesian=FALSE){#Simulate soccer game
  if (class(rtg1) == 'character'){
    if (legs == TRUE){
      rtgs <- new_score(tt, rtg1, rtg2, 1, bayesian) + 
        new_score(tt, rtg1, rtg2, 0, bayesian)
    } else {
      rtgs <- new_score(tt, rtg1, rtg2, han, bayesian)
    }
    rtg1 <- rtgs[1]
    rtg2 <- rtgs[2]
  }
  final_score <- (dpois(0:ceiling(max(10, rtg1 * 5)), rtg1)) %*% 
    t(dpois(0:ceiling(max(10, rtg2 * 5)), rtg2))
  p_win <- sum(final_score[lower.tri(final_score)])
  p_loss <- sum(final_score[upper.tri(final_score)])
  p_draw <- 1 - p_win - p_loss
  if (!knockout){
    return(list(prob_win=p_win, prob_draw=p_draw, prob_loss=p_loss))
  } else {
    et_final_score <- (dpois(0:ceiling(max(10, rtg1 * 5 / 3)), rtg1 / 3)) %*% 
      t(dpois(0:ceiling(max(10, rtg2 * 5 / 3)), rtg2 / 3))
    dd = p_draw * sum(diag(et_final_score)) / 2
    return(list(prob_win=p_win + p_draw * 
                  sum(et_final_score[lower.tri(et_final_score)]) + dd,
                prob_loss=p_loss + p_draw * 
                  sum(et_final_score[upper.tri(et_final_score)]) + dd))
  }
}


sim_group <- function(tt,teams,number_to_advance=2,n=10000,already_played=NULL,legs=F,bayesian=F){
  if (!legs){
    J <- c(combn(teams, 2))
  } else {
    J <- c(combn(teams, 2), combn(rev(teams), 2))
  }
  if (!bayesian){
    xg <- rep(0,length(J))
    for (i in 1:(length(J)/2)){
      xg[2 * i + -1:0] <- new_score(tt, J[2 * i - 1], J[2 * i], 
                                    han = ifelse(legs, 1, 0.5), bayesian)
    }
  } else {
    xg <- matrix(0, n, length(J))
    for (i in 1:n){
      for (j in 1:(length(J)/2)){
        xg[i, 2 * j -1:0] <- new_score(tt[i, ], J[2 * j - 1], J[2 * j],
                                       han = ifelse(legs, 1, 0.5), bayesian)
      }
    }
  }
  if (is.null(already_played)){
    scores <- matrix(rpois(n * length(J), lambda=xg), 
                     nrow = n, ncol = length(J), byrow = !bayesian)
  } else {
    scores <- matrix(rep(already_played, n), 
                     nrow = n, ncol = length(J), byrow = TRUE)
    for (g in 1:ncol(scores)){
      if (is.na(scores[1, g])){
        scores[, g] <- rpois(n, xg[g])
      }
    }
  }
  points <- matrix(0, nrow=n, ncol=length(J))
  for (i in 1:(length(J)/2)){
    points[, 2 * i - 1] <- ifelse(scores[, 2 * i - 1] > scores[, 2 * i], 3, 
                                  ifelse(scores[, 2 * i - 1] == scores[, 2 * i], 1, 0))
    points[, 2 * i ] <- ifelse(points[, 2 * i - 1] != 1, 3 - points[, 2 * i - 1], 1)
  }
  standings <- matrix(0, ncol = length(teams), nrow = n)
  for (i in 1:length(teams)){
    standings[, i] <- rowSums(points[, which(J == teams[i])])
  }
  places <- t(apply(-standings, 1, order))
  return(matrix(teams[places], nrow=n, ncol=length(teams))[, 1:number_to_advance])
}

knockout_stage <- function(teams, original_teams = worldcupteams, n = 10000, 
                           rounds = 1, legs = FALSE, competition_matrix = NULL){
  if (rounds == 0){
    return(t(teams))
  }
  if (!is.null(competition_matrix)){
    return(cbind(teams, 
                 knockout_stage(t(apply(teams, 1, knockout_game, 
                                        comp_matrix = competition_matrix)), 
                                original_teams, n = n, rounds - 1, legs = legs,
                                competition_matrix = competition_matrix)))
  } else {
    competition_matrix <- matrix(0, nrow = length(original_teams), 
                                 ncol = length(original_teams))
    rownames(competition_matrix) <- original_teams
    colnames(competition_matrix) <- original_teams
    x <- t(combn(original_teams,2))
    if (!legs){
      y <- rep(0,nrow(x))
      for (i in 1:nrow(x)){
        y[i] <- sim_game(tt, x[i, 1], x[i, 2], knockout = TRUE)$prob_loss
      }
    #y = mapply(sim_game,tt=tt,x[,1],x[,2],knockout=T)
    } else {y <- mapply(sim_game, tt, x[, 1], x[, 2], legs = TRUE)}
    competition_matrix[lower.tri(competition_matrix)] <- y #unlist(y[2,])
    for (i in 1:length(original_teams)){
      for (j in i:length(original_teams)){
        competition_matrix[i, j] <- 1 - competition_matrix[j, i]
      }
    }
  }
  return(cbind(teams, 
               knockout_stage(t(apply(teams, 1, knockout_game, 
                                      comp_matrix = competition_matrix)),
                              original_teams, n = n, rounds = rounds - 1,
                              legs = legs, 
                              competition_matrix = competition_matrix)))
}


knockout_game <- function(teams, comp_matrix = competition_matrix){
  g <- length(teams) / 2; h <- runif(g)
  if (g > 1){
    j <- ifelse(h < c(diag(comp_matrix[teams[seq(1, by=2, length=g)],
                                       teams[seq(2, by=2, length=g)]])), -1, 0)
  } else {
    j <- ifelse(h < comp_matrix[teams[1], teams[2]], -1, 0)
  }
  return(teams[2 * (1:g) + j])
}


knockout_bayes <- function(teams, n = 10000, rounds = 4){
  if (rounds==0){return(teams)}
  winner <- matrix(0, nrow = n, ncol = ncol(teams) / 2)
  for (i in 1:n){
    for (j in 1:(ncol(teams)/2)){
      winner[i, j] <- ifelse(
        runif(1) < sim_game(jj[i, ], teams[i, 2 * j - 1],
                            teams[i, 2 * j], knockout=TRUE,
                            bayesian = TRUE)$prob_win,
        teams[i, 2 * j - 1], teams[i, 2 * j])
    }
  }
  return(cbind(teams, knockout_bayes(winner, n = n, rounds = rounds - 1)))
}


qatar_2022 <- function(n = 1e4, bayesian = FALSE){
  if (!bayesian){
    knockout_qualifiers <- cbind(
      sim_group(tt, worldcupteams[1:4], n=n), sim_group(tt, worldcupteams[5:8], n=n),
      sim_group(tt, worldcupteams[9:12], n=n), sim_group(tt, worldcupteams[13:16], n=n),
      sim_group(tt, worldcupteams[17:20], n=n), sim_group(tt, worldcupteams[21:24], n=n),
      sim_group(tt, worldcupteams[25:28], n=n), sim_group(tt, worldcupteams[29:32], n=n)
    )[,c(1, 4, 5, 8, 9, 12, 13, 16, 2, 3, 6, 7, 10, 11, 14, 15)]
    return(knockout_stage(knockout_qualifiers, worldcupteams, n, 4))
  } else {
    a = sim_group(jj[1:n, ], worldcupteams[1:4], n=n, bayesian = TRUE); print('A')
    b = sim_group(jj[1:n, ], worldcupteams[5:8], n=n, bayesian = TRUE); print('B')
    c = sim_group(jj[1:n, ], worldcupteams[9:12], n=n, bayesian = TRUE); print('C')
    d = sim_group(jj[1:n, ], worldcupteams[13:16], n=n, bayesian = TRUE); print('D')
    e = sim_group(jj[1:n, ], worldcupteams[17:20], n=n, bayesian = TRUE); print('E')
    f = sim_group(jj[1:n, ], worldcupteams[21:24], n=n, bayesian = TRUE); print('F')
    g = sim_group(jj[1:n, ], worldcupteams[25:28], n=n, bayesian = TRUE); print('G')
    h = sim_group(jj[1:n, ], worldcupteams[29:32], n=n, bayesian = TRUE); print('H')
    knockout_qualifiers <- cbind(
      a, b, c, d, e, f, g, h)[, c(1, 4, 5, 8, 9, 12, 13, 16, 
                                  2, 3, 6, 7, 10, 11, 14, 15)]
    return(knockout_bayes(knockout_qualifiers, n = n))
  }
}

## Frequentist model------------------------------------------------------------
nteams <- length(which(substr(colnames(X), 1, 4) == 'team'))
team <- newfifa$team; opponent <- newfifa$opponent; type <- newfifa$type
## Set matrix of predictors with dummy coding (Baseline=Albania)
X <- as.matrix(cbind(1, model.matrix(~team - 1), 
                     model.matrix(~opponent - 1), type))[, -c(2, nteams + 1)]
## Fit model using Fisher scoring and zero vector as starting point (SLOW!)
tt <- fisher_scoring(beta_0 = rep(0, ncol(X)), 
                     y = newfifa$team_score, X = X, family = 'poisson')
## Make sure the coefficient estimates are equal to the glm() function
fifa_model <- glm(team_score ~ team + opponent + type, 
                  data = newfifa, family = 'poisson')
all.equal(unname(tt$coefficients$Estimate), unname(fifa_model$coefficients))

## Bayesian model---------------------------------------------------------------
prior_means <- rep(0, 2 * nteams - 1)
## Set World Cup team prior means to 0.5 (Better than average)
prior_means[which(substr(colnames(X), 5, 100) %in% worldcupteams) - 1] <- 0.5 
## Set World Cup opponent means to -0.5 (harder to beat than average team)
prior_means[which(substr(colnames(X), 9, 100) %in% worldcupteams) - 1] <- -0.5 
prior_means[length(prior_means)] <- 0.3 #Home team advantage
gelman <- bayesglm(team_score ~ team + opponent + type, family = 'poisson',
                  data = newfifa, prior.mean = prior_means, 
                  prior.scale = 1, prior.df = Inf)
jj <- coef(sim(gelman, 100000))


## SIMULATIONS------------------------------------------------------------------

#BAYESIAN SIMULATION (Takes about 8s per 1000 simulations)
x <- qatar_2022(n = 1e5, bayesian = TRUE)
champs <- data.frame(Team = worldcupteams, Knockout = 0, Quarterfinal = 0,
                     Semifinal = 0, Final = 0, Champion = 0)
for(i in 1:32){
  champs$Knockout[i] <- length(which(x[, 1:16] == worldcupteams[i]))
  champs$Quarterfinal[i] <- length(which(x[, 17:24] == worldcupteams[i]))
  champs$Semifinal[i] <- length(which(x[, 25:28] == worldcupteams[i]))
  champs$Final[i] <- length(which(x[, 29:30] == worldcupteams[i]))
  champs$Champion[i] <- length(which(x[, 31] == worldcupteams[i]))
}
champs <- champs[order(champs$Champion, decreasing = TRUE), ]
rownames(champs) <- NULL; champs


#FREQUENTIST SIMULATION (Takes about 0.3s per 1000 simulations)
x <- qatar_2022(n = 1e5)
champs <- data.frame(Team = worldcupteams, Knockout = 0, Quarterfinal = 0,
                    Semifinal = 0, Final = 0, Champion = 0)
for(i in 1:32){
  champs$Knockout[i] <- length(which(x[, 1:16] == worldcupteams[i]))
  champs$Quarterfinal[i] <- length(which(x[, 17:24] == worldcupteams[i]))
  champs$Semifinal[i] <- length(which(x[, 25:28] == worldcupteams[i]))
  champs$Final[i] <- length(which(x[, 29:30] == worldcupteams[i]))
  champs$Champion[i] <- length(which(x[, 31] == worldcupteams[i]))
}
champs <- champs[order(champs$Champion, decreasing = TRUE), ]
rownames(champs) <- NULL; champs





######## HELPER FUNCTIONS FOR AGING CURVE + SIMULATION ########

library(tidyverse)
library(tidymodels)
library(modelr)
library(memoise)

#### CUSTOM THEME ####

theme_bbs <- function() {
  font = 'sans'
  bg = "#E4DBD7"
  light_ln = "#A0A0A0"
  dark_ln = "#404040"
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = bg, color = NA),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.background = element_blank(),
          panel.grid = element_line(color = light_ln),
          panel.grid.minor = element_blank(),
          plot.title = element_text(family = font, size = 18, face = 'bold', hjust = 0.5, vjust = 2),
          plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, vjust = 1),
          plot.caption = element_text(family = font, size = 9, hjust = 1, vjust = -5, color = dark_ln),
          axis.title.x = element_text(family = font, size = 15, vjust = -2, color = dark_ln),
          axis.title.y = element_text(family = font, size = 15,  angle = 90, vjust = 3, color = dark_ln),
          axis.text = element_text(family = font, size = 13, color = dark_ln),
          legend.title = element_text(family = font, size = 13, color = dark_ln, face = 'bold', hjust = 0.5),
          legend.text = element_text(family = font, size = 12, color = dark_ln),
          legend.box.background = element_blank(),
          axis.ticks = element_line(color = light_ln),
          plot.margin = unit(c(1,1,1,1),"cm"))
}




#### DATA COLLECTION ####

collect_stats <- function(table, variable, minimum_mp = 500, minimum_g = 1, wt_var = "mp") {
  new_table <- table %>%
    mutate(current_stat = !!sym(variable)) %>%
    group_by(player_id) %>%
    arrange(season) %>%
    mutate(lag_season = lag(season),
           lag_mp = lag(mp),
           lag_g = lag(g),
           lag_stat = lag(!!sym(variable))) %>%
    ungroup() %>%
    mutate(raw_change = current_stat - lag_stat,
           pct_change = current_stat / lag_stat) %>%
    filter(!is.na(lag_stat),
           lag_mp >= minimum_mp,
           mp >= minimum_mp,
           lag_g >= minimum_g,
           g >= minimum_g)
  
  new_table <- new_table %>%
    left_join(draft, by = c("player_id")) %>%
    group_by(player, player_id) %>%
    mutate(min_season = min(season),
           lag_raw_change = lag(raw_change)) %>%
    ungroup() %>%
    mutate(lag_raw_change = ifelse(is.na(lag_raw_change), 0, lag_raw_change)) %>%
    filter(!is.na(pick_overall) | min_season >= 1995)
  
  career <- table %>%
    mutate(current_stat = !!sym(variable),
           weight = !!sym(wt_var)) %>%
    group_by(player_id) %>%
    arrange(season) %>%
    mutate(wt_adj = current_stat * weight,
           sum_stat = cumsum(wt_adj),
           sum_wt = cumsum(weight),
           career_stat = sum_stat / sum_wt) %>%
    ungroup() %>%
    select(player_id, season, career_stat)
  
  new_table <- new_table %>%
    left_join(career, by = c("player_id", "lag_season" = "season")) %>%
    mutate(pick_overall = ifelse(is.na(pick_overall), 80, pick_overall))
  return(new_table)
}



add_career_progression <- function(table, alpha, wt_var) {
  all_player_ids <- unique(table$player_id)
  data <- list(player_id = c(), season = c(), weighted_lag_change = c())
  for (pid in all_player_ids) {
    player_table <- table %>% filter(player_id == pid)
    yrs <- player_table$season
    vals <- sapply(yrs, function(x) {
      player_table <- player_table %>%
        mutate(weight = !!sym(wt_var),
               lag_weight = !!sym(paste0("lag_", wt_var)),
               yrs_ago = x - season,
               change_wt = ifelse(yrs_ago > 0, (alpha^yrs_ago) * sqrt(weight * lag_weight), 0),
               change_wt = change_wt / sum(change_wt)) %>%
        summarize(weighted_lag_change = weighted.mean(raw_change, change_wt)) %>%
        pull()
    })
    vals <- replace_na(vals, 0)
    data$player_id <- append(data$player_id, rep(pid, length(yrs)))
    data$season <- append(data$season, yrs)
    data$weighted_lag_change <- append(data$weighted_lag_change, vals)
  }
  
  weighted_lags <- data.frame(data)
  
  table <- table %>%
    inner_join(weighted_lags, by = c("player_id", "season"))
  
  return(table)
}



plot_changes <- function(table, varname, pct = TRUE, min_samples = 50) {
  if (pct) {
    base <- 1
    v <- 'pct_change'
    ylab <- paste("% Change in", varname)
  } else {
    base <- 0
    v <- 'raw_change'
    ylab <- paste("Change in", varname)
  }
  
  g <- table %>%
    mutate(var = !!sym(v)) %>%
    group_by(age) %>%
    summarize(n = n(),
              mean = mean(var),
              sd = sd(var),
              se = sd / sqrt(n),
              upper = mean + 2 * se,
              lower = mean - 2 * se) %>%
    filter(n > 50) %>%
    ggplot(aes(x = age, y = mean)) +
    geom_point(aes(size = n)) +
    geom_line(aes(y = upper)) + 
    geom_line(aes(y = lower)) +
    geom_hline(yintercept = base) +
    geom_smooth() +
    labs(x = "Age",
         y = ylab,
         size = "# Samples",
         title = paste("Career Progression of", varname))
  return(g)
}



plot_player <- function(table, playerId, varname, min_mp = 500, min_g = 1) {
  g <- table %>%
    filter(player_id == playerId) %>%
    mutate(current_stat = !!sym(varname)) %>%
    ggplot(aes(x = age, y = current_stat)) +
    geom_point(aes(size = mp)) +
    geom_smooth() +
    labs(x = "Age",
         y = varname,
         size = "MP",
         title = paste("Career Progression of", playerId, varname))
  return(g)
}




#### DATA PROCESSING ####

trainTestSplit <- function(data, p = 0.75) {
  n <- round(p * nrow(data))
  train_rows <- sample(1:nrow(data), n)
  train_data <- data[train_rows, ]
  test_data <- data[-train_rows, ]
  return(list(train = train_data, test = test_data))
}



formatMatrix <- function(data, predictors, response, secondOrder = T, withResponse = T) {
  if (withResponse) {
    y <- data[, c(response)]
  }
  X1 <- data[, c(predictors)]
  if (!secondOrder) {
    if (withResponse) {return(cbind(X1, y))}
    else {return(X1)}
  } 
  X2 <- X1^2
  colnames(X2) <- paste0(colnames(X1), "_sqrd")
  for (i in 1:(length(predictors)-1)) {
    for (j in (i + 1):length(predictors)) {
      vi <- colnames(X1)[i]
      vj <- colnames(X1)[j]
      v <- paste(vi, vj, sep = "_x_")
      X2[[v]] <- X1[[vi]] * X1[[vj]]
    }
  }
  X <- cbind(X1, X2)
  if (withResponse) {return(cbind(X, y))}
  else {return(X)}
}



storeZInfo <- function(df) {
  means <- colMeans(df)
  stdevs <- apply(df, 2, sd)
  return(list(mean = means, std = stdevs))
}



standardize <- function(df, mean, std) {
  for (col in colnames(df)) {
    df[[col]] <- (df[[col]] - mean[col]) / std[col]
  }
  return(df)
}



unstandardize <- function(df, mean, std) {
  for (col in colnames(df)) {
    df[[col]] <- std[col] * df[[col]] + mean[col]
  }
  return(df)
}




#### SIMPLE MODELING ####

fitModel <- function(matrix, response) {
  f <- lm(as.formula(paste(response, "~ .")), data = matrix)
  return(f)
}



fitWeightedModel <- function(matrix, response, weights) {
  f <- lm(as.formula(paste(response, "~ .")), data = matrix, weights = weights)
  return(f)
}




#### BAYESIAN MODELING FUNCTIONS ####

relu <- function(x) {
  x[x < 0] <- 0
  return(x)
}



probPrior_p <- function(priors, p, i, log = T) {
  prob1 <- priors$p[i]
  d <- as.numeric(prob1 * p + (1 - prob1) * (1 - p))
  if (log) {
    return(log(d))
  } else {
    return(d)
  }
}



probPrior_beta <- function(priors, beta, log = T) {
  return(dnorm(beta, priors$beta_mean, priors$beta_var, log))
}



probPrior_sigma <- function(priors, sigma2, log = T) {
  return(dgamma(1 / sigma2, priors$nu / 2, priors$nu * priors$sigma2 / 2, log = log))
}



probPrior_sigma_p <- function(priors, p, i, log = T) {
  prob1 <- priors$sigma2_p[i]
  d <- as.numeric(prob1 * p + (1 - prob1) * (1 - p))
  if (log) {
    return(log(d))
  } else {
    return(d)
  }
}



probPrior_sigma_beta <- function(priors, beta, log = T) {
  return(dnorm(beta, priors$sigma2_beta, priors$sigma2_beta_var, log))
}



probSampling <- function(X, y, params) {
  p <- params$p
  beta <- params$beta
  sigma2 <- params$sigma2
  sigma2_p <- params$sigma2_p
  sigma2_beta <- params$sigma2_beta
  
  beta[p == 0] <- 0
  yhat <- X %*% beta
  sigma2_beta[sigma2_p == 0] <- 0
  yhat_sigma2 <- relu(X %*% sigma2_beta)
  final_sigma <- sqrt(sigma2 + yhat_sigma2)
  return(sum(dnorm(y, yhat, final_sigma, log = T)))
}



draw_p <- function(p) {
  return(rbinom(1, 1, 0.5))
}



draw_fc_p <- function(X, y, current, priors, i) {
  temp <- current
  temp$p[i] <- 0
  log_p0 <- probSampling(X, y, temp) + probPrior_p(priors, 0, i)
  temp$p[i] <- 1
  log_p1 <- probSampling(X, y, temp) + probPrior_p(priors, 1, i)
  maxlog <- max(c(log_p0, log_p1))
  log_p0 <- log_p0 - maxlog
  log_p1 <- log_p1 - maxlog
  prob1 <- exp(log_p1) / (exp(log_p0) + exp(log_p1))
  return(rbinom(1, 1, prob1))
}



draw_fc_sigma2_p <- function(X, y, current, priors, i) {
  temp <- current
  temp$sigma2_p[i] <- 0
  log_p0 <- probSampling(X, y, temp) + probPrior_sigma_p(priors, 0, i)
  temp$sigma2_p[i] <- 1
  log_p1 <- probSampling(X, y, temp) + probPrior_sigma_p(priors, 1, i)
  maxlog <- max(c(log_p0, log_p1))
  log_p0 <- log_p0 - maxlog
  log_p1 <- log_p1 - maxlog
  prob1 <- exp(log_p1) / (exp(log_p0) + exp(log_p1))
  return(rbinom(1, 1, prob1))
}



draw_beta <- function(beta, delta) {
  return(rnorm(1, beta, delta))
}



rmvnorm <- function(n,mu,Sigma) { 
  p<-length(mu) 
  res<-matrix(0,nrow=n,ncol=p) 
  if(n>0 & p>0) {
    E<-matrix(rnorm(n*p),n,p)
    res<-t(  t(E%*%chol(Sigma)) +c(mu))
  }
  return(res)
}



draw_fc_beta <- function(X, y, current, priors) {
  p <- current$p
  if (sum(p) == 0) {return(current$beta)}
  Xp <- X[, which(p != 0)]
  Xps2 <- X[, which(current$sigma2_p != 0)]
  alpha <- current$sigma2_beta[current$sigma2_p != 0]
  prev_beta <- current$beta
  
  sig <- relu(as.vector(Xps2 %*% alpha)) + current$sigma2
  sigma <- solve(t(Xp) %*% (Xp * sig) + (1 / priors$beta_var) * diag(sum(p)))
  theta <- sigma %*% t(Xp * sig) %*% y
  
  sim_beta <- t(rmvnorm(1, theta, sigma))
  new_beta <- prev_beta
  new_beta[p != 0] <- sim_beta
  return(new_beta)
}



draw_sigma2 <- function(sigma2, eta) {
  return(rgamma(1, eta, eta / sigma2))
}



pJ_sigma2 <- function(new, old, eta) {
  return(dgamma(new, eta, eta / old))
}



get_delta <- function(i, start_delta, min_delta, scale = 1000) {
  return(min_delta + exp(-(i-1)/scale) * (start_delta - min_delta))
}



adjust_delta <- function(i, start_delta, min_delta, sd, scale = 1000) {
  d <- get_delta(i, start_delta, min_delta, scale)
  if (is.na(sd)) {sd <- 0}
  wt <- 2 / (1 + exp(-i/1000)) - 1
  return(wt * sd + (1 - wt) * d)
}



get_eta <- function(i, eta_min, eta_max, scale = 1000) {
  return(eta_max - exp(-(i-1)/scale) * (eta_max - eta_min))
}



accept_rate <- function(v) {
  w <- v[v != 0]
  return(1 - mean(w == lag(w), na.rm = T))
}



plot_sims <- function(v, label = "value") {
  w <- v[v != 0]
  plot(1:length(w), w, xlab = "Non-zero Iteration", ylab = label)
}



plot_sims_gg <- function(v, label = "value") {
  g <- ggplot(mapping = aes(x = 1:length(v), y = v)) +
    geom_point(shape = 1) +
    labs(x = "Non-zero Iteration",
         y = label)
  return(g)
}



mh_algorithm <- function(X, y, priors, n_sim, n_drop = 0, delta_max = 0.2, delta_min = 0.05,
                         eta_min = 2, eta_max = 10) {
  
  # initialize current values for each parameter
  current <- list(
    p = rep(1, length(priors$p)),
    # initialize beta with OLS estimates to improve convergence
    beta = as.matrix(unname(solve(t(X) %*% X) %*% t(X) %*% y)),
    sigma2 = priors$sigma2,
    sigma2_p = rep(1, length(priors$sigma2_p)),
    sigma2_beta = as.matrix(rnorm(ncol(X), 0, SIGMA_LAMBDA))
  )
  
  # initialize saved values after each iteration
  saves <- list(
    p = NULL,
    beta = NULL,
    sigma2 = NULL,
    sigma2_p = NULL,
    sigma2_beta = NULL
  )
  
  # MH Algorithm loop
  for (i in 1:(n_sim + n_drop)) {
    # update status
    if (i == 1 & n_drop > 0) {
      print(paste("Beginning with", n_drop, "dropout samples to converge..."))
    }
    if (i == n_drop + 1 & n_drop > 0) {
      print(paste("Completed dropout samples. Beginning", n_sim, "simulations..."))
    }
    if (i %% 100 == 0 & i > n_drop) {
      print(paste0("Completed simulation ", i - n_drop, "/", n_sim, " (", round(100*(i-n_drop)/n_sim), "%)"))
    }
    
    # update p's (using full conditional)
    for (j in 1:length(current$p)) {
      new_p <- draw_fc_p(X, y, current, priors, j)
      current$p[j] <- new_p
    }
    
    # update beta's (using full conditional)
    new_beta <- draw_fc_beta(X, y, current, priors)
    current$beta <- new_beta
    
    # update sigma2 
    # draw new sigma from proposal distribution
    eta <- get_eta(i, eta_min, eta_max)
    new_s2 <- draw_sigma2(current$sigma2, eta)
    # calculate prob accept
    p_old <- probPrior_sigma(priors, current$sigma2) + probSampling(X, y, current)
    temp <- current
    temp$sigma2 <- new_s2
    p_new <- probPrior_sigma(priors, new_s2) + probSampling(X, y, temp)
    adj_factor <- pJ_sigma2(current$sigma2, new_s2, eta) - pJ_sigma2(new_s2, current$sigma2, eta)
    r <- (p_new - p_old) + adj_factor
    # update
    if (log(runif(1)) < r) {
      current$sigma2 <- new_s2
    } 
    
    # update sigma2 p's (using full conditional)
    for (j in 1:length(current$sigma2_p)) {
      new_p <- draw_fc_sigma2_p(X, y, current, priors, j)
      current$sigma2_p[j] <- new_p
    }
    
    # update sigma2 beta's
    for (j in 1:length(current$sigma2_beta)) {
      # only run if corresponding p != 0
      if (current$sigma2_p[j] == 0) {
        next
      }
      # draw new beta from proposal distribution
      new_beta <- draw_beta(current$sigma2_beta[j], 
                            adjust_delta(i, delta_max, delta_min, sd(saves$sigma2_beta[,j][saves$sigma2_p[,j] != 0])))
      # calculate prob accept
      p_old <- probPrior_sigma_beta(priors, current$sigma2_beta[j]) + probSampling(X, y, current)
      temp <- current
      temp$sigma2_beta[j] <- new_beta
      p_new <- probPrior_sigma_beta(priors, new_beta) + probSampling(X, y, temp)
      r <- p_new - p_old
      # update
      if (log(runif(1)) < r) {
        current$sigma2_beta[j] <- new_beta
      } 
    }
    
    # SAVE
    if (i > n_drop) {
      saves$p <- rbind(saves$p, current$p)
      saves$beta <- rbind(saves$beta, t(current$beta))
      saves$sigma2[i - n_drop] <- current$sigma2
      saves$sigma2_p <- rbind(saves$sigma2_p, current$sigma2_p)
      saves$sigma2_beta <- rbind(saves$sigma2_beta, t(current$sigma2_beta))
    }
  }
  
  # set values to 0 for turned off parameters in saved values
  saves$beta[saves$p == 0] <- 0
  saves$sigma2_beta[saves$sigma2_p == 0] <- 0
  
  return(saves)
}



predictions <- function(df, params, zInfo, features, response) {
  # format feature matrix
  X <- formatMatrix(df, features, NULL, secondOrder = TRUE, withResponse = FALSE)
  X <- as.matrix(standardize(X, zInfo$mean, zInfo$std))
  # get predictions in standardized units
  yhat <- X %*% params$beta
  var <- params$sigma2 + relu(X %*% params$sigma2_beta)
  se <- sqrt(var)
  # unstandardize
  yhat <- zInfo$mean[response] + zInfo$std[response] * yhat 
  se <- zInfo$mean[response] + zInfo$std[response] * se
  preds <- data.frame(yhat = yhat, se = se)
  return(preds)
}



predictive_dist <- function(X_row, saves, zInfo, features, response, n = 1000) {
  m <- length(saves$sigma2)
  # format feature matrix (one row matrix)
  X <- formatMatrix(X_row, features, NULL, secondOrder = TRUE, withResponse = FALSE)
  X <- as.matrix(standardize(X, zInfo$mean, zInfo$std))
  # simulate in standardized units
  iters <- sample(1:m, size = n, replace = T)
  yhat <- sapply(iters, function(i) {as.numeric(X %*% as.matrix(saves$beta[i, ]))})
  var <- sapply(iters, 
                function(i) {saves$sigma2[i] + relu(as.numeric(X %*% as.matrix(saves$sigma2_beta[i,])))})
  sims <- rnorm(n, yhat, sqrt(var))
  # unstandardize
  sims <- zInfo$mean[response] + zInfo$std[response] * sims
  return(sims)
}



calculate_rmse <- function(actual, predicted) {
  return(sqrt(mean((actual - predicted)^2)))
}




#### CAREER SIMULATION FUNCTIONS ####

best_stretch <- function(vals, len) {
  if (length(vals) < len) {
    return(NA)
  }
  roll_mean <- function(x, n) {
    y <- c(0,cumsum(x))
    return(sapply(n:length(x), function(i) {(y[i + 1] - y[i - n + 1]) / n}))
  }
  return(max(roll_mean(vals, len)))
}



classify_performance <- function(war) {
  lev <- case_when(
    war > 20 ~ "MVP Candidate",
    war > 15 ~ "All-NBA Level",
    war > 10 ~ "All-Star Level",
    war > 5 ~ "Starter",
    war > 2.5 ~ "Bench",
    war >= 0 ~ "Minimal Impact",
    war < 0 ~ "Negative Impact"
  )
  if (length(lev) > 1) {
    lev <- factor(lev, levels = c("Negative Impact", "Minimal Impact", "Bench", "Starter", 
                                  "All-Star Level", "All-NBA Level", "MVP Candidate"))
  }
  return(lev)
}



summarize_sims <- function(sim_war) {
  df <- data.frame(iteration = 1:length(sim_war),
                   war = sim_war)
  df <- df %>%
    mutate(level = classify_performance(war))
  return(df)
}



plot_sim_levels <- function(sims, fill = "gray", color = "black") {
  df <- summarize_sims(sims)
  g <- df %>%
    group_by(level) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(p = n / sum(n)) %>%
    ggplot(aes(x = level, y = p)) +
    geom_col(fill = fill, color = color) +
    geom_hline(yintercept = 0, color = 'black') +
    labs(x = "Outcome", y = "% of Simulations") +
    scale_y_continuous(labels = scales::percent) 
  return(g)
}



sim_hist <- function(sims, fill = "gray", color = "black") {
  df <- summarize_sims(sims)
  g <- df %>%
    ggplot(aes(x = war, bins = 20)) +
    geom_histogram(fill = fill, color = color, linewidth = 0.25) +
    labs(x = "Best Simulated 5-Year WAR/82", y = "Count")
  return(g)
}



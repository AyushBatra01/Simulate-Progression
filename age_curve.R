rm(list=ls())
library(tidymodels)
library(hoopR)
library(modelr)
library(memoise)
library(patchwork)
library(tidyverse)
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)

source("age_curve_functions.R")
MY_COLOR <- "#96eea1"


######## LOAD IN DATA ########

perposs <- read_csv("data/perposs.csv")
advanced <- read_csv("data/advanced.csv")
draft <- read_csv("data/draft.csv")

perposs <- perposs %>%
  relocate(all_of(c("player_id", "season")), .after = player)
advanced <- advanced %>%
  relocate(all_of(c("player_id", "season")), .after = player)

draft <- draft %>%
  select(player_id, draft_season = season, pick_overall)




######## CLEANING + PLOTTING ########

advanced <- advanced %>%
  mutate(WAR_82 = 82 * 2.70 * vorp / g)

player_df <- collect_stats(advanced, "WAR_82", draft, 
                           minimum_mp = 1, minimum_g = 25, wt_var = "g")
player_df <- add_career_progression(player_df, alpha = 0.5, wt_var = "g")
player_df <- player_df %>% mutate(log_pick = log(pick_overall))


# Some EDA
plot_changes(player_df, "WAR / 82 games", pct = F) +
  labs(y = "Avg Change in WAR / 82") 

war_dist_plot <- player_df %>%
  ggplot(aes(x = WAR_82)) +
  geom_histogram(fill = MY_COLOR, color = 'black') +
  labs(x = "Season WAR/82",
       y = "Count",
       title = "Distribution of Single Season WAR/82") +
  theme_bbs()

player_df %>%
  mutate(level = classify_performance(WAR_82)) %>%
  ggplot(aes(x = level)) +
  geom_bar(fill = MY_COLOR, color = 'black')

change_dist_plot <- player_df %>%
  ggplot(aes(x = raw_change)) +
  geom_histogram(fill = MY_COLOR, color = 'black') +
  labs(x = "Change in WAR/82",
       y = "Count",
       title = "Distribution of WAR/82 Change") +
  theme_bbs()
mean(player_df$raw_change)
sd(player_df$raw_change)


intro_plots <- (war_dist_plot + change_dist_plot)


cor(player_df[c("career_stat", "lag_stat", "lag_raw_change", 
                "weighted_lag_change", "log_pick", "age", "raw_change")])

player_df %>%
  ggplot(aes(x = lag_stat, y = raw_change)) +
  geom_point(alpha = 0.25) +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  geom_smooth() +
  labs(x = "Previous Season WAR/82",
       y = "Change in WAR/82")

player_df %>%
  ggplot(aes(x = as.character(age), y = raw_change)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  geom_smooth() +
  labs(x = "Age",
       y = "Change in WAR/82")

change_by_lag <- player_df %>%
  mutate(lag_stat = ifelse(lag_stat > 20, 19.9, lag_stat),
         lag_category = classify_performance(lag_stat)) %>%
  ggplot(aes(x = raw_change)) +
  geom_histogram(aes(y = after_stat(density)), fill = MY_COLOR, color = 'black') +
  facet_wrap(~lag_category) +
  labs(x = "Change in WAR/82",
       y = "",
       title = "Distribution of WAR/82 Change") +
  theme_bbs() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12)) 
change_by_lag

change_by_pick <- player_df %>%
  filter(lag_stat < 10) %>%
  mutate(pick_category = case_when(
    pick_overall < 6 ~ "Top 5",
    pick_overall < 15 ~ "Lottery",
    pick_overall < 31 ~ "First Round",
    pick_overall < 61 ~ "Second Round",
    pick_overall < 100 ~ "Undrafted"
  )) %>%
  mutate(pick_category = factor(pick_category, 
                                levels = c("Top 5", "Lottery", "First Round", "Second Round", "Undrafted"))) %>%
  ggplot(aes(x = raw_change)) +
  geom_histogram(aes(y = after_stat(density)), fill = MY_COLOR, color = 'black') +
  facet_wrap(~pick_category) +
  labs(x = "Change in WAR/82",
       y = "",
       title = "Distribution of WAR/82 Change",
       subtitle = "Only including players with previous WAR/82 below 10.0") +
  theme_bbs() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12)) 
change_by_pick

player_df %>%
  mutate(age_category = case_when(
    age < 24 ~ "19-23",
    age < 27 ~ "24-26",
    age < 31 ~ "27-30",
    age >= 31 ~ "31+",
  )) %>%
  group_by(age_category) %>%
  summarize(avg = mean(raw_change),
            stdev = sd(raw_change),
            n = n())



######## PROCESSING + MODELING ########

# Split into train and test set
set.seed(123)
split <- trainTestSplit(player_df)
player_train <- split$train
player_test <- split$test




######## MODELING - OLS ########

features <- c("career_stat", "lag_stat", "lag_raw_change", 
              "weighted_lag_change", "log_pick", "age")
response <- "raw_change"
Xy <- formatMatrix(player_train, features, response, secondOrder = F)
ols_model <- fitModel(Xy, response)
summary(ols_model)
rmse(ols_model, player_train)
rmse(ols_model, player_test)

resids <- resid(ols_model)
hist(resids)

resid_plot <- player_train %>%
  ggplot(aes(x = lag_stat, y = resids)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, color = 'red') +
  labs(x = "Previous WAR/82",
       y = "Residual",
       title = "Residual Plot: Nonconstant variance") +
  theme_bbs()

resid_plot_grouped <- player_train %>%
  mutate(lag_stat = ifelse(lag_stat > 20, 19.9, lag_stat),
         lag_category = classify_performance(lag_stat)) %>%
  ggplot(aes(x = resids)) +
  geom_histogram(aes(y = after_stat(density)), fill = MY_COLOR, color = 'black') +
  facet_wrap(~lag_category) +
  labs(x = "Residual",
       y = "",
       title = "Distribution of Residuals") +
  theme_bbs() +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size = 12))

resid_plots <- resid_plot + resid_plot_grouped +
  plot_layout(widths = c(2,3))
resid_plots




######## MODELING - BAYESIAN MODEL AVERAGING ########

features <- c("career_stat", "lag_stat", "lag_raw_change", 
              "weighted_lag_change", "log_pick", "age")
response <- "raw_change"
Xy_train <- formatMatrix(player_train, features, response, secondOrder = TRUE)
Xy_test <- formatMatrix(player_test, features, response, secondOrder = TRUE)

zInfo <- storeZInfo(Xy_train)
Xy_train <- standardize(Xy_train, zInfo$mean, zInfo$std)
Xy_test <- standardize(Xy_test, zInfo$mean, zInfo$std)

X_train <- Xy_train %>% select(-all_of(response)) %>% as.matrix()
y_train <- Xy_train %>% select(all_of(response)) %>% as.matrix()
X_test <- Xy_test %>% select(-all_of(response)) %>% as.matrix()
y_test <- Xy_test %>% select(all_of(response)) %>% as.matrix()


# Set Priors
LAMBDA = 1
SIGMA_LAMBDA = 1
priors <- list(
  p = sapply(colnames(X_train), 
             function(x) {if (grepl("_x_", x) | grepl("_sqrd", x)) {0.25} else {0.5}}),
  beta_mean = 0,
  beta_var = LAMBDA,
  nu = 1,
  sigma2 = 1,
  sigma2_p = sapply(colnames(X_train), 
                    function(x) {if (grepl("_x_", x) | grepl("_sqrd", x)) {0.125} else {0.25}}),
  sigma2_beta = 0,
  sigma2_beta_var = SIGMA_LAMBDA
)


# Run Metropolis-Hastings algorithm
set.seed(314)
saves <- mh_algorithm(X_train, y_train, priors, n_sim = 10000, n_drop = 100)

# FOR LOADING IN SAVES INFO INSTEAD OF RUNNING IT
LOAD <- TRUE
if (LOAD) {
  saves <- list()
  files <- c("p", "beta", "sigma2", "sigma2_p", "sigma2_beta")
  for (f in files) {
    df <- read.table(paste0("saved/", f, ".txt"), header = T, row.names = 1)
    if (ncol(df) == 1) {
      saves[[f]] <- df[[colnames(df)]]
    } else {
      saves[[f]] <- as.matrix(df)
    }
  }
}

# use posterior means as final values
final <- list(
  p = setNames(colMeans(saves$p), colnames(X_train)),
  beta = setNames(colMeans(saves$beta), colnames(X_train)),
  var_beta = var(saves$beta),
  sigma2 = mean(saves$sigma2),
  sigma2_p = setNames(colMeans(saves$sigma2_p), colnames(X_train)),
  sigma2_beta = setNames(colMeans(saves$sigma2_beta), colnames(X_train)),
  sigma2_var_beta = var(saves$sigma2_beta)
)

sort(final$beta)
sort(final$p)
sort(final$sigma2_beta)
sort(final$sigma2_p)

calculate_rmse(predictions(player_train, final, zInfo, features, response)$yhat, 
               player_train$raw_change)
calculate_rmse(predictions(player_test, final, zInfo, features, response)$yhat,
               player_test$raw_change)


# calculate mean log likelihood for MH Algorithm estimates vs OLS estimates on test set 
ols_params <- list(
  p = rep(1, length(priors$p)),
  beta = as.matrix(unname(solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train)),
  sigma2 = mean((y_train - as.matrix(unname(X_train %*% solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train)))^2),
  sigma2_p = rep(0, length(priors$sigma2_p)),
  sigma2_beta = rep(0, length(priors$sigma2_p))
)
probSampling(X_test, y_test, final) / nrow(X_test)
probSampling(X_test, y_test, ols_params) / nrow(X_test)


# Tools for visualizing mixture of MH samples
plot_sims(saves$sigma2, "Baseline Variance")
plot_sims(saves$beta[,1], "Beta 1")

coda::effectiveSize(saves$sigma2)
coda::effectiveSize(saves$beta)
coda::effectiveSize(saves$sigma2_beta)



# Compare Performance - RMSE
ols_train_rmse <- rmse(ols_model, player_train)
ols_test_rmse <- rmse(ols_model, player_test)
bma_train_rmse <- calculate_rmse(predictions(player_train, final, zInfo, features, response)$yhat, 
                                 player_train$raw_change)
bma_test_rmse <- calculate_rmse(predictions(player_test, final, zInfo, features, response)$yhat, 
                                player_test$raw_change)

perf_table <- matrix(c(ols_train_rmse, ols_test_rmse, bma_train_rmse, bma_test_rmse),
                     nrow = 2, ncol = 2,
                     dimnames = list(c("Train", "Test"), c("OLS", "BMA")))
round(perf_table, 3)

model_comp_rmse <- as.data.frame(perf_table) %>%
  add_column(set = row.names(perf_table)) %>%
  pivot_longer(cols = c(OLS, BMA), names_to = "model", values_to = "rmse") %>%
  mutate(baseline = calculate_rmse(player_df$raw_change, mean(player_df$raw_change)),
         reduction = -1 * (rmse - baseline) / baseline) %>%
  ggplot(aes(x = set, y = reduction, fill = model)) +
  geom_col(position = 'dodge', color = 'black', width = 0.7) +
  geom_hline(yintercept = 0, color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Dataset",
       y = "% Reduction in RMSE from Baseline Model",
       fill = "Model",
       title = "Model Performance: RMSE",
       subtitle = "OLS vs Bayesian Model Averaging (BMA)") +
  theme_bbs()


# Compare Performance - Mean Log Likelihood
ols_train_mll <- probSampling(X_train, y_train, ols_params) / nrow(X_train)
ols_test_mll <- probSampling(X_test, y_test, ols_params) / nrow(X_test)
bma_train_mll <- probSampling(X_train, y_train, final) / nrow(X_train)
bma_test_mll <- probSampling(X_test, y_test, final) / nrow(X_test)

mll_table <- matrix(c(ols_train_mll, ols_test_mll, bma_train_mll, bma_test_mll),
                     nrow = 2, ncol = 2,
                     dimnames = list(c("Train", "Test"), c("OLS", "BMA")))
round(mll_table, 3)

base_params <- ols_params
base_params$beta <- base_params$beta * 0
base_params$sigma2 <- sd(y_train)
baseline_mll <- probSampling(X_train, y_train, base_params) / nrow(X_train)

model_comp_mll <- as.data.frame(mll_table) %>%
  add_column(set = row.names(mll_table)) %>%
  pivot_longer(cols = c(OLS, BMA), names_to = "model", values_to = "MLL") %>%
  mutate(baseline = baseline_mll,
         improvement = -1 * (MLL - baseline) / baseline) %>%
  ggplot(aes(x = set, y = improvement, fill = model)) +
  geom_col(position = 'dodge', color = 'black', width = 0.7) +
  geom_hline(yintercept = 0, color = 'black') +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Dataset",
       y = "% Improvement in MLL from Baseline Model",
       fill = "Model",
       title = "Model Performance: Mean Log Likelihood",
       subtitle = "OLS vs Bayesian Model Averaging (BMA)") +
  theme_bbs()

performance_plots <- model_comp_rmse + model_comp_mll
performance_plots


# Mixing Plots
sigma2_mixing <- plot_sims_gg(saves$sigma2, "Baseline Variance") +
  labs(x = "Iteration Number",
       title = "Baseline Variance Mixing") +
  theme_bbs()
sigma2_mixing




#### RESULTS ####

plot_coefs <- function(final, features, order, key, label, percent = F) {
  convert <- c(
    "career_stat" = "Career WAR/82",
    "lag_stat" = "Previous WAR/82",
    "lag_raw_change" = "Previous Change",
    "weighted_lag_change" = "Weighted Change",
    "log_pick" = "Log Pick",
    "age" = "Age"
  )
  
  if (order == 1) {
    feats <- features[!grepl(".*_sqrd", features) & !grepl(".*_x_.*", features)]
    first_order_df <- data.frame(
      feature = convert[feats],
      var = final[[key]][feats]
    )
    g <- first_order_df %>%
      ggplot(aes(y = feature, x = var)) +
      geom_col(fill = MY_COLOR, color = 'black', width = 0.7) +
      labs(x = label,
           y = "Feature") +
      geom_vline(xintercept = 0) +
      theme_bbs()
    if (percent) {
      g <- g +
        scale_x_continuous(labels = scales::percent)
    }
    return(g)
  } else if (order == 2) {
    second_order_df <- data.frame(
      feature = names(final[[key]]),
      var = final[[key]],
      row.names = NULL
    )
    second_order_df <- second_order_df %>%
      filter(!(feature %in% features)) %>%
      mutate(f1 = ifelse(str_detect(feature, ".*_sqrd"), 
                         str_extract(feature, "(.*)_sqrd", group = 1),
                         str_extract(feature, "(.*)_x_", group = 1)),
             f2 = ifelse(str_detect(feature, ".*_sqrd"), 
                         str_extract(feature, "(.*)_sqrd", group = 1),
                         str_extract(feature, ".*_x_(.*)", group = 1)))
    second_order_df$f1 <- convert[second_order_df$f1]
    second_order_df$f2 <- convert[second_order_df$f2]
    temp_df <- second_order_df %>%
      filter(f1 != f2) %>%
      mutate(tmp = f1,
             f1 = f2,
             f2 = tmp) %>%
      select(-tmp)
    second_order_df <- rbind(second_order_df, temp_df)
    
    g <- second_order_df %>%
      ggplot(aes(x = f1, y = f2)) +
      geom_tile(aes(fill = var)) +
      labs(x = "Feature 1", y = "Feature 2",
           fill = paste0(label, "   ")) +
      theme_bbs() +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1)) 
    if (percent) {
      g <- g + scale_fill_gradient(low = "#EEEEEE", high = "red", 
                                   label = scales::percent,
                                   breaks = c(0, 0.5, 1))
    } else {
      g <- g + scale_fill_gradient2(low = "red", mid = "#EEEEEE", high = "blue",
                                    midpoint = 0)
    }
    return(g)
  } else {
    return(NULL)
  }
}


fp <- plot_coefs(final, features, order = 1, key = "p", 
                 label = "Posterior Probability Non-Zero", percent = T)
fbeta <- plot_coefs(final, features, order = 1, key = "beta",
                    label = "Posterior Mean Beta (Standardized)", percent = F)

first_order_graphs <- (fp + fbeta) +
  plot_annotation(title = "Linear Coefficient Values") &
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "#E4DBD7", color = NA))
first_order_graphs


sp <- plot_coefs(final, features, order = 2, key = "p",
                 label = "Posterior Probability Non-Zero", percent = T) +
  theme(legend.title = element_text(face = "plain"))
sbeta <- plot_coefs(final, features, order = 2, key = "beta",
                    label = "Posterior Mean Beta (Standardized)", percent = F) +
  theme(legend.title = element_text(face = "plain"))

second_order_graphs <- (sp + sbeta) +
  plot_annotation(title = "Second Order Coefficient Values") &
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "#E4DBD7", color = NA))
second_order_graphs


s2_beta_1 <- plot_coefs(final, features, order = 1, key = "sigma2_beta",
                        label = "Posterior Mean of Variance Coef (Standardized)", percent = F)
s2_beta_2 <- plot_coefs(final, features, order = 2, key = "sigma2_beta",
                        label = "Posterior Mean of Variance Coef (Standardized)", percent = F) +
  theme(legend.title = element_text(face = "plain"))
variance_beta_graphs <- (s2_beta_1 + s2_beta_2) +
  plot_annotation(title = "Variance Coefficient Values") &
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.background = element_rect(fill = "#E4DBD7", color = NA))
variance_beta_graphs


# The Average Aging Curve
begin <- 20
end <- 35
N <- end - begin + 1
avg_df <- data.frame(age = c(begin:end))
for (feat in features) {
  if (feat != "age") {
    avg_df[[feat]] <- rep(zInfo$mean[feat], N)
  }
}
avg_df <- formatMatrix(avg_df, features, response = NULL, withResponse = F)
avg_df <- as.matrix(standardize(avg_df, zInfo$mean, zInfo$std))
res <- avg_df %*% final$beta
res2 <- cumsum(res)

avg_aging_curve <- ggplot(mapping = aes(x = c(begin:end), y = res2)) +
  geom_point() +
  labs(x = "Age",
       y = "WAR/82 (Relative to Start)",
       title = "Average Aging Curve") +
  geom_vline(xintercept = c(begin:end)[which(res2 == max(res2))], 
             linetype = 'dashed') +
  geom_hline(yintercept = 0) +
  theme_bbs()
avg_aging_curve





######## SIMULATION ########

head(advanced)
head(player_df)
head(draft)

# Some helper functions kept here bc they rely on access to global dataframes

get_initial_values <- function(playerId, year, wt_var, alpha) {
  player_row <- advanced %>%
    filter(player_id == playerId, season == year)
  player_all <- advanced %>%
    filter(player_id == playerId, season <= year)
  
  lag_raw_change <- player_df %>% 
    filter(player_id == playerId, season == year) %>%
    pull(raw_change)
  if (length(lag_raw_change) == 0) {
    lag_raw_change <- 0
  }
  
  temp <- player_df %>%
    filter(player_id == playerId, season <= year) %>%
    mutate(weight = !!sym(wt_var),
           lag_weight = !!sym(paste0("lag_", wt_var)),
           yrs_ago = year - season,
           change_wt = (alpha^yrs_ago) * sqrt(weight * lag_weight),
           adj_raw_change = raw_change * change_wt)
  if (nrow(temp) == 0) {
    change_sum <- 0
    change_wt <- 0
  } else {
    change_sum <- sum(temp$adj_raw_change)
    change_wt <- sum(temp$change_wt)
  }
  
  pick <- draft %>%
    filter(player_id == playerId) %>%
    pull(pick_overall) 
  if (length(pick) == 0) {
    log_pick <- log(80) 
  } else {
    log_pick <- log(pick)
  }
  
  values <- list(
    age = player_row$age + 1,
    career_stat = weighted.mean(player_all$WAR_82, player_all[[wt_var]]),
    career_wt = sum(player_all[[wt_var]]),
    change_sum = change_sum,
    change_wt = change_wt,
    lag_stat = player_row$WAR_82,
    lag_raw_change = lag_raw_change,
    log_pick = log_pick
  )
  return(values)
}
get_initial_values <- memoise(get_initial_values, cache = cachem::cache_mem(max_size = 4 * 1024^4))



get_initial_war <- function(playerId, year, minimum_mp = 1, minimum_g = 1) {
  df <- advanced %>%
    filter(player_id == playerId, season <= year, mp >= minimum_mp, g >= minimum_g)
  return(setNames(df$WAR_82, df$age))
}
get_initial_war <- memoise(get_initial_war, cache = cachem::cache_mem(max_size = 4 * 1024^4))



simulate_progression <- function(player_id, season, n_years, saves, 
                                 zInfo, features, response, futureOnly = F, display = T) {
  initial <- get_initial_values(player_id, season, "g", 0.5)
  current <- list(
    sum_career_stat = initial$career_stat * initial$career_wt,
    wt = initial$career_wt,
    change_sum = initial$change_sum,
    change_wt = initial$change_wt,
    lag_stat = initial$lag_stat,
    lag_raw_change = initial$lag_raw_change,
    age = initial$age
  )
  GAMES_PER_SEASON = 70
  if (futureOnly) {
    war_values <- NULL
  } else {
    war_values <- get_initial_war(player_id, season, minimum_mp = 1, minimum_g = 25)
  }
  for (i in 1:n_years) {
    # generate change
    if (current$change_wt == 0) {
      wlc = 0
    } else {
      wlc = current$change_sum / current$change_wt
    }
    p_df <- data.frame(player_id = player_id, 
                       career_stat = (current$sum_career_stat / current$wt),
                       lag_stat = current$lag_stat,
                       lag_raw_change = current$lag_raw_change,
                       weighted_lag_change = wlc,
                       log_pick = initial$log_pick,
                       age = current$age)
    delta <- predictive_dist(p_df, saves, zInfo, features, response, n = 1)
    # update parameters
    current$lag_stat = current$lag_stat + delta
    current$lag_raw_change = delta
    current$sum_career_stat = current$sum_career_stat + GAMES_PER_SEASON * current$lag_stat
    current$wt = current$wt + GAMES_PER_SEASON
    current$change_sum = 0.5 * current$change_sum + GAMES_PER_SEASON * delta
    current$change_wt = 0.5 * current$change_wt + GAMES_PER_SEASON
    # report and save
    if (display) {
      print(paste0("AGE ", current$age, " SEASON: WAR/82 = ", round(current$lag_stat, 2)))
    }
    war_values <- append(war_values, setNames(current$lag_stat, current$age))
    current$age = current$age + 1
  }
  return(war_values)
}



simulate_career <- function(player_id, season, n_sim, n_years, saves, 
                            zInfo, features, response, best_yrs = 5, futureOnly = F) {
  sims <- NULL
  for (i in 1:n_sim) {
    if (i %% 100 == 0) {
      print(paste0("Completed simulation ", i, "/", n_sim, " (", round(100*i/n_sim), "%)"))
    }
    vals <- simulate_progression(player_id, season, n_years, saves, zInfo, 
                                 features, response, futureOnly, display = F)
    best <- best_stretch(vals, best_yrs)
    sims[i] <- best
  }
  return(sims)
}



plot_career_curve <- function(player_id, season, n_sim, n_years, saves, zInfo, features,
                              response) {
  iteration <- c()
  age <- c()
  war <- c()
  for (i in 1:n_sim) {
    if (i %% 100 == 0) {
      print(paste0("Completed simulation ", i, "/", n_sim, " (", round(100*i/n_sim), "%)"))
    }
    vals <- simulate_progression(player_id, season, n_years, saves, zInfo, 
                                 features, response, futureOnly = F, display = F)
    age <- c(age, names(vals))
    war <- c(war, vals)
    iteration <- c(iteration, rep(i, length(vals)))
  }
  df <- data.frame(iteration = iteration, age = age, war = war)
  df2 <- df %>%
    group_by(age) %>%
    summarize(lower = quantile(war, 0.025),
              upper = quantile(war, 0.975))
  g <- df %>%
    ggplot(aes(x = age, y = war)) +
    geom_boxplot() +
    geom_line(data = df2, mapping = aes(x = age, y = lower), group = 1, linetype = 'dashed', linewidth = 0.5) +
    geom_line(data = df2, mapping = aes(x = age, y = upper), group = 2, linetype = 'dashed', linewidth = 0.5) +
    labs(x = "Age", y = "Simulated WAR/82")
  return(g)
}




team_data <- hoopR::nba_teams()

team_colors <- function(team_df, tm_abbrev) {
  i <- which(team_df$team_abbreviation == tm_abbrev)
  c1 <- paste0("#", team_df[i, "color"])
  c2 <- paste0("#", team_df[i, "alternate_color"])
  return(list(c1, c2))
}


simulation_graphs <- function(player_name, player_id, season, team, 
                              n_sim = 1000, n_years = 10, plots = c("hist", "levels")) {
  colors <- get_team_colors(team_data, team)
  set.seed(42)
  career_war <- simulate_career(player_id, season, n_sim, n_years, saves, zInfo,
                                features, response)
  g1 <- sim_hist(career_war, fill = colors[[1]], color = colors[[2]]) + 
    labs(title = paste(player_name, "Simulated Progression"),
         subtitle = "Using Best 5-Year Stretch of Simulated WAR/82") +
    theme_bbs()
  g2 <- plot_sim_levels(career_war, fill = colors[[1]], color = colors[[2]]) + 
    labs(title = paste(player_name, "Simulated Progression"),
         subtitle = "Using Best 5-Year Stretch of Simulated WAR/82") +
    theme_bbs()
  if (plots == "hist") {
    return(g1)
  } else if (plots == "levels") {
    return(g2)
  } else if ("hist" %in% plots & "levels" %in% plots) {
    return(list(hist = g1, levels = g2))
  } else {
    return()
  }
}


# WAR/82 TIERS 
# 20+: Perennial MVP Candidate
# 15+: All-NBA Level
# 10+: All-Star Level
# 5+: Starter
# 2.5+: Bench
# 0+: Minimal Impact
# <0: Negative Impact


# Generate Plots
simulation_graphs("Victor Wembanyama", "wembavi01", 2024, "SAS", 
                  plots = "levels")
simulation_graphs("Anthony Edwards", "edwaran01", 2024, "MIN", 
                  plots = "levels")




#### SAVE FIGURES ####

# EDA Plots
ggsave(intro_plots, 
       filename = "img/intro_plots.png", height = 6, width = 12)
ggsave(change_by_lag, 
       filename = "img/change_by_lag.png", height = 5, width = 7)
ggsave(change_by_pick, 
       filename = "img/change_by_pick.png", height = 5, width = 7)

# OLS Residual Plots
ggsave(resid_plots, 
       filename = "img/resid_plots.png", height = 6, width = 12)

# Model Performance Comparison
ggsave(performance_plots, 
       filename = "img/performance_plots.png", height = 7, width = 14)

# Mixing
ggsave(sigma2_mixing, 
       filename = "img/sigma2_mixing.png", height = 5, width = 8)

# Coefficient Values
ggsave(first_order_graphs, 
       filename = "img/first_order_graphs.png", height = 6, width = 12)
ggsave(second_order_graphs, 
       filename = "img/second_order_graphs.png", height = 8, width = 16)
ggsave(variance_beta_graphs, 
       filename = "img/variance_beta_graphs.png", height = 8, width = 16)

# Average Aging Curve
ggsave(avg_aging_curve, 
       filename = "img/avg_aging_curve.png", height = 5, width = 6)

# Simulation Plots






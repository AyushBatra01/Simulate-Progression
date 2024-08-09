library(shiny)
library(hoopR)
library(memoise)
library(tidyverse)
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)

source("age_curve_functions.R")


# Load Data
perposs <- read_csv("data/perposs.csv")
advanced <- read_csv("data/advanced.csv")
draft <- read_csv("data/draft.csv")

perposs <- perposs %>%
  relocate(all_of(c("player_id", "season")), .after = player)
advanced <- advanced %>%
  relocate(all_of(c("player_id", "season")), .after = player)

draft <- draft %>%
  select(player_id, draft_season = season, pick_overall)

advanced <- advanced %>%
  mutate(WAR_82 = 82 * 2.70 * vorp / g)

player_df <- collect_stats(advanced, "WAR_82", minimum_mp = 1, minimum_g = 25, wt_var = "g")
player_df <- add_career_progression(player_df, alpha = 0.5, wt_var = "g")
player_df <- player_df %>% mutate(log_pick = log(pick_overall))


temp <- advanced %>%
  distinct(player_id, player) %>%
  mutate(player = gsub("\\*", "", player))
ID_TO_NAME <- setNames(temp$player, temp$player_id)

advanced <- advanced %>%
  mutate(team_id = case_when(
    team_id == "BRK" ~ "BKN",
    team_id == "CHO" ~ "CHA",
    team_id == "PHO" ~ "PHX",
    .default = team_id
  ))



# Processing
set.seed(123)
split <- trainTestSplit(player_df)
player_train <- split$train

features <- c("career_stat", "lag_stat", "lag_raw_change", 
              "weighted_lag_change", "log_pick", "age")
response <- "raw_change"
Xy_train <- formatMatrix(player_train, features, response, secondOrder = TRUE)
zInfo <- storeZInfo(Xy_train)



# Get Model Data
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

final <- list(
  p = setNames(colMeans(saves$p), colnames(X_train)),
  beta = setNames(colMeans(saves$beta), colnames(X_train)),
  var_beta = var(saves$beta),
  sigma2 = mean(saves$sigma2),
  sigma2_p = setNames(colMeans(saves$sigma2_p), colnames(X_train)),
  sigma2_beta = setNames(colMeans(saves$sigma2_beta), colnames(X_train)),
  sigma2_var_beta = var(saves$sigma2_beta)
)



# Simulation Functions
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
  withProgress(message = "Simulating...", value = 0, {
    for (i in 1:n_sim) {
      if (i %% 100 == 0) {
        print(paste0("Completed simulation ", i, "/", n_sim, " (", round(100*i/n_sim), "%)"))
      }
      vals <- simulate_progression(player_id, season, n_years, saves, zInfo, 
                                   features, response, futureOnly, display = F)
      best <- best_stretch(vals, best_yrs)
      sims[i] <- best
      incProgress(1 / n_sim, detail = paste0(i, "/", n_sim))
    }
  })
  return(sims)
}



simulate_career2 <- function(player_id, season, n_sim, n_years, saves, 
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





team_data <- hoopR::nba_teams()

get_team_colors <- function(team_df, tm_abbrev) {
  i <- which(team_df$team_abbreviation == tm_abbrev)
  if (length(i) == 0) {
    return(list("gray", "black"))
  }
  c1 <- paste0("#", team_df[i, "color"])
  c2 <- paste0("#", team_df[i, "alternate_color"])
  return(list(c1, c2))
}


yearFilter <- function(yr) {
  options <- advanced %>%
    filter(season == yr, g >= 25) %>%
    mutate(player = gsub("\\*", "", player)) %>%
    arrange(player_id)
  def <- options %>%
    filter(age < 25) %>%
    arrange(-WAR_82) %>%
    pull(player_id)
  def <- def[1]
  return(list(options = setNames(options$player_id, options$player),
              default = def))
}




ui <- fluidPage(
  title = "Player Development Simulation",
  titlePanel("Player Development Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Year:",
                  choices = 1990:2024,
                  selected = 2024),
      selectInput("player", "Player:",
                  choices = yearFilter(2024)[[1]],
                  selected = yearFilter(2024)[[2]]),
      selectInput("n_sim", "Number Simulations:",
                  choices = c(100, 200, 500, 1000, 5000),
                  selected = 100),
      selectInput("n_yr", "Number Yrs to Sim:",
                  choices = c(5, 10, 15),
                  selected = 10),
      checkboxInput("future", "Future Only?",
                    value = TRUE),
      actionButton("go", "Simulate")
    ),
    mainPanel(
      plotOutput("hist", width = "80%"),
      plotOutput("levels", width = "80%")
    )
  )
)



server <- function(input, output, session) {
  year <- reactiveVal(2024)
  player <- reactiveVal("doncilu01")
  nsim <- reactiveVal(100)
  nyr <- reactiveVal(10)
  fut <- reactiveVal(TRUE)
  team <- reactiveVal("DAL")
  cwar <- reactiveVal(NULL)
  
  observeEvent(input$year, {
    res <- yearFilter(input$year)
    if (input$player %in% res$options) {
      def <- input$player
    } else {
      def <- res$default
    }
    updateSelectInput(session, "player", 
                      choices = res$options,
                      selected = def)
  })
  
  observeEvent(input$go, {
    year(as.numeric(input$year))
    player(input$player)
    nsim(as.numeric(input$n_sim))
    nyr(as.numeric(input$n_yr))
    fut(input$future)
    
    new_tm <- advanced %>%
      filter(player_id == player(),
             season == year()) %>%
      pull(team_id)
    
    team(new_tm)
  })
  
  output$hist <- renderPlot({
    colors <- get_team_colors(team_data, team())
    set.seed(42)
    career_war <- simulate_career(player(), year(), nsim(), nyr(), saves, zInfo,
                                  features, response, futureOnly = fut())
    cwar(career_war)
    player_name <- ID_TO_NAME[player()]
    g1 <- sim_hist(cwar(), fill = colors[[1]], color = colors[[2]]) + 
      labs(title = paste(player_name, "Simulated Progression"),
           subtitle = "Using Best 5-Year Stretch of Simulated WAR/82")
    g1
  })
  
  output$levels <- renderPlot({
    colors <- get_team_colors(team_data, team())
    player_name <- ID_TO_NAME[player()]
    g2 <- plot_sim_levels(cwar(), fill = colors[[1]], color = colors[[2]]) + 
      labs(title = paste(player_name, "Simulated Progression"),
           subtitle = "Using Best 5-Year Stretch of Simulated WAR/82")
    g2
  })
}

shinyApp(ui = ui, server = server)






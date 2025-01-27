See Shiny app here: https://ayushbatra.shinyapps.io/simulate_progression/

See detailed writeup: https://bestballstats.com/2024/08/11/mapping-nba-futures-simulating-career-trajectories-with-war-projections/



**Files**

`age_curve.R`: contains main modeling code

`age_curve_functions.R`: contains functions used in `age_curve.R`

`app.R`: defines Shiny application to simulate a player's career (URL: https://ayushbatra.shinyapps.io/simulate_progression/)

`stat_scraping.ipynb`: python code used to webscrape advanced data and draft data from Basketball Reference

**Directories**

`data`: contains data for creating models

`saved`: contains a saved version of the Bayesian model used to predict year to year changes in WAR (Wins above replacement) per 82 games

`img`: contains images used in website article

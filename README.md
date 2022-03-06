NCAA Simulation
================
Andrew Couch
3/6/2022

# 2022 NCAA March Madness Simulation

Written in R and Developed by [Andrew
Couch](https://www.linkedin.com/in/andrew-couch/)

## Description

A short simulation was created for an entry to my works’ march madness
bracket. The simulation uses ELO scores that were created from the most
recent basketball season. I decided to simulate the march madness
bracket since I do not know anything about college sports and
basketball. The simulation uses a Monte Carlo simulation with the ELO
scores that update throughout the bracket.

## Code for the simulation

``` r
library(tidyverse)
library(progress)
library(elo)

# Read in NCAA basketball season history 
df <- tibble(files = list.files(path = "Data")) %>% 
  mutate(files = paste0("Data/", files),
         data = map(files, read_csv, show_col_types = FALSE)) %>% 
  unnest(data)

# Clean team names 
df <- df %>% 
  select(date, game_id, files, opponent, team_score, opp_score) %>% 
  distinct() %>% 
  mutate(files = str_replace(files, "_schedule.csv", ""),
         files = str_replace(files, "Data\\/", "")) %>% 
  distinct() %>% 
  rename(team = files, opp = opponent) %>% 
  mutate(team = str_replace_all(team, "_|-", " "),
         team = str_replace(team, "St", "State") %>% str_replace("Stateate", "State"),
         opp = str_replace_all(opp, "_|-", " "),
         opp = str_replace(opp, "St", "State") %>% str_replace("Stateate", "State"))
```

``` r
# Randomly sample from each game (pairwise head-to-head ex. A vs. B and B vs. A)
match_df <- df %>% 
  group_by(game_id) %>% 
  slice_sample(n = 1) %>% 
  ungroup() %>% 
  arrange(date)

# Compute ending elo for the season to use in simulation 
start_elo <- elo.run(score(team_score, opp_score) ~ team + opp, data = match_df, k = 20) %>% 
  final.elos() %>% 
  as_tibble(rownames = "team") %>% 
  rename(elo = value)
```

``` r
# Create dataframes for each division for teams in March Madness bracket
# Bracket used: https://www.espn.com/espn/feature/story/_/page/bracketology/ncaa-bracketology-projecting-2022-march-madness-men-field
# Some teams are still playing for bracket and max elo is used to choose teams (will change or not)
south_df <- tibble(div = "south",
                   team = c("Baylor", "Norfolk State", "Marquette", "Miami", "UConn",
                            # Head to head "Rutgers", "Loyola Chicago", 
                            "Loyola Chicago", 
                            "Houston", "Toledo", "Ohio State", "Davidson", "Tennessee", "Texas State", "Boise State", 
                            "San Francisco", "Duke", "Colgate"
                   ))

midwest_df <- tibble(div = "mdiwest",
                     team = c("Arizona", "Long Beach State", "TCU", "Murray State", "Texas", "Chattanooga", "Providence", "Iona", "LSU",
                              "Wyoming", "Purdue", "Northern Iowa", "USC", "San Diego State", "Kansas", "Jacksonville State"))

west_df <- tibble(div = "west",
                  team = c("Gonzaga", 
                           # Head to head "New Orleans", "Alcorn State"
                           "New Orleans",
                           "Iowa State", "Creighton", "Alabama", "South Dakota State", "UCLA", "Towson", "Saint Mary's", "Michigan", "Texas Tech", 
                           "Seattle", "Colorado State", "North Carolina", "Wisconsin", "Montana State"))

east_df <- tibble(div = "east",
                  team = c("Auburn", 
                           # Head to head"Cleveland State", "Bryant", 
                           "Bryant", 
                           "Seton Hall", "Wake Forest", "Arkansas", "North Texas", "Illinois", "Vermont", "Iowa", 
                           # Head to Head? "Xavier", "Memphis", 
                           "Memphis", 
                           "Villanova", "Princeton", "Michigan State", "Notre Dame", "Kentucky", "Longwood"))
```

``` r
# Define a function for simulating a tournament 
simulate_tournament <- function(trials){
  # Create a helper function to simulate each division winners 
  simulate_div <- function(df){
    # Simulate round 1 
    round1 <- df %>% 
      # Join starting elo 
      inner_join(start_elo, by = "team") %>% 
      # Start First round 
      mutate(round = rep(c(1, 0), 8),
             round = cumsum(round),
             team_type = rep(c("team", "opp"), 8)) %>% 
      # Create head to head 
      pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
      # Compute elo probability 
      mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
             # Simulate outcome
             outcome = runif(n = n()),
             outcome = if_else(outcome <= team_prob, 1, 0),
             # Compute elo change 
             elo_team = elo_team + 20*(outcome - team_prob), 
             elo_opp = elo_opp - 20*(outcome - team_prob),
             # Find the winning team and new elo
             team = if_else(outcome == 1, team_team, team_opp),
             elo = if_else(outcome == 1, 
                           elo_team + 20*(outcome - team_prob), 
                           elo_opp - 20*(outcome - team_prob)))
    
    # Simulate round 2 
    round2 <- round1 %>% 
      select(div, round, team, elo) %>% 
      # Start second round 
      mutate(round = rep(c(1, 0), 4),
             round = cumsum(round),
             team_type = rep(c("team", "opp"), 4)) %>% 
      pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
      mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
             outcome = runif(n = n()),
             outcome = if_else(outcome <= team_prob, 1, 0),
             elo_team = elo_team + 20*(outcome - team_prob), 
             elo_opp = elo_opp - 20*(outcome - team_prob),
             team = if_else(outcome == 1, team_team, team_opp),
             elo = if_else(outcome == 1, 
                           elo_team + 20*(outcome - team_prob), 
                           elo_opp - 20*(outcome - team_prob)))
    
    # Simulate round three 
    round3 <- round2 %>% 
      select(div, round, team, elo) %>% 
      mutate(round = rep(c(1, 0), 2),
             round = cumsum(round),
             team_type = rep(c("team", "opp"), 2)) %>% 
      pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
      mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
             outcome = runif(n = n()),
             outcome = if_else(outcome <= team_prob, 1, 0),
             elo_team = elo_team + 20*(outcome - team_prob), 
             elo_opp = elo_opp - 20*(outcome - team_prob), 
             team = if_else(outcome == 1, team_team, team_opp),
             elo = if_else(outcome == 1, 
                           elo_team + 20*(outcome - team_prob), 
                           elo_opp - 20*(outcome - team_prob))) 
    # Simulate round four 
    round4 <- round3 %>% 
      select(div, round, team, elo) %>% 
      mutate(round = rep(c(1, 0), 1),
             round = cumsum(round),
             team_type = rep(c("team", "opp"), 1)) %>% 
      pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
      mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
             outcome = runif(n = n()),
             outcome = if_else(outcome <= team_prob, 1, 0),
             elo_team =elo_team + 20*(outcome - team_prob), 
             elo_opp = elo_opp - 20*(outcome - team_prob),
             team = if_else(outcome == 1, team_team, team_opp),
             elo = if_else(outcome == 1, 
                           elo_team + 20*(outcome - team_prob), 
                           elo_opp - 20*(outcome - team_prob)))
    
    # Combine each round's results 
    div_hist_df <- bind_rows(
      round1 %>% 
        select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
        mutate(round = 1),
      round2 %>% 
        select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
        mutate(round = 2),
      round3 %>% 
        select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
        mutate(round = 3),
      round4 %>% 
        select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
        mutate(round = 4)
    )
    
    # Combine simulated history of division with the winner of the division 
    nest(round4 %>% select(team, elo), data = everything()) %>% rename(winner = data) %>% 
      bind_cols(nest(div_hist_df, data = everything())) %>% rename(history = data)
  }
  
  
  pb$tick()
  Sys.sleep(1/100)
  
  # Simulate each divisions results and winner
  df <- tibble(data = list(west_df, east_df, midwest_df, south_df)) %>% 
    mutate(res = map(data, simulate_div)) %>% 
    select(res) %>% 
    unnest(cols = c(res)) 
  
  # Simulate the semi finals 
  # could use helper function for more tidier version but would need to create functions to aggregate results of winners 
  # resulting in a lot of nested data frames and slowness in simulations (planning on simulating 10,000 times)
  semi_final <- df %>% 
    select(winner) %>% 
    unnest(cols = c(winner)) %>% 
    mutate(round = rep(c(1, 0), 2),
           round = cumsum(round),
           team_type = rep(c("team", "opp"), 2)) %>% 
    pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
    mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
           outcome = runif(n = n()),
           elo_team = elo_team + 20*(outcome - team_prob), 
           elo_opp = elo_opp - 20*(outcome - team_prob),
           outcome = if_else(outcome <= team_prob, 1, 0),
           team = if_else(outcome == 1, team_team, team_opp),
           elo = if_else(outcome == 1, 
                         elo_team + 20*(outcome - team_prob), 
                         elo_opp - 20*(outcome - team_prob))) 
  
  # Create holder for semi final history 
  hist <- semi_final %>%   
    select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
    mutate(round = 5) %>% 
    nest(data = everything()) %>% 
    rename(history = data)
  
  # Create holder for semi final winners 
  winner <- semi_final %>% 
    select(team, elo) %>% 
    nest(data = everything()) %>% 
    rename(winner = data)
  
  # Append simulation history and winner division history and winners 
  df <- bind_rows(df, bind_cols(hist, winner))
  
  # Simulate the finals 
  final <- winner %>% 
    unnest(cols = c(winner)) %>% 
    mutate(round = rep(c(1, 0), 1),
           round = cumsum(round),
           team_type = rep(c("team", "opp"), 1)) %>% 
    pivot_wider(names_from = team_type, values_from = c(team, elo)) %>% 
    mutate(team_prob = 1 / (1 +  10^((elo_opp - elo_team)/ 400)),
           outcome = runif(n = n()),
           outcome = if_else(outcome <= team_prob, 1, 0),
           elo_team = elo_team + 20*(outcome - team_prob), 
           elo_opp = elo_opp - 20*(outcome - team_prob),
           team = if_else(outcome == 1, team_team, team_opp),
           elo = if_else(outcome == 1, 
                         elo_team + 20*(outcome - team_prob), 
                         elo_opp - 20*(outcome - team_prob)))
  
  # Create holder for final history
  hist <- final %>%   
    select(team = team_team, elo_team, opp = team_opp, elo_opp, outcome) %>% 
    mutate(round = 6) %>% 
    nest(data = everything()) %>% 
    rename(history = data)
  
  # Create holder for final winners 
  winner <- final %>% 
    select(team, elo) %>% 
    nest(data = everything()) %>% 
    rename(winner = data) 
  
  # Append simulation history and winner to division and semi final history and winners 
  df <- bind_rows(df, bind_cols(hist, winner))
  
  # Convert/Aggregate the nested dataframes to two nested data frames (history and winners)
  # May have a function that would make it easier
  bind_cols(
    df %>%
      select(history) %>%
      unnest(cols = c(history)) %>%
      nest(data = everything()) %>%
      rename(history = data),
    df %>%
      select(winner) %>%
      unnest(cols = c(winner)) %>%
      nest(data = everything()) %>%
      rename(winner = data)
  )
}

pb <- progress_bar$new(total = 10000, format = "[:bar] :current/:total (:percent)")

start <- Sys.time()
res <- tibble(trials = seq.int(1, 10000)) %>% mutate(sim = map(trials, simulate_tournament))
end <- Sys.time()

write_rds(res, file = "ncaa_sim.RDS")
```

``` r
# Read in simulation 
res <- read_rds("ncaa_sim.RDS")

res_df <- res %>% 
  unnest(sim) %>% 
  select(trials, history) %>% 
  unnest(history) 
```

``` r
# Get top 10 teams who win the simulated finals 
bind_rows(
  res_df %>% 
    select(trials, round, team, outcome),
  res_df %>% 
    select(trials, round, team = opp, outcome) %>% 
    mutate(outcome = if_else(outcome == 1, 0, 1))
) %>% 
  filter(outcome == 1 & round == 6) %>% 
  group_by(team) %>% 
  summarise(total = sum(outcome)) %>% 
  ungroup() %>% 
  slice_max(total, n = 10) %>% 
  select(team, total) %>% 
  arrange(desc(total))
```

    ## # A tibble: 10 x 2
    ##    team               total
    ##    <chr>              <dbl>
    ##  1 Murray State         413
    ##  2 Gonzaga              400
    ##  3 Arizona              335
    ##  4 Auburn               302
    ##  5 North Texas          268
    ##  6 Duke                 265
    ##  7 South Dakota State   265
    ##  8 Houston              257
    ##  9 Kansas               256
    ## 10 Providence           230

``` r
# View round probabilities for top 5 teams who make it to the final round
bind_rows(
  res_df %>% 
    select(trials, round, team, outcome),
  res_df %>% 
    select(trials, round, team = opp, outcome) %>% 
    mutate(outcome = if_else(outcome == 1, 0, 1))
) %>% 
  add_count(trials) %>% 
  group_by(team, round, n) %>% 
  summarise(total = sum(outcome)) %>% 
  ungroup() %>% 
  mutate(prob = total / n) %>% 
  select(team, round, prob) %>% 
  filter(team %in% c("Murray State", "Gonzaga", "Arizona", "Auburn", "North Texas")) %>% 
  ggplot(aes(x = round, y = prob, color = team)) + 
  geom_line()
```

    ## `summarise()` has grouped output by 'team', 'round'. You can override using the `.groups` argument.

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# View every team's round probabilities 
bind_rows(
  res_df %>% 
    select(trials, round, team, outcome),
  res_df %>% 
    select(trials, round, team = opp, outcome) %>% 
    mutate(outcome = if_else(outcome == 1, 0, 1))
) %>% 
  group_by(round, team) %>% 
  summarise(freq = sum(outcome)) %>% 
  ungroup() %>% 
  ggplot(aes(x = round, y = freq, group = team)) + 
  geom_line(alpha = 0.1)
```

    ## `summarise()` has grouped output by 'round'. You can override using the `.groups` argument.

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# View most likely teams to make it for each round
bind_rows(
  res_df %>% 
    select(trials, round, team, outcome),
  res_df %>% 
    select(trials, round, team = opp, outcome) %>% 
    mutate(outcome = if_else(outcome == 1, 0, 1))
) %>% 
  group_by(round, team) %>% 
  summarise(wins = sum(outcome)) %>% 
  group_by(round) %>% 
  mutate(rank = dense_rank(desc(wins))) %>% 
  ungroup() %>% 
  filter(rank <= 2^(6 - round)) %>% 
  arrange(round, rank)
```

    ## `summarise()` has grouped output by 'round'. You can override using the `.groups` argument.

    ## # A tibble: 64 x 4
    ##    round team                wins  rank
    ##    <dbl> <chr>              <dbl> <int>
    ##  1     1 Gonzaga             6828     1
    ##  2     1 Murray State        6685     2
    ##  3     1 Arizona             6602     3
    ##  4     1 Colorado State      6379     4
    ##  5     1 Saint Mary's        6189     5
    ##  6     1 South Dakota State  6113     6
    ##  7     1 Auburn              6105     7
    ##  8     1 Duke                6088     8
    ##  9     1 Purdue              6013     9
    ## 10     1 Kansas              5992    10
    ## # ... with 54 more rows

``` r
# Compute head to head probabilities from simulation
res_df %>% 
  mutate(opp_outcome = if_else(outcome == 1, 0, 1),
         team_a = if_else(team > opp, team, opp),
         team_b = if_else(team > opp, opp, team),
         outcome_a = if_else(team > opp, outcome, opp_outcome)) %>% 
  select(trials, round, team_a, team_b, outcome_a) %>% 
  count(round, team_a, team_b, outcome_a) %>% 
  mutate(outcome_a = if_else(outcome_a == 1, "win", "loss")) %>% 
  pivot_wider(names_from = outcome_a, values_from = n, values_fill = 0) %>% 
  mutate(win_prob = win / (win + loss)) 
```

    ## # A tibble: 1,998 x 6
    ##    round team_a           team_b              loss   win win_prob
    ##    <dbl> <chr>            <chr>              <int> <int>    <dbl>
    ##  1     1 Bryant           Auburn              6105  3895    0.390
    ##  2     1 Duke             Colgate             3912  6088    0.609
    ##  3     1 Iowa State       Creighton           4961  5039    0.504
    ##  4     1 Kansas           Jacksonville State  4008  5992    0.599
    ##  5     1 Long Beach State Arizona             6602  3398    0.340
    ##  6     1 Longwood         Kentucky            5236  4764    0.476
    ##  7     1 Memphis          Iowa                5334  4666    0.467
    ##  8     1 Miami            Marquette           5077  4923    0.492
    ##  9     1 New Orleans      Gonzaga             6828  3172    0.317
    ## 10     1 Norfolk State    Baylor              5943  4057    0.406
    ## # ... with 1,988 more rows

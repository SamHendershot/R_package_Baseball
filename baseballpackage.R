library(dplyr)
library(ggplot2)
library(Lahman)

teams10 <- subset(Teams, Teams$yearID > 2010, select = yearID:name)

#' Overall Rank
#'
#' This function ranks a variable by a variable. Usually grouped by team name or franchise and then it prints the list of means for the other variable. For example, x is name and y is number of wins, or number of loses
#'
#' @importFrom dplyr group_by summarise_at %>%
#' @param dataset the data set of interest
#' @param x what the output should be grouped by (i.e. name, id, serial number, etc)
#' @param y the variable we want to rank by (i.e. income, number of success/failures)
#'
#' @return NULL
#' @export
#' @examples overallRank(dataset = teams10, x = "SFN", y = "W")
overallRank <- function(dataset, x, y) {
  groupsRank <- dataset %>% group_by({{x}}) %>% summarise_at(vars({{y}}), list(mean='mean'))
  return(groupsRank)
}

#' Wins by Team
#'
#' @description This function lists out the proportion of wins (number of wins divided by total number of games) in alphabetical order by team. Since there are ten seasons, there’s 10 proportions for each team.
#' @param dataset data set of interest
#' @param wins variable for wins/success
#' @param loses variable for losses/failures
#' @param teamNames variable for name (i.e. team name, team id, franchise id, etc)
#'
#' @return data frame listing out the proportion of wins for all names
#' @export
#' @examples winsByTeam(dataset = teams10, wins = teams10$W, loses = teams10$L, teams10$name)
winsByTeam <- function(dataset, wins, loses, teamNames) {
  winsprop <- as.numeric(c())
  name <- c()
  for(i in 1:nrow(dataset)) {
    value <- as.numeric(wins[i]/(wins[i]+loses[i]))
    winsprop <- c(winsprop, value)
    name <- c(name, teamNames[i])
  }
  return(data.frame(sort(name), as.numeric(winsprop)))
}

#' Loses by team
#'
#' @description This function lists out the proportion of loses (number of loses divided by total number of games) in alphabetical order by team. Since there are ten seasons, there’s 10 proportions for each team.
#'
#' @param dataset dataset of interst
#' @param wins variable for wins/success
#' @param loses variable for losses/failures
#' @param teamNames variable for name (i.e. team name, team id, franchise id, etc)
#'
#' @return dataframe listing out the proportion of loses for all names
#' @export
#' @examples losesByTeam(dataset = teams10, wins = teams10$W, loses = teams10$L, teams10$name)
losesByTeam <- function(dataset, wins, loses, teamNames) {
  losesprop <- as.numeric(c())
  name <- c()
  for(i in 1:nrow(dataset)) {
    value <- (loses[i]/(wins[i]+loses[i]))
    losesprop <- c(losesprop, value)
    name <- c(name, teamNames[i])
  }
  return(data.frame(sort(name), losesprop))
}

#' Rivalry
#'
#' @description This function takes in two team names and the variable of interest (wins, loses, era, etc) and randomly samples from each teams column and based on the pairs we get from this sampling, then calculates the mean of each sample column and the team with higher average is the one that is assumed to be the winner of the rivalry.
#'
#' @param dataset data set/data frame
#' @param team1 string containing the name of the first team
#' @param team2 string containing the name of the second team
#' @param x the variable we want to predict (wins, loses, era, batting average, etc)
#'
#' @return NULL (prints results)
#' @export
#' @examples rivalry(teams, "San Francisco Giants", "Oakland A's", "W")
rivalry <- function(dataset, team1, team2, x) {
  # creates two data frames with the data from the last
  # ten years for each team
  dat <- dataset
  team1info <- dat[dat$name == team1,]
  team2info <- dat[dat$name == team2,]
  # sampling from each data set and creating pairs
  r1 <- (team1info[x])
  r2 <- (team2info[x])
  # creating vectors of just column of interest (wins in this case)
  t1 <- data.frame()
  t2 <- data.frame()
  t1 <- sample((min(r1)):(max(r1)), 1000, replace=TRUE)
  t2 <- sample((min(r1)):(max(r1)), 1000, replace=TRUE)

  results <- data.frame(cbind(t1, t2))

  # prints the number of wins predicted in the season
  print(cbind(c(team1, mean(t1), sd(t1)), c(team2, mean(t2), sd(t2))))
  results$res <- ifelse(results$t1 > results$t2, 1,
                        ifelse(results$t1 < results$t2, 2, 0))

  team1chance = ((nrow(results[results$res == 1, ]))/nrow(results))
  team2chance = ((nrow(results[results$res == 2, ]))/nrow(results))
  drawPercent = ((nrow(results[results$res == 0, ]))/nrow(results))
  print(data.frame(c(team1, team1chance), c(team2, team2chance), c(drawPercent)))
}

#' Batting Percentage
#'
#' @description Calculates hit rates for given batter for each type of at bat outcome (Single, Double, Triple, HR, Walk, Out)
#'
#' @param batter_name Name of batter you wish to see percentage.
#' @param year_batter Year you want the batter's percentage.
#'
#' @return Data frame of batting percentages
#' @export
#' @examples get_batter_percs(batter_name = "adamewi01", year_batter = "2021")
get_batter_percs <- function(batter_name,year_batter){
  bat_df <- data.frame() #create empty data frame

  at_bat <- sum(Batting[Batting$playerID == batter_name & Batting$yearID == year_batter, "AB"]) # select at bats

  bat_types <- c("H", "X2B", "X3B", "HR", "BB","SO")
  for (i in bat_types){
    bat_stat <- sum(Batting[Batting$playerID == batter_name & Batting$yearID == year_batter, i])
    bat_hit_perc <- bat_stat / at_bat
    bat_df <- rbind(bat_df,bat_hit_perc)
  }
  bat_df[6,] <- (bat_df[6,] + (at_bat - sum(bat_df * at_bat)) / at_bat) #formula to update last row to out rate since batting table only counts strike out rate

  colnames(bat_df) <- "Batter's Hit Rates"

  return(bat_df)
}

#' All Batting Percentages
#'
#' @description  Calculates league average for given year for each type of at bat outcome (Single, Double, Triple, HR, Walk, Out)
#'
#' @param year Year you want too see all player batting averages.
#'
#' @return Data frame of all batting averages for selected year.
#' @export
#' @examples get_all_batter_percs(year = "2021")
get_all_batter_percs <- function(year){
  bat_league_df <- data.frame()

  at_bat_league <- sum(Batting[Batting$yearID == year, "AB"])

  bat_types <- c("H", "X2B", "X3B", "HR","BB", "SO")
  for (i in bat_types){
    bat_total <- sum(Batting[Batting$yearID == year, i])
    bat_total_perc <- sum(bat_total) / at_bat_league
    bat_league_df <- rbind(bat_league_df,bat_total_perc)
  }

  bat_league_df[6,] <- (bat_league_df[6,] + (at_bat_league - sum(bat_league_df * at_bat_league)) / at_bat_league)

  colnames(bat_league_df) <- "League Average for Year"
  return(bat_league_df)
}

#' Combine Batter Matrix
#'
#' @description Calls the functions get_batter_percs and get_all_batter_percs and combines them into a single dataframe.
#'
#' @param batter_matrix Matrix of batters you wish to combine.
#' @param year_matrix Matrix of years you wish to combine.
#'
#' @return Data frame of combined matrix
#' @export
#' @examples combine_batter_matrix(batter_matrix = c("adamewi01","alcansa01"),"2021")
combine_batter_matrix <- function(batter_matrix,year_matrix){
  bat_pitch_list <- list()

  for (i in 1:length(batter_matrix)){
    mat <- get_batter_percs(batter_name = batter_matrix[i], year_batter = year_matrix)
    bat_pitch_list[[i]] <- mat
    colnames( bat_pitch_list[[i]]) <- paste("Batter id:",batter_matrix[i])
  }

  league_avg_df <- get_all_batter_percs(year = year_matrix)
  bat_pitch_df = do.call(cbind,c(bat_pitch_list,league_avg_df))

  row.names(bat_pitch_df) <- c("Single","Double","Triple","HR","Walk","Strike Out")

  return(bat_pitch_df)
}


#' Batter Run Monte Carlo
#'
#' @description Calls the function combine_batter_matrix and passes the arguments batters and year into it where batters can be a vector of any number of player ids from the batting table. Performs a monte carlo which randomly samples from each player’s hit probabilities generated from function combine_batter_matrix and returns each batter’s expected number of runs as well as the expected number of runs for the league average for the given year.
#'
#' @param batters Batters you wish to simulate.
#' @param year Year you want to simulate on.
#' @param trials Number of Monte Carlo iterations to simulate
#'
#' @return Expected number of runs for the batter in the given year.
#' @export
#' @examples MC_batter_runs(batters = c("adamewi01","alcansa01"), year = "2021", trials = 1e4)
MC_batter_runs <- function(batters,year,trials){

  batter_probs_matrix <- combine_batter_matrix(batter_matrix = batters, year_matrix = year)

  MC_batter_run_mat <<- matrix(data = NA, nrow = trials, ncol = length(batter_probs_matrix)) #creates global variable which stores runs for each trial
  colnames(MC_batter_run_mat) <<- c(batters,"League Average")

  for (i in 1:length(batter_probs_matrix)){
    count <- 1
    total_runs <- 0
    runs <- 0
    while (count <= trials){
      bat_outcome <- sample(c(1:6), 1, prob = batter_probs_matrix[ ,i])

      if (bat_outcome == 1|bat_outcome == 5){ runs <- 1
      total_runs <- total_runs + runs}
      if (bat_outcome == 2) { runs <- 2
      total_runs <- total_runs + runs}
      if (bat_outcome == 3) { runs <- 3
      total_runs <- total_runs + runs}
      if (bat_outcome == 4) { runs <- 4
      total_runs <- total_runs + runs}
      if (bat_outcome == 6) { runs <- 0
      total_runs <- total_runs + runs}

      MC_batter_run_mat[count,i] <<- runs
      count <- count + 1
    }
  }
  return(colSums(MC_batter_run_mat)/trials)

}


#' Batter Run Monte Carlo Histogram
#'
#' @description Creates a histogram for each of the runs simulated in the function  MC_batter_runs . Uses the global variable MC_batter_run_mat created by running the previous function.
#'
#' @return No return, creates histogram
#' @export
MC_batter_runs_histo <- function(){

  MC_batter_run_df <- data.frame(MC_batter_run_mat)
  par(mfrow=c(1,ncol(MC_batter_run_df ))) # Create two rows and two columns

  par(bg="gray")

  for (i in 1:ncol(MC_batter_run_df)){
    hist(MC_batter_run_df[[i]], main=paste("Batter",i), xlab = "runs", xlim = c(0,4), ylim = c(0,1e4))
  }
}

#' Filter Player Salary by Year
#'
#' @description Allows user to filter salaries by player for how ever many years they want.
#'
#' @importFrom dplyr group_by %>%
#' @importFrom dplyr select summarize
#' @importFrom dplyr filter inner_join
#' @param startYear Year to begin filter.
#' @param endYear Year to end filter at.
#' @param numAtBats Minimum number of at bats required.
#' @param gamesPitched How many games pitched by player, helps filter if player is pitcher or not.
#'
#' @return Data frame with 2 columns of salaries by player
#' @export
filterSalaryYear <- function(startYear, endYear, numAtBats, gamesPitched){

  # Filters players by number of pitches thrown in career
  pitch <- Appearances %>% filter(yearID > startYear & yearID < endYear) %>%
    group_by(playerID) %>% summarize(G_p = sum(G_p)) %>%
    filter(G_p == gamesPitched) %>% select(playerID)

  # Filters players by number of at bats throughout career
  atBat <- Batting %>% filter(yearID > startYear & yearID < endYear) %>%   group_by(playerID) %>% summarize(AB = sum(AB)) %>%
    filter(AB >= numAtBats) %>% select(playerID)

  # Joins the two sets into one by playerID
  p <- inner_join(pitch, atBat, by = "playerID")

  salaries_filtered <- Salaries %>% filter(yearID > startYear & yearID < endYear) %>% group_by(playerID) %>% summarize(salary = sum(salary, na.rm = TRUE)) %>% inner_join(p, by = "playerID") %>% select(playerID, salary)

  return(salaries_filtered)
}

#' Visualize Filtered Salary
#'
#' @description Allows user to visualize their filtered salary dataset through a histogram.
#'
#' @importFrom dplyr %>%
#' @importFrom ggplot2 ggplot geom_histogram
#' @importFrom ggplot2 aes
#' @param filtered_salaries Salary data frame found using filterSalaryYear() function.
#'
#' @return Histogram plot
#' @export
plotSalary <- function(filtered_salaries){
  filtered_salaries %>% ggplot() + geom_histogram(mapping = aes(x = salary))
}

#' Specific Player Salary
#'
#' @description Helps user find a specific players salary in the filtered salary table found using filterSalaryYear() function.
#'
#' @param firstname First name of baseball player.
#' @param lastname Last name of baseball player.
#' @param salary_table Salary data frame found using filterSalaryYear() function.
#'
#' @return Player salary
#' @export
playerSalary <- function(firstname, lastname, salary_table){
  players <- playerInfo(tolower(lastname))
  player <- players[players$nameFirst == firstname,]
  id <- player$playerID
  player_Salary <- salary_table %>% filter(playerID == id)
  if(nrow(player_Salary) == 0){
    return("Player not in filtered Salary set.")
  } else{
    return(player_Salary['salary'])
  }
}



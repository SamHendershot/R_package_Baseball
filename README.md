# R_package_Baseball
R package that uses Lahman dataset to analyze numerous components about baseball. This was a group project with every team member collaborating to create the project. 

# Functions
There are 12 functions in total. Below are all of them and a description of what they accomplish.

overallRank - This function ranks a variable by a variable. Usually grouped by team name or franchise and then it prints the list of means for the other variable.

winsByTeam - This function lists out the proportion of wins (number of wins divided by total number of games) in alphabetical order by team.

losesByTeam - This function lists out the proportion of loses (number of loses divided by total number of games) in alphabetical order by team.

rivalry - This function takes in two team names and the variable of interest (wins, loses, era, etc) and randomly samples from each teams column and based on the pairs we get from this sampling, then calculates the mean of each sample column and the team with higher average is the one that is assumed to be the winner of the rivalry.

get_batter_percs - Calculates hit rates for given batter for each type of at bat outcome (Single, Double, Triple, HR, Walk, Out).

get_all_batter_percs - Calculates league average for given year for each type of at bat outcome (Single, Double, Triple, HR, Walk, Out).

combine_batter_matrix - Calls the functions get_batter_percs and get_all_batter_percs and combines them into a single dataframe.

MC_batter_runs - Calls the function combine_batter_matrix and passes the arguments batters and year into it where batters can be a vector of any number of player ids from the batting table. Performs a monte carlo which randomly samples from each player’s hit probabilities generated from function combine_batter_matrix and returns each batter’s expected number of runs as well as the expected number of runs for the league average for the given year.

MC_batter_runs_histo - Creates a histogram for each of the runs simulated in the function  MC_batter_runs . Uses the global variable MC_batter_run_mat created by running the previous function.

filterSalaryYear - Allows user to filter salaries by player for how ever many years they want.

plotSalaryYear - Allows user to visualize their filtered salary dataset through a histogram.

playerSalary - Helps user find a specific players salary in the filtered salary table found using filterSalaryYear() function.


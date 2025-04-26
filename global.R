library(httr)
library(jsonlite)
library(dplyr)
library(data.table)
library(writexl)
library(DT)
library(lubridate)
library(ggplot2)
library(ggimage)
library(plotly)
library(ggrepel)

# Safe API call with retries
safe_api_call <- function(url, max_retries = 5, delay = 2, timeout_sec = 15) {
  attempt <- 1
  while (attempt <= max_retries) {
    response <- tryCatch({
      GET(url, timeout(timeout_sec))
    }, error = function(e) NULL)

    if (!is.null(response) && status_code(response) == 200) {
      return(fromJSON(content(response, "text", encoding = "UTF-8")))
    } else {
      warning(sprintf("Attempt %d failed for URL: %s", attempt, url))
      Sys.sleep(delay * attempt)
      attempt <- attempt + 1
    }
  }
  warning(sprintf("Max retries reached for URL: %s", url))
  return(NULL)
}

teams_in <- function(){
  url <- "https://api-web.nhle.com/v1/standings/now"
  resp <- GET(url)
  data <- content(resp, as = "text")
  data <- fromJSON(data)
  
  In <- data$standings$clinchIndicator
  Team <- data$standings$teamAbbrev
  
  teams_in <- data.table(
    Team = Team,
    In = In
  )
  teams_in <- teams_in[teams_in$In != 'e',]
  teams_in <- teams_in$Team.default
  return(teams_in)
}

# playoff_points <- safe_api_call(paste0('https://api-web.nhle.com/v1/skater-stats-leaders/20242025/3?categories=points&limit=1000'))$points
# playoff_goals <- safe_api_call(paste0('https://api-web.nhle.com/v1/skater-stats-leaders/20242025/3?categories=goals&limit=1000'))$goals
# playoff_assists <- safe_api_call(paste0('https://api-web.nhle.com/v1/skater-stats-leaders/20242025/3?categories=assists&limit=1000'))$assists

library(readxl)
source <- readxl::read_xlsx('2025_player_stats.xlsx', sheet = 'Lookup')
# in the source table, if the name has a comma and no space after it, then add the space, otherwise don't
source$name <- gsub(",([^ ])", ", \\1", source$name)
picks <- readxl::read_xlsx('2025_player_stats.xlsx', sheet = 'Picksheet')
#transpose the picks dataframe so that the names are in the first column, and then rounds are on the top, drop the second row and each column that has 'Team' in it
picks <- t(picks)
picks <- as.data.frame(picks)
# move the first row to the names of the columns
names(picks) <- picks[1, ]
names(picks)[1] <- 'Team'

#drop each row where the team column == 'Team' or ''
#drop the first row
picks <- picks[-1, ]
#drop each ROW that has 'Team' in the 'team' column
picks <- picks[picks$Team != 'Team', ]
#drop teh Team column
picks$Team <- NULL
# Drop any row where the rowname is like 'Player'
picks <- picks[!grepl('Player', rownames(picks)), ]
# Drop any row where the rowname is like NA
picks <- picks[!grepl('NA', rownames(picks)), ]
#Replace Picks$team for Luca/Ryan to LucaRyan
picks <- picks %>% mutate_all(~ gsub("Luca/Ryan", "LucaRyan", .))

picks <- as.data.table(picks, keep.rownames = "Team")  # move rownames to a column
# Pivot the picks table into long format

long_picks <- melt(picks, id.vars = "Team", 
                variable.name = "Round", 
                value.name = "Value")

# rename Value column player_name
long_picks <- long_picks %>% rename(player_name = Value)

for (i in 1:nrow(long_picks)) {
  # Look up the player name in the source table and get the player id
  if (long_picks$player_name[i] == 'landeskog') {
    player_id <- 8476455
    team <- 'COL'
  } else {
    player_id <- source[source$name == long_picks$player_name[i], 'id'][[1]]
    team <- source[source$name == long_picks$player_name[i], 'short_team'][[1]]
  }
  player_name <- long_picks$player_name[i]
  # print(player_id)
  
  # player_goals <- ifelse(length(ifelse(is.na(playoff_goals[playoff_goals$id == player_id, 'value'] == 0), 0, playoff_goals[playoff_goals$id == player_id, 'value'])) == 0, 0, playoff_goals[playoff_goals$id == player_id, 'value'])
  # player_assists <- ifelse(length(ifelse(is.na(playoff_assists[playoff_assists$id == player_id, 'value'] == 0), 0, playoff_assists[playoff_assists$id == player_id, 'value'])) == 0, 0, playoff_assists[playoff_assists$id == player_id, 'value'])
  # player_points <- ifelse(length(ifelse(is.na(playoff_points[playoff_points$id == player_id, 'value'] == 0), 0, playoff_points[playoff_points$id == player_id, 'value'])) == 0, 0, playoff_points[playoff_points$id == player_id, 'value'])
  
  long_picks$id[i] <- player_id
  long_picks$team[i] <- team
  # long_picks$goals[i] <- player_goals
  # long_picks$assists[i] <- player_assists
  # long_picks$points[i] <- player_points
  
  }

  





  # # Get current time in MST
  # mst_time <- with_tz(Sys.time(), tzone = "America/Edmonton")
  # 
  # # Get date as of MST
  # mst_date <- as.Date(mst_time)

  playoff_start <- as.Date('2025-04-19')
  playoff_dates <- seq.Date(playoff_start, as.Date(format(Sys.time(), tz = "America/Edmonton", usetz = TRUE)), by = "day")

  teams_tonight <- c()
  
  for (i in 1:length(playoff_dates)) {
    playoff_date <- playoff_dates[i]
    # Construct the API URL
    url <- paste0('https://api-web.nhle.com/v1/score/', playoff_date)
    # Make the GET request
    response <- GET(url)
    # Parse the JSON content
    data <- content(response, as = "parsed", type = "application/json")
    games <- data$games
    date_assists <- c()
    date_goals <- c()
    date_points <- c()
    
    LiveGames <- data.table()
    
    for (j in 1:length(games)) {
      
      current_goals <- games[[j]]$goals
      
      if (playoff_date == as.Date(format(Sys.time(), tz = "America/Edmonton", usetz = TRUE))){
        home_team <- games[[j]]$homeTeam$abbrev
        away_team <- games[[j]]$awayTeam$abbrev
        
        teams_tonight <- c(teams_tonight, home_team, away_team)
        
        home_score <- games[[j]]$homeTeam$score
        away_score <- games[[j]]$awayTeam$score
        game_time <- games[[j]]$clock$timeRemaining
        period <- games[[j]]$period  
        startTimeUTC <- games[[j]]$startTimeUTC
        
        if (length(LiveGames) == 0) {
          LiveGames <- data.table(
            Game = paste0(away_team, " vs ", home_team),
            HomeTeam = home_team,
            AwayTeam = away_team,
            HomeScore = ifelse(is.null(home_score), 0, home_score),
            AwayScore = ifelse(is.null(away_score), 0, away_score),
            HomeLogo = paste0(
              "<img src='https://assets.nhle.com/logos/nhl/svg/",home_team,"_light.svg' height='20'>"),
            AwayLogo = paste0(
              "<img src='https://assets.nhle.com/logos/nhl/svg/",away_team,"_light.svg' height='20'>"),
            GameTime = ifelse(is.null(game_time), 0, game_time),
            Period = ifelse(is.null(period), 0, paste0("Period ", period)),
            GameState = games[[j]]$gameState,
            startTimeUTC <- games[[j]]$startTimeUTC
          )
        } else {
          LiveGames <- rbind(LiveGames, data.table(
            Game = paste0(away_team, " vs ", home_team),
            HomeTeam = home_team,
            AwayTeam = away_team,
            HomeScore = ifelse(is.null(home_score), 0, home_score),
            AwayScore = ifelse(is.null(away_score), 0, away_score),
            HomeLogo = paste0(
              "<img src='https://assets.nhle.com/logos/nhl/svg/",home_team,"_light.svg' height='20'>"),
            AwayLogo = paste0(
              "<img src='https://assets.nhle.com/logos/nhl/svg/",away_team,"_light.svg' height='20'>"),
            GameTime = ifelse(is.null(game_time), 0, game_time),
            Period = ifelse(is.null(period), 0, paste0("Period ", period)),
            GameState = games[[j]]$gameState,
            startTimeUTC <- games[[j]]$startTimeUTC
          ))
        }
      }
      
      if (length(current_goals) != 0) {
      for (k in 1:length(current_goals)) {
        current_goal <- current_goals[[k]]
        scorer <- current_goal$playerId
        
        num_assists <- length(current_goal$assists)
        if (num_assists == 0) {
          assist1 <- NA
          assist2 <- NA 
        } else if (num_assists == 1) {
          assist1 <- current_goal$assists[[1]]$playerId
          assist2 <- NA
        } else {
          assist1 <- current_goal$assists[[1]]$playerId
          assist2 <- current_goal$assists[[2]]$playerId
        }
        
        # Append the data to the list
        date_goals <- c(date_goals, scorer)
        date_assists <- c(date_assists, assist1, assist2)
        date_points <- c(date_points, scorer, assist1, assist2)
      }
      }
    }
        
        # Convert the vector to a dataframe, counting how many of each item as the second column
        date_points <- as.data.frame(table(date_points))
        date_goals <- as.data.frame(table(date_goals))
        date_assists <- as.data.frame(table(date_assists))
        
        # In case there is no data, just make it empty for now
        if (nrow(date_points) == 0) {
          date_points <- data.frame(id = NA, Freq = NA)
        }
        if (nrow(date_goals) == 0) {
          date_goals <- data.frame(id = NA, Freq = NA)
        }
        if (nrow(date_assists) == 0) {
          date_assists <- data.frame(id = NA, Freq = NA)
        }
        
        # Merge with long_picks adding a column for points, goals and assists for the current date
        names(date_points) <- c('id', paste('Pts', format(playoff_date, "%Y-%m-%d")))
        names(date_goals) <- c('id', paste('Goals', format(playoff_date, "%Y-%m-%d")))
        names(date_assists) <- c('id', paste('Assists', format(playoff_date, "%Y-%m-%d")))
        date_points <- merge(merge(date_points, date_goals, by = 'id', all = TRUE), date_assists, by = 'id', all = TRUE)
        
        # Replace NA with 0
        date_points[is.na(date_points)] <- 0
        
        # print(date_points)

        # Convert the id column to numeric
        date_points$id <- as.numeric(as.character(date_points$id))
        # Merge with long_picks
        long_picks <- merge(long_picks, date_points, by = 'id', all.x = TRUE)
        
        # Calculate the current total points, goals, and assists
        long_picks[is.na(long_picks)] <- 0
  }
  
  goals_only <- long_picks %>% select(id, contains("Goals"))
  assists_only <- long_picks %>% select(id, contains("Assists"))
  points_only <- long_picks %>% select(id, contains("Pts"))
  # 
  goals_only$goals_total <- rowSums(goals_only[, -1], na.rm = TRUE)
  assists_only$assists_total <- rowSums(assists_only[, -1], na.rm = TRUE)
  points_only$points_total <- rowSums(points_only[, -1], na.rm = TRUE)
  
  points_only <- points_only %>% select(id, points_total)
  goals_only <- goals_only %>% select(id, goals_total)
  assists_only <- assists_only %>% select(id, assists_total)
  
  # For each playoff_date calculate the running point total
  long_picks <- merge(long_picks, assists_only, by = 'id', all.x = TRUE)
  long_picks <- merge(long_picks, goals_only, by = 'id', all.x = TRUE)
  long_picks <- merge(long_picks, points_only, by = 'id', all.x = TRUE)
  long_picks$points_today <- long_picks[[ncol(long_picks) - 5]]
  
  long_picks$player_remaining <- ifelse(long_picks$team %in% teams_in(), 1, 0)
  TeamSum <- long_picks[long_picks$player_remaining == 1, ]
  # Count the number of players remaining for each team
  TeamSum <- TeamSum %>%
    group_by(Team) %>%
    summarise(Remaining = n())
  
  long_picks$Tonight <- ifelse(long_picks$team %in% teams_tonight, 1, 0)
  TonightSum <- long_picks[long_picks$Tonight == 1, ]
  TonightSum <- TonightSum %>%
    group_by(Team) %>%
    summarise(Tonight = n())
 #  ### ESTABLISHED
 #  
 #  today <- Sys.Date()
 #  
 #  # Construct the API URL
 #  # url <- paste0("https://api-web.nhle.com/v1/schedule/", today)
 #  url <- paste0('https://api-web.nhle.com/v1/score/', today)
 # # url <- 'https://api-web.nhle.com/v1/scoreboard/now'
 #  # Make the GET request
 #  response <- GET(url)
 #  
 #  # Parse the JSON content
 #  data <- content(response, as = "parsed", type = "application/json")
 #  
 #  todays_assists <- c()
 #  todays_goals <- c()
 #  todays_points <- c()
 #  
 #  # Extract the relevant data
 #  games <- data$games
 #  for (i in 1:length(games)) {
 #    current_goals <- games[[i]]$goals
 #    for (j in 1:length(current_goals)) {
 #      current_goal <- current_goals[[j]]
 #      scorer <- current_goal$playerId
 #      
 #      num_assists <- length(current_goal$assists)
 #      if (num_assists == 0) {
 #        assist1 <- NA
 #        assist2 <- NA 
 #      } else if (num_assists == 1) {
 #        assist1 <- current_goal$assists[[1]]$playerId
 #        assist2 <- NA
 #      } else {
 #        assist1 <- current_goal$assists[[1]]$playerId
 #        assist2 <- current_goal$assists[[2]]$playerId
 #      }
 #      
 #      # Append the data to the list
 #      todays_goals <- c(todays_goals, scorer)
 #      todays_assists <- c(todays_assists, assist1, assist2)
 #      todays_points <- c(todays_points, scorer, assist1, assist2)
 #    }
 #  }
 # 
 #  # Convert the vector to a dataframe, counting how many of each item as the second column
 #  todays_points <- as.data.frame(table(todays_points))
 #  todays_goals <- as.data.frame(table(todays_goals))
 #  todays_assists <- as.data.frame(table(todays_assists))
 #  
 #  names(todays_points) <- c('id', 'Pts Today')
 #  names(todays_goals) <- c('id', 'Goals Today')
 #  names(todays_assists) <- c('id', 'Assists Today')
 #  
 #  todays_points <- merge(merge(todays_points, todays_goals, by = 'id', all = TRUE), todays_assists, by = 'id', all = TRUE)
 #  
 #  # Convert the id column to numeric
 #  todays_points$id <- as.numeric(as.character(todays_points$id))
 #  
 #  long_picks <- merge(long_picks, todays_points, by = 'id', all.x = TRUE)
 #  
 #  #replace NA with 0
 #  long_picks[is.na(long_picks)] <- 0
 #  
 #  #rename goals, assists, point as Yesterday Goals, Yesterday Assists, Yesterday Points
 #  names(long_picks)[names(long_picks) == 'goals'] <- 'Yesterday Goals'
 #  names(long_picks)[names(long_picks) == 'assists'] <- 'Yesterday Assists'
 #  names(long_picks)[names(long_picks) == 'points'] <- 'Yesterday Points'
 #  
 #  long_picks$`Total Goals` <- long_picks$`Yesterday Goals` + long_picks$`Goals Today`
 #  long_picks$`Total Assists` <- long_picks$`Yesterday Assists` + long_picks$`Assists Today`
 #  long_picks$`Total Points` <- long_picks$`Yesterday Points` + long_picks$`Pts Today`
  
  
  #select Team and any column with Pts in the column name from long_picks
  progress_data <- long_picks[, .SD, .SDcols = c('Team', grep('Pts', names(long_picks), value = TRUE))]
  #Group and aggregate by Team
  progress_data <- progress_data %>%
    group_by(Team) %>%
    summarise(across(everything(), sum, na.rm = TRUE))
  for (i in 3:ncol(progress_data)) {
    # sum prior column and current column, replace value in current column
    progress_data[[i]] <- progress_data[[i]] + progress_data[[i - 1]]
  }
  
  progress_data <- as.data.table(progress_data)
  # use ggplot to create a line chart
  long_progress <- melt(
    progress_data,
    id.vars = "Team",          # keep Team as identifier
    variable.name = "Date",     # name for the new 'date' column
    value.name = "Points"       # name for the points column
  )
  long_progress <- as.data.table(long_progress)
  long_progress[, Date := as.Date(gsub("Pts ", "", Date))]
  # long_progress[, Avatar := paste0("www/", Team, ".png")]
  
  # long_progress$Team <- gsub("\\", "", long_progress$Team)
  
  end_points <- long_progress %>%
    group_by(Team) %>%
    filter(Date == max(Date)) %>%
    ungroup()

  
  
  
  
  
  
  
  
  
  
  # print(teams_tonight)
  
  # Create a new table, summing points by team
  team_points <- long_picks %>%
    group_by(Team) %>%
    summarise(
      total_goals = sum(`goals_total`, na.rm = TRUE),
      total_assists = sum(`assists_total`, na.rm = TRUE),
      total_points = sum(`points_total`, na.rm = TRUE),
      total_today = sum(`points_today`, na.rm = TRUE)
    )
  # Sort team_points by total_points

  team_points <- merge(team_points, TeamSum, by = 'Team', all.x = TRUE)
  team_points$Remaining[is.na(team_points$Remaining)] <- 0
  
  team_points <- merge(team_points, TonightSum, by = 'Team', all.x = TRUE)
  team_points$Tonight[is.na(team_points$Tonight)] <- 0
  
  team_points <- team_points %>%
    arrange(desc(total_points))
  
  names(team_points) <- c('Team', 'Goals', 'Assists', 'Points', 'Pts Today', 'Remaining', 'Playing')
  
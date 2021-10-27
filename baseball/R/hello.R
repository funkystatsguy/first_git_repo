#' A function to say hello
#' @return A statement saying hello to something
#' @export


hello <- function() {
  print("Hello America, the land of the Free")
}


#' A clarification of you don't understand something
#' @return A statement asking what you said
#' @export


huh <- function() {
  print("What did you say?")
}

#' Summary of Hitting
#' @param x The dataset to be summarize, by default hitting.
#' @return A summary of the dataset hitting
#' @export

hit_summary <- function(x = hitting) {
  summary(x)
}

#'Team Average Plot
#'@param x The dataset to be summarize, by default hitting.
#'@param tm The team to be looked for their averages over time
#'@return A plot of the averages of all teams, with a smoothing curve, and the team of interest highlighted in red points.
#'@export
#'

team_avg <- function(x = hitting, tm = "MIN"){
  avg <- x %>% group_by(teamID, yearID) %>% summarise(AVERAGE = sum(H)/sum(AB))
  team <- x %>% filter(teamID == "CIN") %>% group_by(yearID) %>% summarise(AVERAGE = sum(H)/sum(AB))
  ggplot(avg, aes(x = yearID, y = AVERAGE)) +
    geom_point(alpha = 1/15) +
    geom_point(data = team, aes(x = yearID, y = AVERAGE, col = "red"), pch = 19, cex = 3)+
    geom_smooth() +
    theme(legend.position = "none")
}

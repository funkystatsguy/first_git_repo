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

#'Average Statistics Data Frame
#'@param x The dataset to be summarize, by default hitting.
#'@return A dataset of Average statistics across year, to be anlyzed
#'@export

avg_statistics <- function(x = hitting){
  AVG <- x %>% dplyr::group_by(yearID, teamID) %>%
    dplyr::summarise(AVERAGE = sum(H)/sum(AB)) %>% dplyr::summarise(MAX = max(AVERAGE), MIN = min(AVERAGE), MED = median(AVERAGE), MEAN = mean(AVERAGE), SD = sd(AVERAGE))
  return(AVG)
}

#'Generating a Linear Model
#'@param x The dataset to be summarize, by default hitting.
#'@param y The additional column wished to be used on the regression model.
#'@return The summary of the linear model given
#'@export
#'

linear_model <- function(x = hitting, y = "AB"){
  return(summary(lm(H ~ teamID + yearID + x[[y]], data  = x)))
}


#'Team Average Plot
#'@param x The dataset to be summarize, by default hitting.
#'@param tm The team to be looked for their averages over time
#'@return A plot of the averages of all teams, with a smoothing curve, and the team of interest highlighted in red points.
#'@export
#'

team_avg <- function(x = hitting, tm = "MIN"){
  avg <- x %>% dplyr::group_by(teamID, yearID) %>% dplyr::summarise(AVERAGE = sum(H)/sum(AB))
  team <- x %>% dplyr::filter(teamID == "CIN") %>% group_by(yearID) %>% summarise(AVERAGE = sum(H)/sum(AB))
  ggplot2::ggplot(avg, ggplot2::aes(x = yearID, y = AVERAGE)) +
    ggplot2::geom_point(alpha = 1/15) +
    ggplot2::geom_point(data = team, ggplot2::aes(x = yearID, y = AVERAGE, col = "red"), pch = 19, cex = 3)+
    ggplot2::geom_smooth() +
    ggplot2::theme(legend.position = "none")
}

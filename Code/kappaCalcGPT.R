# Load required libraries
library(irr)
library(ggplot2)

kappaCalc <- function(csv){
  # Read and combine ChatGPT predictions into one data frame
  gpt_predictions <- read.csv(csv)[, 2:4]
  # Calculate intrarater reliability (Cohen's Kappa)
  intrarater_kappa <- kappam.fleiss(gpt_predictions)$value
  return(intrarater_kappa)
}

#barplot formatting
barCols <- c("darkgoldenrod1", "cornflowerblue", "darkorchid3")
data <- c(kappaCalc("gptGuess.csv"), kappaCalc("gptGuessClean.csv"), kappam.fleiss(read.csv("gptVhuman.csv"))$value)
barLabels <- c(paste("GPT Output\n(", round(data[1], digits = 4), ")"), paste("GPT Cleaned Output\n(", round(data[2], digits = 4), ")"), paste("GPT Cleaned v.\nHuman Rater Cleaned\n(", round(data[3], digits = 4), ")"))

#barplot creation
par(mar=c(5,10,1,2))
kappaPlot <- barplot(data, horiz = T, xlab = "Kappa Value", names.arg = barLabels, col = barCols, xlim =  c(0,1), las = 1)


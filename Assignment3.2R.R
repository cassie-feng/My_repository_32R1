# Q3.2R.1 -----------------------------------------------------------------------
remind_me <- function() {
    print("The deadline for internship proposal is Thursday 28 March 2024.")
  }

cheat <- function(number){
  if (number %in% 2:4 == 0) {
    stop("input should be 2, 3, or 4.")
  } else{
    if (number == 2) {
      cat(
        "# Load the data:\n",
        'dataQ2 <- read.csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/schiphol_data.csv")\n',
        "\n",
        "# Generate and save the plot:\n",
        "getwd()\n",
        "png('question2.png')\n",
        "plot(x = dataQ2$DATE, \n",
        "     y = dataQ2$TMIN, \n",
        '     main = "Scatterplot of The Minimum Temperature at Schiphol Airport", \n',
        '     xlab = "Time", \n',
        '     ylab = "Temperature")\n',
        "dev.off()"
      )
    }
    
    if (number == 3) {
      cat(
        'install.packages("titanic")\n',
        "library(titanic)\n",
        "library(ggplot2)\n",
        "\n",
        "# Change the original database into clearer format for plotting\n",
        "sexQ3 <- titanic_train$Sex\n",
        'surviveQ3 <- ifelse(titanic_train$Survived == 1, "alive", "dead")\n',
        "classQ3 <- ifelse(\n",
        '  titanic_train$Pclass == 1, "Class 1",\n',
        "  ifelse(\n",
        '    titanic_train$Pclass == 2, "Class 2", "Class 3"\n',
        "  )\n",
        ")\n",
        "\n",
        "# Generate a new data frame for Q3\n",
        "dataQ3 <- data.frame(sexQ3, classQ3, surviveQ3)\n",
        "\n",
        "# Generate and save the plot\n",
        "plotQ3 <- ggplot(dataQ3, aes(x = factor(interaction(classQ3, sexQ3)), fill = surviveQ3)) +\n",
        "  geom_bar() +\n",
        '  labs(x = "Class and Sex", fill = "How did it go?") +\n',
        "  theme(axis.text.x = element_text(angle = 45))\n",
        'ggsave("question3.png", plot = plotQ3)\n'
      )
    }
    
    if (number == 4) {
      cat(
        "# I think the dark is the worst one.\n",
        "# because the dark background makes x-axis labels very unreadable.\n",
        "# (The process of generating pictures has been deleted from R script.) \n",
        "# (For clarity, I generated svg files. )\n"
      )
    }
  }
}


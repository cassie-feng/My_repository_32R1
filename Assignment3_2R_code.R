# Q3.2R.1 -----------------------------------------------------------------------
# Function 1: read_me()
remind_me <- function() {
  print("The deadline for internship proposal is Thursday 28 March 2024.")
}

# Function 2: cheat(), to produce answer of question 2, 3, and 4 for PIPS Assignment3.1 
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




# Q3.2R.2 -----------------------------------------------------------------------
make_art <- function(seed) { 
  # Set the seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Randomly decide whether a pixel has color or not
  has_color <-
    sample(
      c(TRUE, FALSE),
      size = 500 * 500,
      replace = TRUE,
      prob = c(0.3, 0.7)
    )
  
  # Assign colors if has_color is TRUE
  red <- green <- blue <- colour <- rep(NA, 500 * 500)
  for (i in 1:(500 * 500)) {
    if (has_color[i]) {
      red[i] <- sample(0:255, 1)
      green[i] <- sample(0:255, 1)
      blue[i] <- sample(0:255, 1)
      colour[i] <- sprintf("#%02x%02x%02x", red[i], green[i], blue[i])
    }
  }
  
  # Create a data frame
  data <- data.frame(x = rep(1:500, each = 500),
                     y = rep(1:500, times = 500),
                     colour)
  
  # Generate the plot
  ggplot(data, aes(x = x, y = y, fill = colour)) +
    geom_tile() +
    scale_fill_identity() +
    theme_void() +
    coord_fixed(ratio = 1)
}



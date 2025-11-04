# Growth model demo

# Load necessary libraries
library(ggplot2)

# Load sample data
growth_data <- read.csv("growth_model_demo_data.csv")

# Create the plot
ggplot(growth_data, aes(x = School, y = Growth_Score)) +
  geom_point(size = 4, color = "blue") +  # Plot points for schools
  geom_errorbar(aes(ymin = Lower_Bound, ymax = Upper_Bound), 
                width = 0.2, color = "darkgray") +  # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # Add the zero line
  theme_minimal() +
  labs(
    title = "School Growth Scores with Confidence Intervals",
    x = "School",
    y = "Growth Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

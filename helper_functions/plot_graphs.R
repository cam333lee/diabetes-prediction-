library(ggplot2)
library(reshape2)

# 1. Correlation Heatmap
plot_correlation_heatmap <- function(data) {
    correlation_matrix <- cor(data)
    correlation_melted <- melt(correlation_matrix)
    ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                                                 limit = c(-1, 1), space = "Lab", name = "Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Correlation Heatmap", x = "Features", y = "Features")
}

# 2. Distribution of Outcomes
plot_outcome_distribution <- function(data) {
    outcome_counts <- table(data$Outcome)
    outcome_df <- data.frame(Outcome = names(outcome_counts),
                                                     Count = as.numeric(outcome_counts))
    ggplot(outcome_df, aes(x = Outcome, y = Count)) +
        geom_bar(stat = "identity", fill = "pink") +
        labs(title = "Distribution of Diabetes Outcomes", x = "Outcome", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 12),
                    plot.title = element_text(size = 16, face = "bold"))
}

# 3. Distribution of Pregnancies by Outcomes
plot_pregnancies_by_outcome <- function(data) {
    diabetes_subset <- data[, c("Pregnancies", "Glucose", "BloodPressure",
                                                            "BMI", "Age", "Outcome")]
    ggplot(diabetes_subset, aes(x = Pregnancies, fill = factor(Outcome))) +
        geom_histogram(position = "identity", bins = 30, alpha = 0.7) +
        labs(title = "Distribution of Pregnancies by Outcome") +
        facet_wrap(~Outcome, scales = "free_y") +
        theme_minimal()
}

# 4. Boxplot of BMI by Outcomes
plot_bmi_by_outcome <- function(data) {
    ggplot(data, aes(x = factor(Outcome), y = BMI, fill = factor(Outcome))) +
        geom_boxplot() +
        labs(title = "BMI Distribution by Outcome") +
        theme_minimal()
}

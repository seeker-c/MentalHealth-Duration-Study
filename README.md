# PROJECT TITLE: Prevalence and Determinants of Mental Health Problems among Internally Displaced Persons in Northwestern Nigeria
# install.packages("cli", dependencies = TRUE) # Some Installed package below require its dependency 
# install.packages("dplyr") # This package is used for data cleaning
# install.packages("tidyr")
# install.packages("ggplot2") # This package is used for graphical visualization
# install.packages("olsrr") # This package is used to check the multicollinearity assumptions for the logistic model, e.g., the V.I.F. test
# install.packages("ResourceSelection") # This package is used for the Hosmer-Lemeshow "Test of Independence" assumption for the logistic model
# install.packages("VennDiagram")# Used for Venn Diagram Plot to also show Co morbidity 
# install.packages("summarytools")
# install.packages("psych")  # For descriptive statistics of continuous variables
# install.packages("multcompView")
# install.packages("writexl") # Used for converting data frames in R into a CSV file 


# Load the required package and assign to p_data
require(readxl)
Project_ABU_Interpreted_1_ <- read_excel("C:/Users/HP/OneDrive/Project_ABU_Interpreted(1).xlsx")
p_data <- Project_ABU_Interpreted_1_


# Clean the data and Rename some columns in the data frame
# Load the required package
require(dplyr)
require(tibble)
require(writexl)

# Rename Interpretation...31 
p_data <- p_data %>% rename(Ptsd_interpretation = `Interpretation...31`)

# Check unique outcomes in 'Ptsd_interpretation'
unique_ptsd_outcomes <- unique(p_data$Ptsd_interpretation)
print(unique_ptsd_outcomes)

# Assign binary values to 'Ptsd_interpretation',create a binary Column for 'Ptsd_interpretation'
p_data <- p_data %>%
    mutate(Ptsd_binary = ifelse(Ptsd_interpretation == "Post Traumatic Stress Disorder", 1, 0))

# Summary table for PTSD
summary_table_ptsd <- p_data %>%
    group_by(Ptsd_binary, Ptsd_interpretation) %>%
    summarise(Count = n()) %>%
    arrange(Ptsd_binary) %>%
    as_tibble()

# View Summary table for PTSD
View(summary_table_ptsd)

# Save summary_table_ptsd data frame in a CSV file.
write.csv(summary_table_ptsd, "summary_table_ptsd.csv", row.names = FALSE)
cat("File saved as summary_table_ptsd.csv\n")


# Rename Equivalent Interpretation
p_data <- p_data %>% rename(Depression_interpretation = `Equivalent Interpretation`)

# Check unique outcomes in 'Depression_interpretation'
unique_depression_outcomes <- unique(p_data$Depression_interpretation)
print(unique_depression_outcomes)

# Assign integer values to 'Depression_interpretation',create a binary Column for 'Depression_interpretation'
p_data <- p_data %>%
    mutate(Depression_binary = case_when(
        Depression_interpretation == "Minimal depression" ~ 0,
        Depression_interpretation == "Mild depression" ~ 0,
        Depression_interpretation == "Moderate depression" ~ 1,
        Depression_interpretation == "Moderately severe depression" ~ 2,
        Depression_interpretation == "Severe depression" ~ 3,
        TRUE ~ NA_real_  # Assign NA to any unexpected categories
    ))


# Create a summary table For Depression 
summary_table_depression <- p_data %>%
    group_by(Depression_binary, Depression_interpretation) %>%
    summarise(Count = n()) %>%
    arrange(Depression_binary) %>%
    as_tibble()

# View the summary table
View(print(summary_table_depression))

# Save summary_table_depression data frame in a CSV file.
write.csv(summary_table_depression, "summary_table_depression.csv", row.names = FALSE)
cat("File saved as summary_table_depression.csv\n")


# Rename Interpretation...50
p_data <- p_data %>% rename(Anxiety_interpretation = `Interpretation...50`)

# Standardize `Anxiety_interpretation` to lowercase and remove extra spaces
p_data <- p_data %>%
    mutate(Anxiety_interpretation = trimws(tolower(Anxiety_interpretation)))

# Check unique outcomes in 'Anxiety_interpretation'
unique_anxiety_outcomes <- unique(p_data$Anxiety_interpretation)
print(unique_anxiety_outcomes)

# Assign integer values to 'Anxiety_interpretation',create a binary Column for 'Anxiety_interpretation'
p_data <- p_data %>%
    mutate(Anxiety_binary = case_when(
        Anxiety_interpretation == "no anxiety disorder" ~ 0,
        Anxiety_interpretation == "mild anxiety disorder" ~ 0,
        Anxiety_interpretation == "moderate anxiety disorder" ~ 1,
        Anxiety_interpretation == "severe anxiety disorder" ~ 2,
        TRUE ~ NA_real_  # Assign NA to any unexpected categories
    ))

# Summary table for Anxiety
summary_table_anxiety <- p_data %>%
    group_by(Anxiety_binary, Anxiety_interpretation) %>%
    summarise(Count = n()) %>%
    arrange(Anxiety_binary) %>%
    as_tibble()

# View the summary table for Anxiety
View(summary_table_anxiety)

# Save summary_table_anxiety data frame in a CSV file
write.csv(summary_table_anxiety, "summary_table_anxiety.csv", row.names = FALSE)
cat("File saved as summary_table_anxiety.csv\n")


# Check unique variables in Sex
unique_sex_outcomes <- unique(p_data$Sex)
print(unique_sex_outcomes)

# Create a binary column for "Sex"
p_data$Sex_binary <- ifelse(p_data$Sex == "Female", 1, 0)


# Summary table for Anxiety
summary_table_sex <- p_data %>%
    group_by(Sex_binary, Sex) %>%
    summarise(Count = n()) %>%
    arrange(Sex_binary) %>%
    as_tibble()

# View the summary table for Sex
View(summary_table_sex)

# Save summary_table_sex data frame in a CSV file
write.csv(summary_table_sex, "summary_table_sex.csv", row.names = FALSE)
cat("File saved as summary_table_sex.csv\n")


# Calculating the prevalence for each disease entities
require(dplyr)

# Ensure integer values are correctly assigned
p_data <- p_data %>%
    mutate(
        Depression_level = case_when(
            Depression_interpretation == "Minimal depression" ~ 0,
            Depression_interpretation == "Mild depression" ~ 0,
            Depression_interpretation == "Moderate depression" ~ 1,
            Depression_interpretation == "Moderately severe depression" ~ 2,
            Depression_interpretation == "Severe depression" ~ 3,
            TRUE ~ NA_real_  # Assign NA to any unexpected categories
        ),
        Anxiety_level = case_when(
            Anxiety_interpretation == "no anxiety disorder" ~ 0,
            Anxiety_interpretation == "mild anxiety disorder" ~ 0,
            Anxiety_interpretation == "moderate anxiety disorder" ~ 1,
            Anxiety_interpretation == "severe anxiety disorder" ~ 2,
            TRUE ~ NA_real_  # Assign NA to any unexpected categories
        ),
        Ptsd_binary = ifelse(Ptsd_interpretation == "Post Traumatic Stress Disorder", 1, 0)
    )


# Calculate the prevalence percentages
prevalence_data <- p_data %>%
    summarise(
        Depression_prevalence = sum(Depression_binary > 0, na.rm = TRUE) / n() * 100,
        Anxiety_prevalence = sum(Anxiety_binary > 0, na.rm = TRUE) / n() * 100,
        Ptsd_prevalence = sum(Ptsd_binary == 1, na.rm = TRUE) / n() * 100
    )

# Print the prevalence data
print(prevalence_data)


# Load library
require(ggplot2)

# Create a data frame for visualization of prevalence
prevalence_plot_data <- data.frame(
    Mental_Health_Problem = c("Depression", "Anxiety", "PTSD"),
    Prevalence = c(prevalence_data$Depression_prevalence, prevalence_data$Anxiety_prevalence, prevalence_data$Ptsd_prevalence)
)

# View prevalence of each disease entity
View(prevalence_plot_data)

# Save prevalence_plot_data data frame into a CSV file
write.csv(prevalence_plot_data, "prevalence_plot_data.csv", row.names = FALSE)
cat("File saved as prevalence_plot_data.csv\n")


# Create a pie chart for prevalence
ggplot(prevalence_plot_data, aes(x = "", y = Prevalence, fill = Mental_Health_Problem)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_minimal() +
    labs(title = "Prevalence of Mental Health Problems", x = "", y = "") +
    geom_text(aes(label = paste0(round(Prevalence, 1), "%")), 
              position = position_stack(vjust = 0.5)) +
    theme(axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank())


# Create a bar chart for prevalence
bar_chart <- ggplot(prevalence_plot_data, aes(x = Mental_Health_Problem, y = Prevalence, fill = Mental_Health_Problem)) +
    geom_bar(stat = "identity", width = 0.7) +
    theme_minimal() +
    labs(title = "Prevalence of Mental Health Problems", x = "Mental Health Problem", y = "Prevalence (%)") +
    theme(legend.position = "none") +
    geom_text(aes(label = paste0(round(Prevalence, 1), "%")), vjust = -0.5, size = 4)

# Print the bar chart
print(bar_chart)


# Fit logistic regression model
logistic_model <- glm(Ptsd_binary ~ Depression_binary + Anxiety_binary, data = p_data, family = binomial)

# Summarize the model
model_summary <- summary(logistic_model)

# Print the model summary
print(model_summary)

# Calculate odds ratios: Exponentiate the coefficients
odds_ratios <- exp(coef(logistic_model))

# Create a summary table with coefficients, odds ratios, and confidence intervals
summary_table_lg <- data.frame(
    Coefficient = coef(logistic_model),
    Odds_Ratio = odds_ratios,
    Conf_Lower = exp(confint(logistic_model)[, 1]),
    Conf_Upper = exp(confint(logistic_model)[, 2])
)

# View the summary table
View(print(summary_table_lg))

# Save summary_table_lg data frame into a CSV file
write.csv(summary_table_lg, "summary_table_lg.csv", row.names = FALSE)
cat("File saved as summary_table_lg.csv\n")


# Check the assumptions of the Logistic model e.g(Linearity, Multicollinearity, and Goodness of Fit Tests)
# Check linearity using the Box-Tidwell test
p_data <- p_data %>%
    mutate(logDepression = log(Depression_binary + 1),
           logAnxiety = log(Anxiety_binary + 1))

# Fit the logistic model including the interaction terms for Box-Tidwell test
box_tidwell_model <- glm(Ptsd_binary ~ Depression_binary * logDepression + Anxiety_binary * logAnxiety, data = p_data, family = binomial)

# Summarize the model to check interaction terms
summary(box_tidwell_model)

# Check correlation between the two disease entities
cor(p_data$Depression_binary, p_data$Anxiety_binary)

# Check for the Assumptions of Multicolinearity, using V.I.F and Eigen-value condition Index Test
require(olsrr)
ols_coll_diag(logistic_model)


#Check the assumption of Goodness of fit for the Logistics model
# Load necessary library
require(ResourceSelection)

# Ensure the logistic regression model is rightly fitted
logistic_model <- glm(Ptsd_binary ~ Depression_binary + Anxiety_binary, data = p_data, family = binomial)

# Perform Hosmer-Lemeshow for Goodness Of Fit Test
hoslem_test_default <- hoslem.test(p_data$Ptsd_binary, fitted(logistic_model))

# Print the test results
print(hoslem_test_default)


# Relationship Analysis:Explore the relationships between mental health problems and duration of displacement
# Load the required package 
require(dplyr)

# Remove any rows with NA in the duration of displacement
p_data_clean <- p_data %>%
    filter(!is.na(`Duration of displacement (in months)`))

# Group the data by duration of displacement e.g(First 6 months, Second 6 months,Second year,third year)
p_data_clean <- p_data_clean %>%
    mutate(Duration_displacement_group = cut(`Duration of displacement (in months)`, 
                                             breaks = c(0, 6, 12, 24, Inf), 
                                             labels = c("0-6 months", "7-12 months", "13-24 months", "25+ months"))
    )

# Calculate prevalence for each group by duration of displacement
prevalence_by_duration <- p_data_clean %>%
    group_by(Duration_displacement_group) %>%
    summarise(
             Depression_prevalence = sum(Depression_binary > 0, na.rm = TRUE) / n() * 100,
                Anxiety_prevalence = sum(Anxiety_binary > 0, na.rm = TRUE) / n() * 100,
                Ptsd_prevalence = sum(Ptsd_binary == 1, na.rm = TRUE) / n() * 100
            )

print(prevalence_by_duration)

# View prevalence by duration
View(prevalence_by_duration)

# Save prevalence_by_duration  data frame in CSV file
write.csv(prevalence_by_duration, "prevalence_by_duration.csv", row.names = FALSE)
cat("File saved as prevalence_by_duration.csv\n")


# Load the tidyr package 
require(tidyr)

# Transform the data to long format for ggplot visualization
prevalence_long <- prevalence_by_duration %>%
    pivot_longer(cols = c(Depression_prevalence, Anxiety_prevalence, Ptsd_prevalence),
                 names_to = "Mental_Health_Issue",
                 values_to = "Prevalence")

print(prevalence_long)
View(prevalence_long)

# Save prevalence_long data frame in CSV file
write.csv(prevalence_long, "prevalence_long.csv", row.names = FALSE)
cat("File saved as prevalence_long.csv\n")


# Visualization Of Mental Health Problems with prevalence, according to duration of displacement 
# Load ggplot2 for visualization
require(ggplot2)

# Create a bar plot 
# Plot the data using ggplot
ggplot(prevalence_long, aes(x = Duration_displacement_group, y = Prevalence, fill = Mental_Health_Issue)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = sprintf("%.1f%%", Prevalence)), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, 
              size = 3.5) +
    labs(title = "Prevalence of Mental Health Problems by Duration of Displacement",
         x = "Duration of Displacement (in months)",
         y = "Prevalence (%)") +
    scale_fill_manual(name = "Mental Health Issue",
                      values = c("Ptsd_prevalence" = "blue", "Depression_prevalence" = "red", "Anxiety_prevalence" = "green"),
                      labels = c("Ptsd_prevalence" = "PTSD", "Depression_prevalence" = "Depression", "Anxiety_prevalence" = "Anxiety")) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
    )


# Create the heat map with blue gradient
ggplot(prevalence_long, aes(x = Duration_displacement_group, y = Mental_Health_Issue, fill = Prevalence)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "#deebf7", high = "#08306b", na.value = "white") +
    labs(title = "Heat Map of Mental Health Problems by Duration of Displacement",
         x = "Duration of Displacement (in months)",
         y = "Mental Health Issue",
         fill = "Prevalence (%)") +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
    )


# ANOVA to see if there's a significant difference in prevalence based on duration of displacement
anova_ptsd <- aov(Ptsd_binary ~ Duration_displacement_group, data = p_data_clean)
anova_depression <- aov(Depression_binary ~ Duration_displacement_group, data = p_data_clean)
anova_anxiety <- aov(Anxiety_binary ~ Duration_displacement_group, data = p_data_clean)

summary(anova_ptsd)
summary(anova_depression)
summary(anova_anxiety)


# Post-Hoc Test, since there is a significant difference in all the ANOVA Tests 
# Tukey's Honest Significant Difference test for PTSD
TukeyHSD(anova_ptsd)

# Tukey's HSD test for Depression
TukeyHSD(anova_depression)

# Tukey's HSD test for Anxiety
TukeyHSD(anova_anxiety)


# Crete a summary table/ data frame for ANOVA Results
# Load required libraries
require(dplyr)
require(tidyr)
require(broom)
require(ggplot2)
require(multcompView)

# Extract p-values from ANOVA summaries
anova_summary <- data.frame(
    Model = c("PTSD", "Depression", "Anxiety"),
    p_value = c(
        summary(anova_ptsd)[[1]][["Pr(>F)"]][1],
        summary(anova_depression)[[1]][["Pr(>F)"]][1],
        summary(anova_anxiety)[[1]][["Pr(>F)"]][1]
    )
)

# Perform Tukey's HSD tests and assign to variables
tukey_ptsd <- TukeyHSD(anova_ptsd)$Duration_displacement_group
tukey_depression <- TukeyHSD(anova_depression)$Duration_displacement_group
tukey_anxiety <- TukeyHSD(anova_anxiety)$Duration_displacement_group

# Convert Tukey's HSD results to data frames
tukey_ptsd_df <- as.data.frame(tukey_ptsd)
tukey_depression_df <- as.data.frame(tukey_depression)
tukey_anxiety_df <- as.data.frame(tukey_anxiety)

# Add Model columns
tukey_ptsd_df$Model <- "PTSD"
tukey_depression_df$Model <- "Depression"
tukey_anxiety_df$Model <- "Anxiety"

# Add Comparison columns
tukey_ptsd_df$Comparison <- rownames(tukey_ptsd_df)
tukey_depression_df$Comparison <- rownames(tukey_depression_df)
tukey_anxiety_df$Comparison <- rownames(tukey_anxiety_df)

# Combine Tukey's HSD results into a single data frame
tukey_summary <- bind_rows(
    tukey_ptsd_df,
    tukey_depression_df,
    tukey_anxiety_df
) %>%
    select(Model, Comparison, everything()) %>%
    arrange(Model, `p adj`)

# Add significance level and significance status columns
tukey_summary <- tukey_summary %>%
    mutate(Significance_Level = case_when(
        `p adj` < 0.001 ~ "***",
        `p adj` < 0.01 ~ "**",
        `p adj` < 0.05 ~ "*",
        `p adj` < 0.1 ~ ".",
        TRUE ~ ""
    ),
    Significant = ifelse(`p adj` < 0.05, "Yes", "No"))

# Combine ANOVA and Tukey's HSD summaries into one table / data frame
anova_tukey_summary <- left_join(
    anova_summary,
    tukey_summary,
    by = "Model"
)

# Print the combined summary table
print(anova_tukey_summary)

# View the combined summary table of ANOVA and Tukey's HSD test
View(anova_tukey_summary)

# Save anova_tukey_summary data frame in CSV file
write.csv(anova_tukey_summary, "anova_tukey_summary.csv", row.names = FALSE)
cat("File saved as anova_tukey_summary.csv\n")


# Fit a linear regression to see the effect of continuous duration on mental health problems
lm_ptsd <- lm(Ptsd_binary ~ `Duration of displacement (in months)`, data = p_data_clean)
lm_depression <- lm(Depression_binary ~ `Duration of displacement (in months)`, data = p_data_clean)
lm_anxiety <- lm(Anxiety_binary ~ `Duration of displacement (in months)`, data = p_data_clean)

# Summarise the results
summary_lm_ptsd <- summary(lm_ptsd)
summary_lm_depression <- summary(lm_depression)
summary_lm_anxiety <- summary(lm_anxiety)

# Obtain the regression models
summary_lm_ptsd
summary_lm_depression
summary_lm_anxiety 

# Create a summary table for the Regression Analysis result
# Load required libraries
require(dplyr)
require(broom)

# Extract key results function
extract_results <- function(model_summary) {
    coef <- coef(model_summary)
    data.frame(
        Estimate = coef["`Duration of displacement (in months)`", "Estimate"],
        Std_Error = coef["`Duration of displacement (in months)`", "Std. Error"],
        t_value = coef["`Duration of displacement (in months)`", "t value"],
        p_value = coef["`Duration of displacement (in months)`", "Pr(>|t|)"],
        R_squared = model_summary$r.squared
    )
}

# Create a summary table/ data frame
lm_summary <- bind_rows(
    extract_results(summary_lm_ptsd) %>% mutate(Model = "PTSD"),
    extract_results(summary_lm_depression) %>% mutate(Model = "Depression"),
    extract_results(summary_lm_anxiety) %>% mutate(Model = "Anxiety")
)

# Reorder columns
lm_summary <- lm_summary %>% select(Model, everything())

# View the summary table for the Regression Analysis
View(lm_summary)


# Venn Diagram Visualizing co morbidity 
require(VennDiagram)

# Clean and prepare data
p_data_clean <- p_data %>%
    filter(!is.na(`Duration of displacement (in months)`)) %>%
    mutate(
        Depression_present = ifelse(Depression_binary > 0, 1, 0),  # Any level of depression
        Anxiety_present = ifelse(Anxiety_binary > 0, 1, 0),        # Any level of anxiety
        Ptsd_present = ifelse(Ptsd_binary == 1, 1, 0)              # Presence of PTSD
    )

# Calculate the intersection sizes
depression_only <- sum(p_data_clean$Depression_present == 1 & p_data_clean$Anxiety_present == 0 & p_data_clean$Ptsd_present == 0)
anxiety_only <- sum(p_data_clean$Anxiety_present == 1 & p_data_clean$Depression_present == 0 & p_data_clean$Ptsd_present == 0)
ptsd_only <- sum(p_data_clean$Ptsd_present == 1 & p_data_clean$Depression_present == 0 & p_data_clean$Anxiety_present == 0)

# Calculate all the necessary intersections 
depression_anxiety <- sum(p_data_clean$Depression_present == 1 & p_data_clean$Anxiety_present == 1 & p_data_clean$Ptsd_present == 0)
depression_ptsd <- sum(p_data_clean$Depression_present == 1 & p_data_clean$Ptsd_present == 1 & p_data_clean$Anxiety_present == 0)
anxiety_ptsd <- sum(p_data_clean$Anxiety_present == 1 & p_data_clean$Ptsd_present == 1 & p_data_clean$Depression_present == 0)
all_three <- sum(p_data_clean$Depression_present == 1 & p_data_clean$Anxiety_present == 1 & p_data_clean$Ptsd_present == 1)

# Ensure logical consistency
total_depression <- sum(p_data_clean$Depression_present == 1)
total_anxiety <- sum(p_data_clean$Anxiety_present == 1)
total_ptsd <- sum(p_data_clean$Ptsd_present == 1)

# Print all calculated values 
cat("Depression only:", depression_only, "\n")
cat("Anxiety only:", anxiety_only, "\n")
cat("PTSD only:", ptsd_only, "\n")
cat("Depression and Anxiety:", depression_anxiety, "\n")
cat("Depression and PTSD:", depression_ptsd, "\n")
cat("Anxiety and PTSD:", anxiety_ptsd, "\n")
cat("All three:", all_three, "\n")
cat("Total Depression:", total_depression, "\n")
cat("Total Anxiety:", total_anxiety, "\n")
cat("Total PTSD:", total_ptsd, "\n")

# Create a Venn diagram
venn.plot <- draw.triple.venn(
    area1 = total_depression,
    area2 = total_anxiety,
    area3 = total_ptsd,
    n12 = depression_anxiety + all_three,  # Correct intersection
    n23 = anxiety_ptsd + all_three,        # Correct intersection
    n13 = depression_ptsd + all_three,     # Correct intersection
    n123 = all_three,
    category = c("Depression", "Anxiety", "PTSD"),
    fill = c("red", "green", "blue"),
    lty = "blank",
    cex = 2,
    cat.cex = 2,
    cat.col = c("red", "green", "blue")
)

# View the column names to understand the structure
colnames(p_data)

# Standardize column names by replacing spaces with underscores, removing dots, and trimming whitespace
require(stringr)
colnames(p_data) <- colnames(p_data) %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("\\.", "") %>%
    trimws()

# Load required library
require(dplyr)

# Create a vector with the new names for the columns at specific positions
new_names <- c(
    "Separated_From_Family", "Livelihood_Destroyed", "Family_Loss", "Injury", 
    "Abode_Destroyed", "Sexual_Violence", 
    "Business_Continued", "Access_To_Basics", "Dependence_On_Help", 
    "Received_Support", "Support_From_Persons_Organizations",  "Enough_Space", "Safety_In_Camp", 
    "Accepted_By_Host", "Supported_By_Family", "Discrimination", 
    "Substance_Use"
)

# Define the positions of the columns you want to rename
positions <- c(52, 53, 59, 62, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76)

# Rename columns based on their positions
for (i in seq_along(positions)) {
    col_position <- positions[i]
    new_name <- new_names[i]
    colnames(p_data)[col_position] <- new_name
}

# View the updated column names to ensure they have been renamed correctly
colnames(p_data)

# Check unique values for renamed columns
renamed_columns <- c(52, 53, 59, 62, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76)

# Print unique values for each renamed column
for (col in renamed_columns) {
    cat("Unique values in column", colnames(p_data)[col], ":\n")
    print(unique(p_data[[col]]))
    cat("\n")
}
    

# Remove rows with NA values in relevant columns
p_data <- p_data %>%
    filter(
        !is.na(`Separated_From_Family`),
        !is.na(`Livelihood_Destroyed`),
        !is.na(Family_Loss),
        !is.na(Injury),
        !is.na(Abode_Destroyed),
        !is.na(Sexual_Violence),
        !is.na(Business_Continued),
        !is.na(Access_To_Basics),
        !is.na(Dependence_On_Help),
        !is.na(Received_Support),
        !is.na(`Support_From_Persons_Organizations`),
        !is.na(Enough_Space),
        !is.na(Safety_In_Camp),
        !is.na(Accepted_By_Host),
        !is.na(Supported_By_Family),
        !is.na(Discrimination),
        !is.na(Substance_Use)
    )

# Assign integer/binary values to the renamed columns
p_data <- p_data %>%
    mutate(
        separated_from_family = case_when(
            `Separated_From_Family` == "Yes" ~ 1,
            `Separated_From_Family` == "No" ~ 0,
            `Separated_From_Family` == "Partially" ~ 2
        ),
        livelihood_destroyed = case_when(
            `Livelihood_Destroyed` == "Yes" ~ 1,
            `Livelihood_Destroyed` == "No" ~ 0,
            `Livelihood_Destroyed` == "Partially" ~ 2
        ),
        Family_Loss = case_when(
            Family_Loss == "Yes" ~ 1,
            Family_Loss == "No" ~ 0,
            Family_Loss == "Partially" ~ 2
        ),
        Injury = ifelse(Injury == "Yes", 1, 0),
        Abode_Destroyed = case_when(
            Abode_Destroyed == "Yes" ~ 1,
            Abode_Destroyed == "No" ~ 0,
            Abode_Destroyed == "Partially" ~ 2
        ),
        Sexual_Violence = ifelse(Sexual_Violence == "Yes", 1, 0),
        Business_Continued = case_when(
            Business_Continued == "Yes" ~ 1,
            Business_Continued == "No" ~ 0,
            Business_Continued == "Partially" ~ 2
        ),
        Access_To_Basics = case_when(
            Access_To_Basics == "Yes" ~ 1,
            Access_To_Basics == "No" ~ 0,
            Access_To_Basics == "Partially" ~ 2
        ),
        Dependence_On_Help = case_when(
            Dependence_On_Help == "Yes" ~ 1,
            Dependence_On_Help == "No" ~ 0,
            Dependence_On_Help == "Partially" ~ 2
        ),
        Received_Support = ifelse(Received_Support == "Yes", 1, 0),
        
        Support_From_Persons_Organizations = case_when(
            `Support_From_Persons_Organizations` == "Yes" ~ 1,
            `Support_From_Persons_Organizations` == "No" ~ 0,
            `Support_From_Persons_Organizations` == "Partially" ~ 2
        ),
        Enough_Space = case_when(
            Enough_Space == "Yes" ~ 1,
            Enough_Space == "No" ~ 0,
            Enough_Space == "Partially" ~ 2
        ),
        Safety_In_Camp = case_when(
            Safety_In_Camp == "Yes" ~ 1,
            Safety_In_Camp == "No" ~ 0,
            Safety_In_Camp == "Partially" ~ 2
        ),
        Accepted_By_Host = case_when(
            Accepted_By_Host == "Yes" ~ 1,
            Accepted_By_Host == "No" ~ 0,
            Accepted_By_Host == "Partially" ~ 2
        ),
        Supported_By_Family = case_when(
            Supported_By_Family == "Yes" ~ 1,
            Supported_By_Family == "No" ~ 0,
            Supported_By_Family == "Partially" ~ 2
        ),
        Discrimination = case_when(
            Discrimination == "Yes" ~ 1,
            Discrimination == "No" ~ 0,
            Discrimination == "Partially" ~ 2
        ),
        Substance_Use = case_when(
            Substance_Use == "Yes" ~ 1,
            Substance_Use == "No" ~ 0,
            Substance_Use == "Partially" ~ 2
        )
    )


# Assign integer values to education levels and marital status
p_data <- p_data %>%
    mutate(
        Highest_level_of_Education = case_when(
            Highest_level_of_Education == "None" ~ 0,
            Highest_level_of_Education == "Primary School" ~ 1,
            Highest_level_of_Education == "Quranic school" ~ 2,
            Highest_level_of_Education == "Secondary school" ~ 3
        ),
        Marital_Status = case_when(
            Marital_Status == "Single" ~ 0,
            Marital_Status == "Married" ~ 1,
            Marital_Status == "Divorced" ~ 2,
            Marital_Status == "Widowed" ~ 3,
            Marital_Status == "Separated" ~ 4
        )
    )


# Load required library
library(dplyr)
library(psych)

# Define the function to calculate frequencies and percentages
calc_freq_percent <- function(column, description) {
    table_data <- table(p_data[[column]])
    freq <- as.data.frame(table_data)
    colnames(freq) <- c("Value", "Frequency")
    freq$Percentage <- round((freq$Frequency / sum(freq$Frequency)) * 100, 2)
    freq$Variable <- column
    freq$Description <- description[as.character(freq$Value)]
    return(freq)
}

# Define the function to calculate descriptive statistics for continuous variables
calc_descriptive_stats <- function(column) {
    stats <- describe(p_data[[column]])
    data.frame(
        Variable = column,
        Mean = round(stats$mean, 2),
        SD = round(stats$sd, 2),
        Min = stats$min,
        Max = stats$max
    )
}

# List of categorical and continuous columns
categorical_columns <- c("Sex_binary", "Highest_level_of_Education", "Marital_Status", 
                         "separated_from_family", "livelihood_destroyed", "Family_Loss", 
                         "Abode_Destroyed", "Business_Continued", "Access_To_Basics", 
                         "Dependence_On_Help","Received_Support", "Support_From_Persons_Organizations",
                         "Enough_Space", "Safety_In_Camp", "Accepted_By_Host", "Supported_By_Family", 
                         "Discrimination", "Substance_Use"
) 

# List the continious columns 
continuous_columns <- c("Age", "Duration_of_displacement_(in_months)", "Family_size") 

# Descriptions for categorical columns
descriptions <- list(
    Sex_binary = c("1" = "Female", "0" = "Male"),
    Highest_level_of_Education = c("0" = "None", "1" = "Primary", "2" = "Secondary", "3" = "Tertiary"),
    Marital_Status = c("0" = "Single", "1" = "Married", "2" = "Divorced", "3" = "Widowed", "4" = "Seprated"),
    separated_from_family = c("0" = "No", "1" = "Yes", "2" = "Partially"),
    livelihood_destroyed = c("0" = "No", "1" = "Yes", "2" = "Partially"), 
    Family_Loss  = c("0" = "No", "1" = "Yes", "2" = "Partially"), Injury = c("1" = "Yes", "0" = "No"),
    Abode_Destroyed = c("1" = "Yes", "0" = "No", "2" = "Partially"), Sexual_Violence =  c("1" = "Yes", "0" = "No"), 
    Business_Continued = c("1" = "Yes", "0" = "No", "2" = "Partially"), 
    Access_To_Basics = c("1" = "Yes", "0" = "No", "2" = "Partially"), 
    Dependence_On_Help = c("1" = "Yes", "0" = "No", "2" = "Partially"), 
    Received_Support = c("1" = "Yes", "0" = "No"),
    Support_From_Persons_Organizations = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Enough_Space = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Safety_In_Camp = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Accepted_By_Host = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Supported_By_Family = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Discrimination = c("1" = "Yes", "0" = "No", "2" = "Partially"),
    Substance_Use = c("1" = "Yes", "0" = "No", "2" = "Partially")
)


# Calculate frequencies and percentages for each categorical column
categorical_summary <- lapply(categorical_columns, function(col) {
    calc_freq_percent(col, descriptions[[col]])
})
categorical_summary


# Combine all categorical summaries into one data frame
categorical_summary_df <- bind_rows(categorical_summary)

# Calculate descriptive statistics for each continuous column
continuous_summary <- lapply(continuous_columns, calc_descriptive_stats)
continuous_summary

# Combine all continuous summaries into one data frame
continuous_summary_df <- bind_rows(continuous_summary)

# Combine categorical and continuous summaries into a single data frame
combined_summary <- bind_rows(
    categorical_summary_df %>%
        select(Variable, Value,Description, Frequency, Percentage),
    continuous_summary_df %>%
        rename(Value = Variable) %>%
        select(Variable = Value, Mean, SD, Min, Max)
)

# Print the combined summary
print(combined_summary)

# View the combined summary
View(combined_summary)

# Save combined_summary DF in CSV file 
write.csv(combined_summary, "combined_summary.csv", row.names = FALSE)
cat("File saved as combined_summary.csv\n")


# Fit Interplay regression models for PTSD, Anxiety, and Depression with other variables
# Load necessary packages
library(dplyr)
library(broom)

multiple_regression_ptsd <- lm(Ptsd_binary ~ Age + Sex_binary + Highest_level_of_Education + Marital_Status + Separated_From_Family + Livelihood_Destroyed + Family_Loss + Injury + Abode_Destroyed + Sexual_Violence + Business_Continued + Access_To_Basics + Dependence_On_Help + Received_Support + Support_From_Persons_Organizations + Enough_Space + Safety_In_Camp + Accepted_By_Host + Supported_By_Family + Discrimination + Substance_Use, 
                               data = p_data)

multiple_regression_anxiety <- lm(Anxiety_binary ~ Age + Sex_binary + Highest_level_of_Education + Marital_Status + Separated_From_Family + Livelihood_Destroyed + Family_Loss + Injury + Abode_Destroyed + Sexual_Violence + Business_Continued + Access_To_Basics + Dependence_On_Help + Received_Support + Support_From_Persons_Organizations + Enough_Space + Safety_In_Camp + Accepted_By_Host + Supported_By_Family + Discrimination + Substance_Use, 
                                  data = p_data)

multiple_regression_depression <- lm(Depression_binary ~ Age + Sex_binary + Highest_level_of_Education + Marital_Status + Separated_From_Family + Livelihood_Destroyed + Family_Loss + Injury + Abode_Destroyed + Sexual_Violence + Business_Continued + Access_To_Basics + Dependence_On_Help + Received_Support + Support_From_Persons_Organizations + Enough_Space + Safety_In_Camp + Accepted_By_Host + Supported_By_Family + Discrimination + Substance_Use, 
                                     data = p_data)

# Use broom to tidy up the model outputs
tidy_ptsd <- tidy(multiple_regression_ptsd)
tidy_anxiety <- tidy(multiple_regression_anxiety)
tidy_depression <- tidy(multiple_regression_depression)

# Add a column to indicate the model
tidy_ptsd <- tidy_ptsd %>% mutate(Model = "PTSD")
tidy_anxiety <- tidy_anxiety %>% mutate(Model = "Anxiety")
tidy_depression <- tidy_depression %>% mutate(Model = "Depression")

# Combine the tidy data frames into one summary table
combined_summary1<- bind_rows(tidy_ptsd, tidy_anxiety, tidy_depression)

# Add a column for significance levels
combined_summary1 <- combined_summary1 %>%
    mutate(Significance = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ ".",
        TRUE ~ ""
    ))

# Reorder columns to have Model and Significance as the first columns
combined_summary1 <- combined_summary1 %>% select(Model, term, estimate, std.error, statistic, p.value, Significance, everything())

# Print the combined summary 
print(combined_summary1)

# View the combined summary of all disease entity and interplay between important variables in the project
View(combined_summary1)

# Save combined_summary1 DF in CSV file
write.csv(combined_summary1, "combined_summary1.csv", row.names = FALSE)
cat("File saved as combined_summary1.csv\n")

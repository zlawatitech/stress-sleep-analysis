install.packages("haven") 
install.packages("tidyverse")
install.packages("psych") 
install.packages("knitr")

# Load required libraries
library(haven)     # For importing SPSS (.sav) files
library(tidyverse) # For data manipulation and visualization
library(psych)     # For descriptive statistics
library(knitr)     # For report formatting

# Import SPSS data (replace 'stress_study.sav' with your file)
##data <- read_sav("stress_study.sav")
data <- read_csv("C:\\Users\\zlawa\\OneDrive\\Documents\\sleepVSstress\\StressSleep.csv")


# View variable names and structure
glimpse(data)


# Rename variables for clarity
data_clean <- data %>%
  rename(
    stress = stress,   # Perceived Stress Scale (PSS) score
    sleep = sleep,   # Pittsburgh Sleep Quality Index (PSQI) score
    gender = gender       # 1 = Male, 2 = Female
  )

# Handle missing values (if any)
data_clean <- data_clean %>%
  drop_na() # Removes rows with missing values

# Convert gender to a factor with labels
data_clean$gender <- factor(data_clean$gender,
                            levels = c(1, 2),
                            labels = c("Male", "Female"))

# Summary statistics for stress and sleep scores
desc_stats <- describe(data_clean %>% select(stress, sleep))
kable(desc_stats, caption = "Descriptive Statistics")

# Group comparison by gender
group_stats <- data_clean %>%
  group_by(gender) %>%
  summarise(
    mean_stress = mean(stress),
    sd_stress = sd(stress),
    mean_sleep = mean(sleep),
    sd_sleep = sd(sleep)
  )
kable(group_stats, caption = "Stress and Sleep Scores by Gender")

# Scatterplot: Stress vs. Sleep
ggplot(data_clean, aes(x = stress, y = sleep)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Stress vs. Sleep Quality",
       x = "Perceived Stress Score",
       y = "Sleep Quality Index") +
  theme_minimal()

# Boxplot: Sleep Quality by Gender
ggplot(data_clean, aes(x = gender, y = sleep, fill = gender)) +
  geom_boxplot() +
  labs(title = "Sleep Quality by Gender",
       x = "Gender",
       y = "Sleep Quality Index") +
  theme_minimal()

## Hypothesis Testing ##
# Correlation between stress and sleep
cor_test <- cor.test(data_clean$stress, data_clean$sleep)
print(cor_test)

# Independent t-test: Sleep quality by gender
t_test <- t.test(sleep ~ gender, data = data_clean)
print(t_test)

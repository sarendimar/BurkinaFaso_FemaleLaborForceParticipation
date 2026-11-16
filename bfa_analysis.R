# ----------------- setup and data import -----------------

install.packages(c("tidyverse", "here"))

library(tidyverse)
library(here)

bfa_data <- read.csv("~/Desktop/BurkinaFaso/indicators_bfa.csv")

View(bfa_data)

# ----------------- look through data -----------------

names(bfa_data)

str(bfa_data)

summary(bfa_data)

colSums(is.na(bfa_data))

sum(duplicated(bfa_data$year))

# sort indicators

length(unique(bfa_data$Indicator.Name))

indicators <- sort(unique(bfa_data$Indicator.Name))

View(indicators)

indicators <- data.frame(
  Indicator = sort(unique(bfa_data$Indicator.Name))
)

# ----------------- create clean labor dataset -----------------

library(dplyr)

labor_data <- bfa_data %>%
  filter(Indicator.Name %in% c(
    "Labor force participation rate, female (% of female population ages 15+) (national estimate)",
    "Labor force participation rate, male (% of male population ages 15+) (national estimate)"
  ))

library(tidyr)
library(dplyr)

labor_wide <- labor_data %>%
  mutate(
    Indicator.Name = case_when(
      grepl("female", Indicator.Name, ignore.case = TRUE) ~ "Female",
      grepl("male", Indicator.Name, ignore.case = TRUE) ~ "Male"
    )
  ) %>%
  pivot_wider(
    names_from = Indicator.Name,
    values_from = Value
  )

View(labor_wide)

library(dplyr)
library(tidyr)

 labor_wide2 <- labor_wide %>%
      pivot_longer(
           cols = c(Female, Male),
           names_to = "Sex",
           values_to = "Participation_Rate",
           values_drop_na = TRUE
        ) %>%
       select(Country.Name, Country.ISO3, Year,
                           Sex, Participation_Rate) %>%
       pivot_wider(
             names_from = Sex,
            values_from = Participation_Rate
        )
View(labor_wide2)

clean_data <- labor_wide2 %>%
  select(Year, Female, Male) %>%
  arrange(Year)

# ----------------- add gender gap -----------------

clean_data <- clean_data %>%
  mutate(
    gender_gap = Male - Female
  )

# ----------------- lf participation plot -----------------

long_data <- clean_data %>%
  pivot_longer(
    cols = c(Female, Male),
    names_to = "gender",
    values_to = "participation"
  )

plot1 <- ggplot(long_data,
       aes(x = Year,
           y = participation,
           color = gender)) +
  
  geom_line(linewidth = 0.5) +
  
  labs(
    title = "Labor Force Participation in Burkina Faso",
    subtitle = "Male versus female participation, 1975–2025",
    x = "Year",
    y = "Labor force participation (%)",
    color = "Gender"
  ) +
  
  theme_minimal()

ggsave(
  here("outputs", "participation_trends.png"),
  plot = plot1,
  width = 9,
  height = 6
)

# ----------------- gender gap plot -----------------

plot2 <- ggplot(clean_data,
       aes(x = Year,
           y = gender_gap)) +
  
  geom_line(color = "steelblue",
            linewidth = 0.5) +
  
  geom_vline(xintercept = 1987,
             linetype = "dashed",
             color = "red") +
  
  labs(
    title = "The Gender Gap in Labor Force Participation",
    subtitle = "Burkina Faso, 1975–2025",
    x = "Year",
    y = "Difference in participation (percentage points)",
    caption = "Dashed line indicates 1987"
  ) +
  
  theme_minimal()

ggsave(
  here("outputs", "gender_gap.png"),
  plot = plot2,
  width = 9,
  height = 6
)

# ----------------- linear regression and plots -----------------

model <- lm(
  Female ~ Year,
  data = clean_data
)

summary(model)

library(ggplot2)

plot3 <- ggplot(clean_data, aes(x = Year, y = Female)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Female Labor Force Participation Over Time",
    subtitle = "Linear Regression Model",
    x = "Year",
    y = "Female Labor Force Participation (%)"
  ) +
  theme_minimal()

ggsave(
  here("outputs", "flfp_regression.png"),
  plot = plot3,
  width = 9,
  height = 6
)

clean_data$Predicted <- predict(model)

plot4 <- ggplot(clean_data, aes(x = Year)) +
  geom_point(aes(y = Female), size = 1) +
  geom_line(aes(y = Predicted), linewidth = 0.5) +
  labs(
    title = "Actual vs. Predicted Female Labor Force Participation",
    x = "Year",
    y = "Participation Rate (%)"
  ) +
  theme_minimal()

ggsave(
  here("outputs", "regression_pred.png"),
  plot = plot4,
  width = 9,
  height = 6
)

# ----------------- compare pre/post 1987 -----------------

period_summary <- clean_data %>%
  mutate(
    period = ifelse(Year < 1987,
                    "Before 1987",
                    "1987 and after")
  ) %>%
  group_by(period) %>%
  summarise(
    avg_female = mean(Female),
    avg_male = mean(Male),
    avg_gap = mean(gender_gap),
    observations = n()
  )

print(period_summary)

# ----------------- save cleaned dataset -----------------

write_csv(
       clean_data,
       "~/Desktop/bfa_labor_force_clean.csv"
   )


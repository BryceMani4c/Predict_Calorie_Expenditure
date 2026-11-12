# Code written with help from Claude (AI assistant); see the AI-use note in the report Appendix.

library(tidyverse)
library(skimr)
library(hexbin)
library(patchwork)

set.seed(3220)

dir.create("figures", showWarnings = FALSE)
dir.create("output",  showWarnings = FALSE)

numeric_vars <- c("Age", "Height", "Weight", "Duration",
                  "Heart_Rate", "Body_Temp", "Calories")

unit_labels <- c(
  Age        = "Age (years)",
  Height     = "Height (cm)",
  Weight     = "Weight (kg)",
  Duration   = "Duration (minutes)",
  Heart_Rate = "Heart rate (bpm)",
  Body_Temp  = "Body temperature (\u00B0C)",
  Calories   = "Calories burned (kcal)"
)

train <- read_csv("data/train.csv", show_col_types = FALSE)
test  <- read_csv("data/test.csv",  show_col_types = FALSE)

dim(train)
dim(test)
glimpse(train)

colSums(is.na(train))
colSums(is.na(test))

sum(duplicated(train))
sum(duplicated(select(train, -id)))
sum(duplicated(select(train, -id, -Calories)))
sum(duplicated(select(test, -id)))

train <- train %>% mutate(Sex = factor(Sex))
test  <- test  %>% mutate(Sex = factor(Sex))

count(train, Sex)
count(test, Sex)

skim(train)
skim(test)

train %>% arrange(Height) %>% head(10)
train %>% arrange(desc(Height)) %>% head(10)

train %>% arrange(Weight) %>% head(10)
train %>% arrange(desc(Weight)) %>% head(10)

train %>%
  mutate(BMI = Weight / (Height / 100)^2) %>%
  summarise(min_BMI = min(BMI), median_BMI = median(BMI), max_BMI = max(BMI))

train %>% count(Body_Temp) %>% arrange(Body_Temp) %>% head(10)

train %>% filter(Calories <= 5) %>% count(Duration)

fig_boxplots <- train %>%
  select(all_of(numeric_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_x",
             labeller = as_labeller(unit_labels)) +
  labs(x = "Value (units in panel titles)", y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
fig_boxplots

iqr_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr)
}

train %>% summarise(across(all_of(numeric_vars), iqr_outliers))

train <- train %>% filter(!(Calories <= 5 & Duration >= 8))
nrow(train)

both <- bind_rows(
  train %>% select(-Calories) %>% mutate(set = "train"),
  test  %>% mutate(set = "test")
)

fig_train_test <- both %>%
  select(-id, -Sex) %>%
  pivot_longer(-set, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, colour = set)) +
  geom_density() +
  facet_wrap(~ variable, scales = "free", labeller = as_labeller(unit_labels)) +
  labs(x = "Value (units in panel titles)", y = "Density", colour = "Data set")
fig_train_test

fig_cal_hist <- ggplot(train, aes(x = Calories)) +
  geom_histogram(bins = 50) +
  labs(x = "Calories burned (kcal)", y = "Number of workouts")
fig_cal_hist

fig_cal_log_hist <- ggplot(train, aes(x = log1p(Calories))) +
  geom_histogram(bins = 50) +
  labs(x = "log(1 + Calories burned)", y = "Number of workouts")
fig_cal_log_hist

fig_cal_both <- fig_cal_hist + fig_cal_log_hist
fig_cal_both

sample_rows <- slice_sample(train, n = 5000)

fig_cal_duration <- ggplot(sample_rows, aes(x = Duration, y = Calories, colour = Sex)) +
  geom_point(alpha = 0.4) +
  labs(x = "Workout duration (minutes)", y = "Calories burned (kcal)", colour = "Sex")
fig_cal_duration

fig_cal_hr <- ggplot(train, aes(x = Heart_Rate, y = Calories)) +
  geom_hex() +
  labs(x = "Heart rate (beats per minute)", y = "Calories burned (kcal)",
       fill = "Number of workouts")
fig_cal_hr

cor_matrix <- train %>% select(all_of(numeric_vars)) %>% cor()
round(cor_matrix, 2)

fig_corr <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(limits = c(-1, 1)) +
  labs(x = "Variable", y = "Variable", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig_corr

fig_cal_sex <- ggplot(train, aes(x = Sex, y = Calories)) +
  geom_boxplot() +
  labs(x = "Sex", y = "Calories burned (kcal)")
fig_cal_sex

ggsave("figures/boxplots_all_variables.png", fig_boxplots,     width = 7, height = 5, dpi = 300)
ggsave("figures/train_vs_test_density.png",  fig_train_test,   width = 7, height = 5, dpi = 300)
ggsave("figures/calories_histogram.png",     fig_cal_hist,     width = 6, height = 4, dpi = 300)
ggsave("figures/calories_log_histogram.png", fig_cal_log_hist, width = 6, height = 4, dpi = 300)
ggsave("figures/calories_hist_raw_and_log.png", fig_cal_both,  width = 10, height = 4, dpi = 300)
ggsave("figures/calories_vs_duration.png",   fig_cal_duration, width = 6, height = 4, dpi = 300)
ggsave("figures/calories_vs_heart_rate.png", fig_cal_hr,       width = 6, height = 4, dpi = 300)
ggsave("figures/correlation_heatmap.png",    fig_corr,         width = 6, height = 5, dpi = 300)
ggsave("figures/calories_by_sex.png",        fig_cal_sex,      width = 5, height = 4, dpi = 300)

saveRDS(train, "output/train_prepared.rds")
saveRDS(test,  "output/test_prepared.rds")

R.version.string

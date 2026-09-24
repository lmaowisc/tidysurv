# Tidy Survival Analysis - Chapter 2
# Run from the course project folder. Includes exercises and optional exports.

library(tidyverse)
library(lubridate)

trial <- tibble(
  id = 1:6,
  trt = c("A", "A", "B", "B", "A", "B"),
  age = c(65, 70, 58, 60, 64, 59),
  time = c(5, 8, 12, 3, 2, 6),
  status = c(1, 0, 1, 1, 0, 0)
)
trial

select(trial, id, trt, time, status)

trial_with_age <- mutate(
  trial,
  age_group = if_else(age >= 65, "65 or older", "Under 65")
)
trial_with_age

arm_a <- filter(trial_with_age, trt == "A")
arrange(arm_a, time)

arm_a_ordered <- trial |>
  mutate(age_group = if_else(age >= 65, "65 or older", "Under 65")) |>
  filter(trt == "A") |>
  arrange(time)
arm_a_ordered

trial |>
  group_by(trt) |>
  summarise(
    patients = n(),
    observed_events = sum(status == 1),
    median_observed_time = median(time),
    .groups = "drop"
  )

trial |>
  filter(trt == "B") |>
  summarise(patients = n(), observed_events = sum(status == 1))

dates_raw <- tibble(
  id = 1:3,
  start_date = c("2022-01-01", "2022-01-15", "2022-01-20"),
  end_date = c("2022-04-01", "2022-06-01", "2022-03-15"),
  outcome = c("dead", "censored", "dead")
)
dates_raw

dates_ready <- dates_raw |>
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date),
    time_days = as.numeric(end_date - start_date),
    event = case_when(
      outcome == "dead" ~ 1L,
      outcome == "censored" ~ 0L,
      TRUE ~ NA_integer_
    )
  )
dates_ready

dates_ready |>
  summarise(
    missing_time = sum(is.na(time_days)),
    missing_event = sum(is.na(event)),
    negative_time = sum(time_days < 0, na.rm = TRUE)
  )

recorded <- tibble(record = c("10", "32+", "23", "25+"))
parsed <- recorded |>
  mutate(
    time = parse_number(record),
    event = if_else(str_detect(record, fixed("+")), 0L, 1L)
  )
parsed

wide <- tibble(
  id = 1:3,
  prog_time = c(10, 20, 30),
  prog_status = c(1, 0, 1),
  death_time = c(15, 20, 35),
  death_status = c(0, 1, 1)
)
wide

long <- wide |>
  pivot_longer(
    cols = -id,
    names_to = c("endpoint", ".value"),
    names_sep = "_"
  )
long

long |>
  summarise(records = n(), people = n_distinct(id))
long |>
  group_by(endpoint) |>
  summarise(observed_events = sum(status == 1), .groups = "drop")

gbc_raw <- as_tibble(read.table("data/gbc.txt", header = TRUE))
gbc_raw |>
  select(id, time, status, hormone) |>
  slice_head(n = 6)

rfs <- gbc_raw |>
  arrange(id, time, status == 0) |>
  group_by(id) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    event = as.integer(status > 0),
    hormone = factor(hormone, levels = c(1, 2), labels = c("No", "Yes")),
    meno = factor(meno, levels = c(1, 2), labels = c("Pre", "Post")),
    grade = factor(grade, levels = c(1, 2, 3), labels = c("I", "II", "III"))
  )

rfs |>
  summarise(
    rows = n(),
    patients = n_distinct(id),
    events = sum(event),
    missing_time = sum(is.na(time)),
    missing_event = sum(is.na(event))
  )

rfs |>
  group_by(hormone) |>
  summarise(patients = n(), observed_events = sum(event), .groups = "drop")

rats <- tibble(
  id = 1:11,
  time = c(101, 55, 67, 23, 45, 98, 34, 77, 91, 104, 88),
  status = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1),
  group = c("A", "A", "A", "B", "B", "B", "A", "B", "B", "A", "B")
) |>
  mutate(
    outcome = factor(status, levels = c(0, 1),
                     labels = c("Censored", "Tumor development")),
    subject = reorder(factor(id), time)
  )

swimmer <- ggplot(rats, aes(x = time, y = subject)) +
  geom_segment(aes(x = 0, xend = time, yend = subject),
               colour = "#172d49", linewidth = 0.6) +
  geom_point(aes(shape = outcome), size = 3, colour = "#172d49")
swimmer

swimmer <- swimmer +
  scale_shape_manual(values = c("Censored" = 1, "Tumor development" = 16)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Days of follow-up", y = "Rat", shape = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.major.y = element_blank())
swimmer

swimmer + facet_wrap(~ group, scales = "free_y")

dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/follow-up.png", plot = swimmer,
       width = 7, height = 4.5, units = "in", dpi = 300,
       bg = "white")

patients <- as_tibble(read.table("data/gbc_mort.txt", header = TRUE)) |>
  mutate(
    hormone = factor(hormone, levels = c(1, 2), labels = c("No", "Yes")),
    meno = factor(meno, levels = c(1, 2), labels = c("Pre", "Post")),
    grade = factor(grade, levels = c(1, 2, 3), labels = c("I", "II", "III"))
  )
patients |>
  summarise(rows = n(), patients = n_distinct(id))

library(gtsummary)
table1 <- patients |>
  select(hormone, age, meno, grade, size, nodes) |>
  tbl_summary(
    by = hormone,
    type = list(c(age, size, nodes) ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      age ~ "Age (years)",
      meno ~ "Menopausal status",
      grade ~ "Tumor grade",
      size ~ "Tumor size (mm)",
      nodes ~ "Positive lymph nodes"
    ),
    missing = "ifany"
  )
table1

table1 |>
  add_overall() |>
  bold_labels()

table1_report <- table1 |>
  modify_header(label = "**Baseline characteristic**") |>
  modify_spanning_header(all_stat_cols() ~ "**Hormone therapy**") |>
  modify_caption("**Baseline characteristics of the 686 GBC patients**") |>
  bold_labels()
table1_report

dir.create("outputs", showWarnings = FALSE)
table1_report |>
  as_gt() |>
  gt::gtsave(filename = "outputs/baseline-table.html")

library(ggplot2)
library(MASS)
library(readr)
library(here)
library(dplyr)
library(ggpubr)
library(ggpomological)
library(gtsummary)
library(janitor)
library(openxlsx)
library(DescTools)

# load in data
wrca_frame <- read_csv(
  here("data", "wrca_audit.csv"),
  show_col_types = FALSE
  ) |>
  mutate(rep_verb = case_when(rep_verb == "reaad" ~ "read",
                              rep_verb == "NOT YET COMPLETE" ~ NA,
                              rep_verb == "complete" ~ NA,
                              rep_verb == "visit" ~ "read",
                              rep_verb == "view" ~ "read",
                              rep_verb == "research" ~ "read",
                              TRUE ~ rep_verb)) |>
  mutate(rep_object = case_when(rep_object == "assiggnment" ~ "assignment",
                                rep_object == "article" ~ "text",
                                rep_object == "document" ~ "text",
                                rep_object == "chapter" ~ "text",
                                rep_object == "images" ~ "image",
                                rep_object == "diagram" ~ "image",
                                rep_object == "interview" ~ "text",
                                rep_object == "museum" ~ "website",
                                rep_object == "musuem" ~ "website",
                                rep_object == "university" ~ "website",
                                rep_object == "videos" ~ "video",
                                rep_object == "chapter" ~ "text",
                                rep_object == "webpage" ~ "website",
                                TRUE ~ rep_object)) |>
  mutate(exp_verb = case_when(exp_verb == "comlpete" ~ "complete",
                              exp_verb == "?" ~ NA,
                              exp_verb == "revise" ~ "modify",
                              exp_verb == "solve" ~ "complete",
                              TRUE ~ exp_verb)) |>
  mutate(exp_object = case_when(exp_object == "collage (vision board)" ~ "collage",
                                exp_object == "definion" ~ "definition",
                                exp_object == "question" ~ "questions",
                                exp_object == "slides" ~ "presentation",
                                TRUE ~ exp_object)) |>
  mutate(dok = case_when(dok == "Assignment Missing?" ~ NA,
                              dok == "Not sure; can't see the full activity since it is a Google Form" ~ NA,
                              TRUE ~ dok)) |>
  arrange(course, lesson, sequence)# |>
  #na.omit()

wrca_rep_verb <- wrca_frame |>
  select(rep_verb) |>
  na.omit() |>
  count(rep_verb) |>
  mutate(percent = n/sum(n)*100) |>
  arrange(percent)
Desc(wrca_frame$rep_verb, plotit = TRUE) |> knitr::kable()
ggbarplot(
  wrca_rep_verb,
  x = "rep_verb",
  y = "percent",
  fill = "rep_verb"
)

wrca_rep_verb_course <- wrca_frame |>
  select(course, rep_verb) |>
  na.omit() |>
  group_by(course) |>
  count(rep_verb) |>
  mutate(percent = n/sum(n)*100) |>
  ungroup() |>
  arrange(percent)

ggbarplot(
  wrca_rep_verb_course,
  x = "rep_verb",
  y = "percent",
  facet.by = "course",
  fill = "rep_verb"
)

wrca_rep_object <- wrca_frame |>
  select(rep_object) |>
  na.omit() |>
  count(rep_object) |>
  mutate(percent = n/sum(n)*100) |>
  arrange(percent)

Desc(wrca_frame$rep_object, plotit = TRUE)

ggbarplot(
  wrca_rep_object,
  x = "rep_object",
  y = "percent",
  fill = "rep_object"
)

wrca_exp_verb <- wrca_frame |>
  select(exp_verb) |>
  na.omit() |>
  count(exp_verb) |>
  mutate(percent = n/sum(n)*100) |>
  arrange(percent)

Desc(wrca_frame$exp_verb, plotit = TRUE)

Desc(exp_verb ~ dok, wrca_frame, plotit = TRUE)

Desc(rep_verb ~ dok, wrca_frame, plotit = TRUE)

Desc(sequence ~ rep_verb, wrca_frame, plotit = TRUE)

Desc(sequence ~ exp_verb, wrca_frame, plotit = TRUE)

ggbarplot(
  wrca_exp_verb,
  x = "exp_verb",
  y = "n"
)

wrca_exp_object <- wrca_frame |>
  select(exp_object) |>
  na.omit() |>
  count(exp_object) |>
  mutate(percent = n/sum(n)*100) |>
  arrange(percent)

ggbarplot(
  wrca_exp_object,
  x = "exp_object",
  y = "percent"
)

wrca_dok <- wrca_frame |>
  select(dok) |>
  na.omit() |>
  count(dok) |>
  mutate(percent = n/sum(n)*100)

ggbarplot(
  wrca_dok,
  x = "dok",
  y = "percent",
  fill = "dok"
)

wrca_dok_course <- wrca_frame |>
  select(course, dok)# |>
  na.omit() |>
  group_by(course) |>
  count(dok) |>
  mutate(percent = n/sum(n)*100) |>
  ungroup() |>
  mutate(dok = as.numeric(dok))

wrca_dok_table <- wrca_dok_course |> tabyl(dok)

wrca_frame$dok <- as.integer(wrca_frame$dok)

Desc(wrca_frame$dok, plotit = TRUE)
dev.off()

ggbarplot(
  wrca_dok_course,
  x = "dok",
  y = "percent",
  facet.by = "course",
  fill = "dok"
)

wrca_lessons <- wrca_frame |>
  select(course, lesson) |>
  na.omit() |>
  group_by(course) |>
  filter(lesson == max(lesson)) |>
  ungroup() |>
  unique() |>
  arrange(lesson, course)

wrca_nl_table <- wrca_lessons |> tabyl(lesson) %>%
  #adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  #adorn_ns()# |>
knitr::kable()
Desc(wrca_lessons$lesson, plotit = TRUE)

write.xlsx(wrca_nl_table, here("output", "lessons-number.xlsx"))

ggbarplot(
  wrca_lessons,
  x = "course",
  y = "lesson"
)

# how many activities per lesson?
wrca_activities <- wrca_frame |>
  select(course, lesson, sequence) |>
  na.omit() |>
  group_by(course, lesson) |>
  filter(sequence == max(sequence)) |>
  ungroup() |>
  arrange(course, lesson)

wrca_na_table <- wrca_activities |> tabyl(sequence) %>%
  #adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  #adorn_ns()# |>
  knitr::kable()

write.xlsx(wrca_na_table, here("output", "activities-number.xlsx"))


ggboxplot(
  wrca_activities,
  x = "course",
  y = "sequence"
)

wrca_activities_total <- wrca_frame |>
  select(lesson, sequence) |>
  na.omit() |>
  group_by(lesson) |>
  filter(sequence == max(sequence)) |>
  ungroup()

summary(wrca_activities_total$sequence)

#Mode(wrca_activities_total$sequence)
Desc(wrca_activities$sequence, main = "Number of Activities Per Lesson", plotit = TRUE)
dev.off()

wrca_frame$sequence <- factor(wrca_frame$sequence,
                                 levels = c("1", "2", "3", "4", 
                                           "5", "6", "7", "8", "9", "10", "11"))
wrca_frame$dok <- factor(wrca_frame$dok,
                         levels = c("1", "2", "3", "4"))


Desc(sequence ~ dok, data=wrca_frame, digits=1, plotit = TRUE)
dev.off()

wrca_activities_sequence <- wrca_frame |>
  select(course, lesson, sequence, dok) |>
  na.omit() |>
#  group_by(lesson) |>
#  filter(sequence <= 5) |>
  unique() |>
  mutate(dok = as.numeric(dok)) |>
  mutate(sequence = as.ordered(sequence))# |>
wrca_ad_table <- wrca_activities_sequence |> tabyl(sequence, dok) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()# |>
  knitr::kable()

write.xlsx(wrca_ad_table, here("output", "sequence-dok.xlsx"))

wrca_rep_verb_sequence <- wrca_frame |>
  select(course, lesson, sequence, rep_verb) |>
  na.omit() |>
  #  group_by(lesson) |>
  #  filter(sequence <= 5) |>
  unique() |>
  #mutate(rep_verb = as.numeric(rep_verb)) |>
  mutate(sequence = as.ordered(sequence))# |>
wrca_rvs_table <- wrca_rep_verb_sequence |> tabyl(sequence, rep_verb) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() |>
knitr::kable()

write.xlsx(wrca_rvs_table, here("output", "sequence-rep_verb.xlsx"))

wrca_rep_object_sequence <- wrca_frame |>
  select(course, lesson, sequence, rep_object) |>
  na.omit() |>
  #  group_by(lesson) |>
  #  filter(sequence <= 5) |>
  unique() |>
  #mutate(rep_verb = as.numeric(rep_verb)) |>
  mutate(sequence = as.ordered(sequence))# |>
wrca_ros_table <- wrca_rep_object_sequence |> tabyl(sequence, rep_object) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() |>
  knitr::kable()

write.xlsx(wrca_ros_table, here("output", "sequence-rep_object.xlsx"))

wrca_exp_verb_sequence <- wrca_frame |>
  select(course, lesson, sequence, exp_verb) |>
  na.omit() |>
  #  group_by(lesson) |>
  #  filter(sequence <= 5) |>
  unique() |>
  #mutate(rep_verb = as.numeric(rep_verb)) |>
  mutate(sequence = as.ordered(sequence))# |>
wrca_evs_table <- wrca_exp_verb_sequence |> tabyl(sequence, exp_verb) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() |>
  knitr::kable()

write.xlsx(wrca_evs_table, here("output", "sequence-exp_verb.xlsx"))

wrca_exp_object_sequence <- wrca_frame |>
  select(course, lesson, sequence, exp_object) |>
  na.omit() |>
  #  group_by(lesson) |>
  #  filter(sequence <= 5) |>
  unique() |>
  #mutate(rep_verb = as.numeric(rep_verb)) |>
  mutate(sequence = as.ordered(sequence))# |>
wrca_eos_table <- wrca_exp_object_sequence |> tabyl(sequence, exp_object) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() |>
  knitr::kable()

write.xlsx(wrca_eos_table, here("output", "sequence-exp_object.xlsx"))

ggboxplot(
  wrca_activities_sequence,
  x = "sequence",
  y = "dok"
)

dok_1 <- wrca_activities_sequence |>
  filter(sequence == 1)
dok_1_median <- median(as.numeric(dok_1$dok))
dok_2 <- wrca_activities_sequence |>
  filter(sequence == 2)
dok_2_median <- median(as.numeric(dok_2$dok))
dok_3 <- wrca_activities_sequence |>
  filter(sequence == 3)
dok_3_median <- median(as.numeric(dok_3$dok))
dok_4 <- wrca_activities_sequence |>
  filter(sequence == 4)
dok_4_median <- median(as.numeric(dok_4$dok))
dok_5 <- wrca_activities_sequence |>
  filter(sequence == 5)
dok_5_stats <- data.frame(median(as.numeric(dok_5$dok))

# sequence and dok?

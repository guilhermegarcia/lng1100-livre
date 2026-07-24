library(tidyverse)
library(languageR)

# Data for LM question (q4)
d = read_csv("donnees/prepost.csv")

d |> glimpse()

d

aov(diff ~ L1 + Condition, data = d) |> summary()

ggplot(data = d, aes(x = Proficiency, y = PostTest-PreTest)) + 
  stat_summary() + 
  facet_grid(L1~Feedback)

set.seed(2)
d2 = tibble(ID = str_c("Learner_", seq(101, 150)),
            L1 = rep("Mandarin", times = 50),
            Proficiency = rep(c("Int", "Adv"), times = c(20, 30)),
            Sex = sample(x = c("Male", "Female"), size = 50, replace = TRUE),
            Feedback = c(rep(x = c("Explicit correction", "Recast"), each = 10),
                         rep(x = c("Explicit correction", "Recast"), each = 15)),
            Hours = sample(x = seq(1, 10), size = 50, replace = TRUE),
            PreTest = 50 + rnorm(50, mean = 10, sd = 2) |> round(2),
            DIFF = c(rnorm(n = 10, mean = 0, sd = 2.5),    # int exp
                     rnorm(n = 10, mean = 15, sd = 5),     # int rec
                     rnorm(n = 15, mean = 0, sd = 2.5),    # adv exp
                     rnorm(n = 15, mean = 18, sd = 5))) |> # adv rec 
  # rowwise() |> 
  mutate(PostTest = PreTest + DIFF,
         DelayedPostTest = NA)

ggplot(data = d2, aes(x = Proficiency, y = PostTest-PreTest)) + 
  stat_summary() + 
  facet_grid(~Feedback) + 
  geom_hline(yintercept = 0)

ggplot(data = d2, aes(x = Hours, y = DIFF)) + 
  geom_point() +
  facet_grid(Proficiency~Feedback) + 
  geom_smooth(method = "lm")


d = d |> 
  bind_rows(d2) |> 
  select(-c(DelayedPostTest, DIFF))

d

ggplot(data = d, aes(x = Proficiency, y = PostTest-PreTest)) + 
  stat_summary() + 
  facet_grid(L1~Feedback) + 
  geom_hline(yintercept = 0)

d |> glimpse()

d = d |> 
  mutate(PostTest = PostTest + Hours/2,
         diff = PostTest - PreTest)

fit = lm(diff ~ L1 + Feedback + Hours, data = d)

summary(fit)

library(emmeans)

emmeans(fit, pairwise ~ L1, adjust = "tukey")$contrasts


# Now change labels
# Learning vowel contrasts in French from three different L1s : Danish (M) - Portuguese (J) - Spanish (G)
# Pre and post test
# Treatment : phonetics component in course (recast) vs. no phonetics (explicit correction)

# Rename cols:
d = d |> 
  rename(Condition = Feedback,
         Pre = PreTest,
         Post = PostTest,
         Compétence = Proficiency)

d

# Rename levels:
d = d |> 
  mutate(L1 = case_when(L1 == "German" ~ "espagnol",
                        L1 == "Japanese" ~ "portugais",
                        L1 == "Mandarin" ~ "danois",
                        .default = NA),
         Condition = case_when(Condition == "Recast" ~ "phonétique",
                               Condition == "Explicit correction" ~ "traditionnelle",
                               .default = NA)) |> 
  mutate(across(where(is_character), as_factor)) |> 
  mutate(L1 = factor(L1, levels = c("danois", "portugais", "espagnol")))

d |> glimpse()

ggplot(data = d, aes(x = Compétence, y = Post-Pre)) + 
  stat_summary() + 
  facet_grid(L1~Condition) + 
  geom_hline(yintercept = 0, linetype = "dashed")


phon = d |> select(-diff)

save(phon, file = "donnees/phonetique.RData")



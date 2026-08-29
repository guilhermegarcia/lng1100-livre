library(languageR)
library(tidyverse)

data(english)

set.seed(1)
en = english |> 
  as_tibble() |> 
  select(Word, Familiarity, AgeSubject, RTlexdec) |> 
  rename(TR = "RTlexdec",
         Familiarite = "Familiarity",
         Age = "AgeSubject",
         Mot = "Word") |> 
  slice_sample(n = 2000)

en

# ggplot(data = en, aes(x = Age, y = TR)) + 
#   stat_summary()
# 
# ggplot(data = en, aes(x = Familiarite, y = TR)) + 
#   geom_point(aes(color = Age))


# write_csv(en, file = "donnees/base/anglais.csv")
# 

en = en |> 
  mutate(Fam_cat = ntile(Familiarite, 4))

en
fit1 = lm(TR ~ Age + Familiarite, data = en)

fit2 = lm(TR ~ Age + Fam_cat, data = en)


summary(fit1)
summary(fit2)
library(emmeans)

emmeans(fit, pairwise ~ Fam_cat, adjust = "tukey")$contrasts


newdata = expand.grid(Class = c("1st", "2nd", "3rd", "Crew"),
                              Sex = rep(c("Male", "Female")),
                              Age = c("Child", "Adult")) |> as_tibble()

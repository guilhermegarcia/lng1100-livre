library(tidyverse)
library(languageR)

data("regularity")

reg = regularity |> as_tibble()

# write_csv(reg, file = "~/Repos/LNG1100/donnees/base/auxiliaires.csv")

reg |> glimpse()

str(reg)

LM = lm(WrittenFrequency ~ Auxiliary, data = reg)
ANO = aov(WrittenFrequency ~ Auxiliary, data = reg)

summary(ANO)

summary(LM)

ANO |> TukeyHSD()

?regularity

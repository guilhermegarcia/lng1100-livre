rm(list = ls())

library(tidyverse)
villes = read_csv("donnees/villes2.csv")

# Nombre d'observations :
N = nrow(villes) # = le nombre de lignes dans le tableau

# Moyenne totalle X :
X = mean(villes$note)

# Nombre de groupes k :
k = 3 # trois villes

# Ajoutons la moyenne de chaque ville au tableau :
villes = villes |> 
  select(-duree) |> 
  mutate(moyenne = mean(note), .by = ville)

# Un tableau simples avec la moyenne de chaque ville
entreVilles = villes |> 
  summarize(moyenne = mean(note), 
            n = n(),
            .by = ville) |> 
  mutate(diff = moyenne - X,
         CE = diff^2,
         n_CE = n * CE)

entreVilles

SCEentre = entreVilles |> 
  summarize(SCEentre = sum(n_CE)) |> 
  pull() 

SCEentre

MSCEentre = SCEentre / (k-1)

SCEintra = villes |> 
  mutate(diff = note - moyenne) |> 
  mutate(CE = diff^2) |> 
  summarize(SCEintra = sum(CE)) |> 
  pull()

SCEintra

MSCEintra = SCEintra / (N - k)

valeurF = MSCEentre / MSCEintra

valeurF 

pf(valeurF, df1 = k-1, df2 = N-k, lower.tail = F)



# === FIGURE

set.seed(1)
villes = villes |> 
  mutate(moyenne = mean(note), .by = ville) |> 
  mutate(villeN = case_when(ville == "Calgary" ~ 1,
                            ville == "Montréal" ~ 2,
                            ville == "Québec" ~ 3,
                            .default = NA)) |> 
  rowwise() |> 
  mutate(vr = villeN + rnorm(n = 1, mean = 0, sd = 0.1)) |> 
  ungroup()

villes

ggplot(data = villes, aes(x = ville, y = note)) + 
  geom_jitter(alpha = 0.2, width = 0.1, size = 1) +
  stat_summary(geom = "line", aes(group = 1)) +
  stat_summary(fun = mean, color = "darkorange2", size = 1) +
  theme_classic() 


# ================ Review on anovas
data(mtcars)

library(languageR)

data("Titanic")

Titanic |> glimpse()

rm(list = ls())
library(tidyverse)
library(janitor) # pour simplifier les noms des colonnes

# Vérifier si le fichier csv est importé
# correctement :
q2 = read_csv2("donnees/supplementaires/questionnaire2.csv")

# 1. Simplifier les noms des colonnes
q2 = q2 |> 
  clean_names()

# 2. Enlever des colonnes inutiles
q2 = q2 |>
  dplyr::select(-matches("heure|adresse|nom|total|quiz|points|feedback"))

# 2. Transformer les colonnes des question (wide-to-long) 
long = q2 |> 
  pivot_longer(names_to = "question",
               values_to = "reponse",
               cols = 4:8)

# 3. Renommer quelques colonnes?
long = long |> 
  rename("geo" = 2,
         "langues" = 3)

# 4. Corriger le questionnaire (il faut avoir la clé de correction!)

# Il faut s'assurer que les réponses dans la clé sont IDENTIQUES aux
# réponses réelles dans le questionnaire :
cle = c("le dzongkha",
        "le catalan",
        "le néerlandais",
        "le swati et l'anglais",
        "le tétum et le portugais")

# long = long |> 
#   group_by(id) |> 
#   mutate(cle = cle) |> 
#   ungroup()

long = long |> 
  mutate(cle = cle, .by = id)

# IMPORTANT : l'ordre de cle et l'ordre des questions sont les mêmes!!!
# Sinon, il faut utiliser une autre méthode (..._join())

# Maintenant, on corrige/compare les deux colonnes pertinentes :
long = long |> 
  mutate(correct = if_else(reponse == cle, 1, 0)) |> 
  dplyr::select(-cle)


# ALTERNATIVE :
# Une version concise de la colonne question :
long = long |> 
  mutate(q = question |> str_sub(start = 1, end = 7) |> str_c("...")) |> 
  dplyr::select(-question) |>
  dplyr::select(id, geo, langues, q, reponse, correct)

# ALTERNATIVE :
# Une version numérotée de la colonne question :
long = long |> 
  mutate(q = str_c("question_", row_number()), .by = id)

# Vous pouvez toujours retourner aux versions 
# précédentes dans le script.

# Finalement, on vérifie les classes des colonnes :
# glimpse(long, width = 50)

# Changer quelques classes (selon le besoin) :
long = long |> 
  mutate(id = as_factor(id),
         across(where(is_character),
                as_factor))

# write_csv(long, file = "~/Desktop/q2_net.csv")

library(dplyr)
library(tidyverse)
library(lubridate)
library(rdflib)
library(purrr)
library(stringr)
library(ontologyIndex)
library(fuzzyjoin)
library(stringdist)
library(rlang)
library(tidyr)
library(data.tree)


hierarchy <- read.csv(
  "prelim_hierarchy.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

names(hierarchy) <- make.names(names(hierarchy), unique = TRUE)

# Now safe to mutate
hierarchy <- hierarchy %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~iconv(., from = "", to = "UTF-8", sub = ""))) %>%
  mutate(across(everything(), ~na_if(trimws(.), ""))) %>% 
  rename(
    level1 = X,
    level2 = X.1,
    level3 = X.2,
    level4 = X.3
  ) %>%
  dplyr::select(level1, level2, level3, level4)

df_filled <- hierarchy %>%
  fill(level1) %>%
  group_by(level1) %>%
  fill(level2) %>%
  group_by(level1, level2) %>%
  fill(level3) %>%
  ungroup() %>%
  mutate(across(
    c(level1, level2, level3, level4),
    ~gsub("/", "_", .)
  ))

df_filled$pathString <- apply(
  df_filled[, c("level1", "level2", "level3", "level4")],
  1,
  function(x) paste(c("Root", x[!is.na(x) & x != ""]), collapse = "/")
)

df_paths <- df_filled %>%
  distinct(pathString)

tree <- as.Node(df_paths)
print(tree)


df_lookup <- ToDataFrameTree(
  tree,
  "name",
  "pathString"
)

df_lookup <- df_lookup %>%
  tidyr::separate(
    pathString,
    into = c("Root", "level1", "level2", "level3", "level4"),
    sep = "/",
    fill = "right"
  )

dictionary <- df_lookup %>%
  filter(!is.na(level2)) %>% 
  transmute(
    raw_term = level4,
    category = level3,
    group = level2,
    domain = level1
  )

write.csv(dictionary, "hierarchy_df.csv", row.names = FALSE)

library(ggplot2)
library(dplyr)
library(patchwork)
library(ggforce)
library(scales)
library(RColorBrewer)
library(moonBook)
library(plotly)
library(colorspace)

my_cols <- c(
  "#88CCEE", "#CC6677", "#DDCC77", "#117733",
  "#332288", "#AA4499", "#44AA99", "#999933",
  "#882255", "#661100", "#6699CC", "#888888"
)

mycols <- rcartocolor::carto_pal(name = "Safe")

df <- read.csv("Coded_Methods_hierarchy.csv")

forsunburst <- df %>%
  dplyr::select(Analysis_std, Analysis_category, Analysis_group) %>%
  count(Analysis_std, Analysis_category, Analysis_group) %>%
  na.omit()


analysis_clean <- forsunburst %>%
  mutate(
    Analysis_group = recode(
      Analysis_group,
      "statistical inference" = "Inference",
      "Descriptive_Statistics" = "Descriptive",
      "Multivariate_Analysis" = "Multivariate",
      "Economic_Analysis" = "Economic"
    ),
    Analysis_category = recode(
      Analysis_category,
      "Hypothesis_Testing" = "Hypothesis Test",
      "Regression_Predictive_Modeling" = "Regression",
      "Classification_clustering" = "Classification / Clustering",
      "Correlation_Association" = "Correlation / Association",
      "Rank_choice" = "Rank Choice",
      "Rank_Index" = "Rank Index",
      "Dimension_Reduction" = "Dimension Reduction",
      "Cross_Tabulation" = "Cross Tabulation",
      "Deviations_ratio" = "Deviations Ratio",
      "Cost-benefit_ratios" = "Cost-Benefit Ratios",
      "Partial_budgeting" = "Partial Budgeting",
      "Model_Evaluation" = "Model Evaluation",
      "Stability Analysis" = "Stability Analysis",
      "Frequency" = "Frequency",
      "Mean" = "Mean",
      "Pivot_Table" = "Pivot Table"
    ),
    Analysis_std = recode(
      Analysis_std,
      "ANOVA" = "ANOVA",
      "Chi-Square Analysis" = "Chi-Square",
      "Cluster Analysis" = "Cluster Analysis",
      "Cochran's q" = "Cochran's Q",
      "Conjoint Analysis" = "Conjoint Analysis",
      "Correlation Analysis" = "Correlation Analysis",
      "Correspondence Analysis" = "Correspondence Analysis",
      "Cross Tabulation" = "Cross Tabulation",
      "Cross-Validation" = "Cross-Validation",
      "Deviations Ratio" = "Deviations Ratio",
      "Diversity Index" = "Diversity Index",
      "F-test" = "F-Test",
      "Fisher's LSD" = "Fisher's LSD",
      "Friedman Test" = "Friedman Test",
      "Garrett Ranking Test" = "Garrett Ranking",
      "Kendall Correlation/Concordance" = "Kendall Correlation",
      "Kruskal-Wallis Test" = "Kruskal-Wallis",
      "LSD" = "LSD",
      "Latent Class Model" = "Latent Class Model",
      "Logistic Regression" = "Logistic",
      "Mann-Whitney Test" = "Mann-Whitney",
      "Newman-Keuls" = "Newman-Keuls",
      "OLS Regression" = "OLS Regression",
      "Pair-wise rankings" = "Pairwise Ranking",
      "Pearson Correlation" = "Pearson Correlation",
      "Principal Component Analysis" = "PCA",
      "Principal Coordinates Analysis" = "PCoA",
      "Probit Model" = "Probit Model",
      "Qualitative/Thematic Analysis" = "Qualitative Analysis",
      "Random Utility" = "Random Utility Model",
      "Regression" = "Regression",
      "Spearman Correlation" = "Spearman Correlation",
      "Stability analysis" = "Stability Analysis",
      "Tobit model" = "Tobit Model",
      "Tukey's HSD Test" = "Tukey HSD",
      "Wald Test" = "Wald Test",
      "Wilcoxon Test" = "Wilcoxon Test",
      "cost-benefit ratios" = "Cost-Benefit Analysis",
      "dominance analysis" = "Dominance Analysis",
      "marginal analysis" = "Marginal Analysis",
      "partial budgeting" = "Partial Budgeting",
      "t-test" = "t-Test",
      "Pivot_Table" = "Pivot Table",
      "Rank index" = "Rank Index"
    )
  )

analysis_clean <- analysis_clean %>%
  mutate(
    Analysis_std = ifelse(
      is.na(Analysis_std) | Analysis_std == Analysis_category,
      NA_character_,
      Analysis_std
    )
  )




# -----------------------------
# Group base colors (level 1)
# -----------------------------
group_color_map <- c(
  "Inference"    = "#2471A3",  
  "Descriptive"  = "#7D3C98",  
  "Multivariate" = "#117733",   
  "Economic"     = "#882255"
)

# -----------------------------
# Category colors (level 2) - same family, mid shade
# -----------------------------
category_color_map <- c(
  # Inference family - reds/warm
  "Hypothesis Test"           = "#6699CC",
  "Regression"                = "slategray2", 
  "Rank Choice"               = "cornflowerblue",
  "Correlation / Association" = "#AED6F1",
  "Classification / Clustering" = "#2E86C1", 
  "Model Evaluation"          = "royalblue3", 
  
  # Descriptive family - blues
  "Frequency"                 = "orchid4", 
  "Cross Tabulation"          = "#D2B4DE",
  "Deviations Ratio"          = "darkmagenta",   
  "Mean"                      = "#A569BD",   
  "Pivot Table"               = "plum2",   
  "Rank Index"                = "blueviolet",   
  
  # Multivariate family - greens
  "Dimension Reduction"       = "#52BE80",   # medium green
  "Stability Analysis"        = "#A9DFBF",   # light green
  
  # Economic family - purples
  "Cost-Benefit Ratios"       = "violetred4",   # medium purple
  "Partial Budgeting"         = "maroon3"    # light purple
)

# -----------------------------
# Build hierarchy
# -----------------------------
lvl1 <- analysis_clean %>%
  group_by(Analysis_group) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("group__", Analysis_group),
    labels    = Analysis_group,
    parents   = "",
    group_key = Analysis_group,
    color     = unname(group_color_map[Analysis_group])
  )

lvl2 <- analysis_clean %>%
  group_by(Analysis_group, Analysis_category) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("cat__", Analysis_group, "__", Analysis_category),
    labels    = Analysis_category,
    parents   = paste0("group__", Analysis_group),
    group_key = Analysis_group,
    color     = unname(category_color_map[Analysis_category])
  )

lvl3 <- analysis_clean %>%
  filter(!is.na(Analysis_std)) %>%
  group_by(Analysis_group, Analysis_category, Analysis_std) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("std__", Analysis_group, "__", Analysis_category, "__", Analysis_std),
    labels    = Analysis_std,
    parents   = paste0("cat__", Analysis_group, "__", Analysis_category),
    group_key = Analysis_group,
    # lighten the category color for level 3
    color     = colorspace::lighten(unname(category_color_map[Analysis_category]), 0.45)
  )

sunburst_df <- bind_rows(lvl1, lvl2, lvl3) %>%
  mutate(
    level = case_when(
      grepl("^group__", ids) ~ 1L,
      grepl("^cat__",   ids) ~ 2L,
      grepl("^std__",   ids) ~ 3L
    )
  )

# -----------------------------
# Plot
# -----------------------------
plot_ly(
  ids      = sunburst_df$ids,
  labels   = sunburst_df$labels,
  parents  = sunburst_df$parents,
  values   = sunburst_df$values,
  type     = "sunburst",
  branchvalues = "total",
  marker   = list(
    colors = sunburst_df$color,
    line   = list(color = "white", width = 1)
  ),
  textinfo = "label",
  insidetextorientation = "auto",
  hovertemplate = paste(
    "<b>%{label}</b><br>",
    "Count: %{value}<br>",
    "Percent of parent: %{percentParent:.1%}<br>",
    "Percent of total: %{percentRoot:.1%}",
    "<extra></extra>"
  )
) %>%
  layout(margin = list(t = 20, l = 20, r = 20, b = 20))


# Traits
library(readxl)

df <- read_excel("Cleaned_Preferences.xlsx")

forsunburst <- df %>%
  dplyr::select(Trait_std, TraitGroup_category, TraitGroup_group) %>%
  count(Trait_std, TraitGroup_category, TraitGroup_group) %>%
  na.omit()


trait_clean <- forsunburst %>%
  mutate(
    TraitGroup_group = recode(
      TraitGroup_group,
      "sensory" = "Sensory",
      "end_use_quality" = "End-Use Quality",
      "composition_nutritional" = "Composition",
      "postharvest_handling" = "Postharvest Handling",
      "grain_morphology" = "Grain Morphology"
    )
    # ,
    # TraitGroup_category = recode(
    #   TraitGroup_category,
    #   "Pivot_Table" = "Pivot Table"
    # ),
    # Trait_std = recode(
    #   Trait_std,
    #   "Rank index" = "Rank Index"
    # )
  )

trait_clean2 <- trait_clean %>%
  mutate(
    Trait_std = ifelse(
      is.na(Trait_std) | Trait_std == TraitGroup_category,
      NA_character_,
      Trait_std
    )
  )


# -----------------------------
# Group base colors (level 1)
# -----------------------------
group_color_map <- c(
  "Composition"    = "#2471A3",  
  "End-Use Quality"  = "#882255",  
  "Sensory" = "#DDCC77",   
  "Grain Morphology"     = "#7D3C98",
  "Postharvest Handling" = "#117733"
)

# -----------------------------
# Category colors (level 2) - same family, mid shade
# -----------------------------
# 
# view <- trait_clean2 %>% 
#   filter(TraitGroup_group == "Postharvest Handling")
# 
# unique(view$TraitGroup_category)

category_color_map <- c(
 
  "carbohydrate_content"           = "#6699CC",
  "digestibility"                = "slategray2", 
  "fiber"               = "cornflowerblue",
  "iron" = "#AED6F1",
  "moisture" = "#2E86C1",
  "nutritional_quality"          = "royalblue3",
  "protein_content" = "skyblue",      
  "starch" = "slateblue4",                
  "sugar_content" = "dodgerblue",         
  "tannins" = "dodgerblue4",  

 
  "endosperm_texture"                 = "orchid4", 
  "grain_color"          = "#D2B4DE",
  "grain_length"          = "darkmagenta",   
  "grain_shape"                      = "#A569BD",
  "grain_size"               = "plum2",
  "hardness"                = "darkviolet",
  "hilum"                = "darkorchid2",
  "testa"                = "darkorchid4",
  "texture"                = "mediumpurple3",
  "weight"                = "mediumorchid2",
  "morphology"                = "mediumpurple4",
  
  
  "cleaning_ability"       = "#52BE80",  
  "decortication"        = "#A9DFBF",  
  "germination" = "#117733",
  "grinding_ability" = "forestgreen",
  "milling_quality" = "green3",
  "processing_quality" = "darkgreen",
  "shattering" = "palegreen",
  "solubility" = "seagreen4",
  "storability" = "palegreen3",
  "water_holding_capacity" = "seagreen",
  
    
  "food_quality"       = "violetred4",   
  "beverage_quality"         = "maroon2",    
  "brewing_quality"          = "deeppink3",
  
  "aroma"       = "khaki1",  
  "taste"        = "gold2",  
  "food_elasticity" = "lightgoldenrod2"
  
)

# -----------------------------
# Build hierarchy
# -----------------------------
lvl1 <- trait_clean2 %>%
  group_by(TraitGroup_group) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("group__", TraitGroup_group),
    labels    = TraitGroup_group,
    parents   = "",
    group_key = TraitGroup_group,
    color     = unname(group_color_map[TraitGroup_group])
  )

lvl2 <- trait_clean2 %>%
  group_by(TraitGroup_group, TraitGroup_category) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("cat__", TraitGroup_group, "__", TraitGroup_category),
    labels    = TraitGroup_category,
    parents   = paste0("group__", TraitGroup_group),
    group_key = TraitGroup_group,
    color     = unname(category_color_map[TraitGroup_category])
  )

lvl3 <- trait_clean %>%
  filter(!is.na(Trait_std)) %>%
  group_by(TraitGroup_group, TraitGroup_category, Trait_std) %>%
  summarise(values = sum(n), .groups = "drop") %>%
  mutate(
    ids       = paste0("std__", TraitGroup_group, "__", TraitGroup_category, "__", Trait_std),
    labels    = Trait_std,
    parents   = paste0("cat__", TraitGroup_group, "__", TraitGroup_category),
    group_key = TraitGroup_group,
    # lighten the category color for level 3
    color     = colorspace::lighten(unname(category_color_map[TraitGroup_category]), 0.45)
  )

sunburst_df <- bind_rows(lvl1, lvl2, lvl3) %>%
  mutate(
    level = case_when(
      grepl("^group__", ids) ~ 1L,
      grepl("^cat__",   ids) ~ 2L,
      grepl("^std__",   ids) ~ 3L
    )
  )

# -----------------------------
# Plot
# -----------------------------

sunburst_df <- sunburst_df %>%
  mutate(
    plot_label = case_when(
      level == 3 & is.na(Trait_std) ~ "",
      TRUE ~ labels
    )
  )

plot_ly(
  ids      = sunburst_df$ids,
  labels   = sunburst_df$labels,
  parents  = sunburst_df$parents,
  values   = sunburst_df$values,
  type     = "sunburst",
  branchvalues = "total",
  marker   = list(
    colors = sunburst_df$color,
    line   = list(color = "white", width = 1)
  ),
  textinfo = "label",
  insidetextorientation = "auto",
  hovertemplate = paste(
    "<b>%{label}</b><br>",
    "Count: %{value}<br>",
    "Percent of parent: %{percentParent:.1%}<br>",
    "Percent of total: %{percentRoot:.1%}",
    "<extra></extra>"
  )
) 
# %>%
#   layout(margin = list(t = 20, l = 20, r = 20, b = 20))

sunburst_df_plot <- sunburst_df %>%
  filter(!is.na(Trait_std))

plot_ly(
  ids      = sunburst_df_plot$ids,
  labels   = sunburst_df_plot$labels,
  parents  = sunburst_df_plot$parents,
  values   = sunburst_df_plot$values,
  type     = "sunburst",
  branchvalues = "total",
  marker   = list(
    colors = sunburst_df_plot$color,
    line   = list(color = "white", width = 1)
  ),
  textinfo = "label"
)

sunburst_df <- sunburst_df %>%
  mutate(
    plot_label = case_when(
      level == 3 & labels == TraitGroup_category ~ "",
      level == 3 & is.na(Trait_std) ~ "",
      TRUE ~ labels
    )
  )

plot_ly(
  ids      = sunburst_df$ids,
  labels   = sunburst_df$labels,
  text     = sunburst_df$plot_label,
  parents  = sunburst_df$parents,
  values   = sunburst_df$values,
  type     = "sunburst",
  branchvalues = "total",
  marker   = list(
    colors = sunburst_df$color,
    line   = list(color = "white", width = 1)
  ),
  textinfo = "text",
  insidetextorientation = "auto",
  hovertemplate = paste(
    "<b>%{label}</b><br>",
    "Count: %{value}<br>",
    "Percent of parent: %{percentParent:.1%}<br>",
    "Percent of total: %{percentRoot:.1%}",
    "<extra></extra>"
  )
)

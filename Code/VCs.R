source("Code/Functions.R")

df <- read_csv("Extraction_Export_Raw.csv")

long_df <- df %>%
  rename(StudyID = `Covidence #`) %>%
  pivot_longer(
    cols = matches("^\\d+\\s"),
    names_to = c("entry_id", "field"),
    names_pattern = "(\\d+)\\s(.+)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = field,
    values_from = value
  )

fortraits <- long_df %>%
  filter(if_any(c(Trait, Preference), ~ !is.na(.) & . != "")) %>%
  dplyr::select(-c(DOI, `Reviewer Name`, Title...6, `Journal/Publisher`, `Location(s)`, `List any specific scales or methods named`, 
                   `Detailed preferences discussed in the study:`, `Time of study`, `Publication Date`, Statistic)) %>%
  rename(Title = Title...3, Methods = `How were preferences determined?`, Analysis = `How was data analyzed?`, 
         Software = `Was a statistical software used?`, VCsGeneral = `Value chain actors included`, 
         TraitsGeneral = `Grain quality traits included`, CropsGeneral = `Crop(s) studied`, EndUse = `End uses mentioned`,
         DisaggregationGeneral = `Disaggregating variables`, Citation = `Study ID`, StudySize = `Size of study/respondents`) %>%
  mutate(Country = gsub("Ethopia", "Ethiopia", Country)) %>%
  mutate(across(c(Crop, Location, `VC Actor`, Trait, Preference), ~ str_trim(as.character(.))))

write_csv(fortraits, "CleanExtractionData.csv")


subdf <- Cleaned_Preferences %>% 
  filter(TraitGroup_group == "composition_nutrition")


vcs <- df %>% 
  dplyr::select(c(StudyID, Methods, VCsGeneral)) %>%
  separate_rows(Methods, sep = "; ") %>%
  separate_rows(VCsGeneral, sep = ",") %>%
  separate_rows(VCsGeneral, sep = "; ")
 

variable1 <-  "Methods"
variable2 <-  "VCsGeneral"

vcscompare <- make_pair_summary(
  data = vcs,
  variable1 = "Methods",
  variable2 = "VCsGeneral"
)

vcsplot <- vcs %>%
  clean_domain(
    col_name = "Methods",
    domain_name = "Method",
    std_dict_path = "Dictionaries/methods_dictionary.csv",
    hierarchy_path = "hierarchy_df2.csv",
    out_col = "Methods_clean_std",
    prefix = "Methods",
    max_dist = 0.10
  ) %>% 
  mutate(
    Methods_category = ifelse(is.na(Methods_category), "Other", Methods_category),
    Methods_group = ifelse(is.na(Methods_group), "Other", Methods_group)
  ) %>% 
  mutate(VCsGeneral = case_when(
    grepl("Other:", VCsGeneral, ignore.case = TRUE) ~ "Other",
    TRUE ~ VCsGeneral
  )) 

ggplot(vcsplot, aes(x = VCsGeneral, y = n, fill = Methods_category)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = longcols) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

method_actor_df <- vcsplot %>%
  distinct(StudyID, VCsGeneral, Methods_category) %>%
  filter(
    !is.na(VCsGeneral),
    !is.na(Methods_category)
  )

overall_methods <- method_actor_df %>%
  distinct(StudyID, Methods_category) %>%
  count(Methods_category, name = "n_studies") %>%
  mutate(
    percent = round(100 * n_studies / sum(n_studies), 1)
  ) %>%
  arrange(desc(n_studies))

overall_methods

actor_method_summary <- method_actor_df %>%
  count(VCsGeneral, Methods_category, name = "n") %>%
  group_by(VCsGeneral) %>%
  mutate(
    total_actor = sum(n),
    percent_within_actor = round(100 * n / total_actor, 1)
  ) %>%
  ungroup() %>%
  arrange(VCsGeneral, desc(n))


top_method_by_actor <- actor_method_summary %>%
  group_by(VCsGeneral) %>%
  slice_max(n, n = 3, with_ties = FALSE) %>%
  ungroup()

top_method_by_actor



clean_actor <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("/", "_") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "_") %>%
    str_squish()
}

myvcs <- fortraits %>% 
  dplyr::select(c(StudyID, `VC Actor`)) %>%
  rename(VCs = `VC Actor`) %>%
  separate_rows(VCs, sep = ", ") %>%
  separate_rows(VCs, sep = "; ") %>% 
  distinct(StudyID, VCs)

vcsclean <- myvcs %>%
  clean_domain(
    col_name = "VCs",
    domain_name = "ValueChainActor",
    std_dict_path = "Dictionaries/vc_dictionary.csv",
    hierarchy_path = "hierarchy_df2.csv",
    out_col = "VCs_clean_std",
    prefix = "VCs",
    max_dist = 0.10
  ) %>% 
  dplyr::select(StudyID, VCs, VCs_term)
  

vcsanalysis <- vcsclean %>% distinct(StudyID, VCs_term)

genvcs <- fortraits %>% 
  dplyr::select(c(StudyID, VCsGeneral)) %>%
  separate_rows(VCsGeneral, sep = ", ") %>%
  separate_rows(VCsGeneral, sep = "; ") %>% 
  distinct(StudyID, VCsGeneral)

fullvcs <- genvcs %>% 
  left_join(vcsclean, by = "StudyID")


check_vcs <- fullvcs %>%
  mutate(
    VCsGeneral_clean = clean_actor(VCsGeneral),
    VCs_term_clean = clean_actor(VCs_term)
  ) %>%
  group_by(StudyID) %>%
  mutate(
    has_farmer_general = any(str_detect(VCsGeneral_clean, "farmer|grower")),
    has_consumer_general = any(str_detect(VCsGeneral_clean, "consumer")),
    has_brewer_general = any(str_detect(VCsGeneral_clean, "brewer")),
    has_chef_general = any(str_detect(VCsGeneral_clean, "chef|home_cook|cook")),
    
    term_present_in_general = case_when(
      VCs_term_clean == "farmer_grower" ~ has_farmer_general,
      VCs_term_clean == "consumer" ~ has_consumer_general,
      VCs_term_clean == "brewer" ~ has_brewer_general,
      VCs_term_clean == "chef_home_cook" ~ has_chef_general,
      TRUE ~ NA
    )
  ) %>%
  ungroup()

vccounts <- table(vcsanalysis$VCs_term)
vccounts/131






df <- read.csv("CleanExtractionData.csv")

disag <- df %>% 
  dplyr::select(c(StudyID, Methods, DisaggregationGeneral)) %>%
  separate_rows(DisaggregationGeneral, sep = "; ") %>% 
  distinct(StudyID, DisaggregationGeneral) %>%
  count(DisaggregationGeneral, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(percent = round(100 * n / sum(n), 1))

CleanExtractionData <- read_csv("CleanExtractionData.csv")

disagdf <- CleanExtractionData[,c(1,17,18,26)] %>% 
  distinct(StudyID, DisaggregationGeneral, Disaggregation, `Explanation for any disaggregation`) %>% 
  filter(DisaggregationGeneral != "None reported") %>% 
  dplyr::select(-DisaggregationGeneral) %>% 
  filter(!is.na(Disaggregation) | !is.na(`Explanation for any disaggregation`))

view <- CleanExtractionData %>% 
  filter(!is.na(`Explanation for any disaggregation`) & grepl("Location", DisaggregationGeneral, ignore.case = TRUE)) %>% 
  distinct(StudyID, Country, `Explanation for any disaggregation`)

mydisag <- CleanExtractionData %>% 
  dplyr::select(StudyID, DisaggregationGeneral, Disaggregation, `Explanation for any disaggregation`, `Key findings`, Trait)


journals <- df %>% dplyr::select("Journal/Publisher", "Study ID")
journaldictionary <- read.csv("Dictionaries/journal_dictionary.csv")


journal_std <- standardize_with_dictionary(
    data = journals,
    col_name = "Journal/Publisher",
    dict = journaldictionary,
    suffix = "_std"
  )

journal_counts <- journal_std %>%
  count(`Journal/Publisher_std`, sort = TRUE)

journal_counts


nofarmcons <- df %>%
  filter(!grepl("Farmer/Grower|Consumer", `Value chain actors included`, ignore.case = TRUE)) %>%
  dplyr::select(`Study ID`, `Value chain actors included`)

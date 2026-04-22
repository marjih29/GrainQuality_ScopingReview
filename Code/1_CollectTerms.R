df <- read_csv("Extraction_Export_Raw.csv")

long_df <- df %>%
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

fordictionary <- long_df %>% 
  filter(if_any(c(Trait, Preference), ~ !is.na(.) & . != "")) %>% 
  mutate(across(c(Crop, Location, `VC Actor`, Trait, Preference), ~ str_trim(as.character(.)))) %>% 
  rename(Methods = `How were preferences determined?`, Analysis = `How was data analyzed?`, Software = `Was a statistical software used?`) %>%
  separate_rows(Country, sep = ", ") %>% 
  separate_rows(`VC Actor`, sep = ", ") %>%
  separate_rows(`VC Actor`, sep = " ; ") %>%
  separate_rows(Crop, sep = ", ") %>%
  separate_rows(Crop, sep = " and ") %>%
  separate_rows(Methods, sep = "; ") %>% 
  separate_rows(Analysis, sep = ", ") %>%
  separate_rows(Software, sep = "; ") %>%
  mutate(Location = gsub("^(and|also)\\s+", "", Location)) %>% 
  dplyr::select(Country, Crop, `VC Actor`, Trait, Preference, Methods, Analysis, `Publication Type`, 
                `Type of study`, Software) %>% 
  pivot_longer(cols = c(Crop, `VC Actor`, Trait, Preference, Methods, Analysis, `Publication Type`, 
                        `Type of study`, Software, Country), names_to = "variable", values_to = "standard")
  
  
  
  write.csv(fordictionary, "rawdictionary.csv", row.names = FALSE)
  
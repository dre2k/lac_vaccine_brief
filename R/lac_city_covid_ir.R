# let's redo this so that you combine unincorporated with cities 
# (combine Incidence rates like you did with los angeles neighborhoods)


# ---------- read IR data ------
# anyway, take the IR rate, combined Los Angeles neighborhoods so I can plot using LA Almanac info
tmp1 <- fread("data/LA_County_Covid19_CSA_14day_case_death_table.csv", 
              col.names = c("nrow", "Date", "city", "cases_14day", "case_14day_rate", 
                            "adj_case_14day_rate", "case_rate_unstable", "death_14day",
                            "death_14day_rate", "adj_death_14day_rate", "death_rate_unstable", "population")) %>%
  dplyr::select(-contains('unstable'), -nrow, -cases_14day, -case_14day_rate, -death_14day, -death_14day_rate) %>% 
  dplyr::filter(!is.na(city), 
                population > 1000) %>%
  dplyr::mutate(Date = ymd(Date),
                across(adj_case_14day_rate:population, ~ as.numeric(.x)), 
                city = gsub("City of ", "", city), 
                city = gsub("Unincorporated - ", "", city))

tmp0 <- data.frame(table(tmp1$city))


# For combining LA, essentially calculating aggregate rate weighted by neighborhood populations
tmp2 <- tmp1 %>% 
  dplyr::filter(grepl("Los Angeles - ", city)) %>% 
  dplyr::group_by(Date) %>% 
  dplyr::mutate(case_counts = (population / 100000) * adj_case_14day_rate,
                death_counts = (population / 100000) * adj_death_14day_rate) %>% 
  dplyr::select(-(contains("14day"))) %>% 
  dplyr::summarise(across(.cols = population:death_counts, sum, na.rm = T)) %>% ungroup() %>% 
  dplyr::mutate(adj_case_14day_rate = (case_counts / population) * 100000, 
                adj_death_14day_rate = (death_counts / population) * 100000,
                city = "Los Angeles") %>% 
  dplyr::select(-case_counts, -death_counts)


# combine city + unincorporated areas with same name
# (same way you did with Los Angeles city)
tmp1b <- tmp1 %>% 
  dplyr::filter(!grepl("Los Angeles - ", city)) %>%
  group_by(Date) %>% 
  dplyr::mutate(case_counts = (population / 100000) * adj_case_14day_rate, 
                death_counts = (population / 100000) * adj_death_14day_rate) %>% ungroup() %>%
  group_by(Date, city) %>% 
  dplyr::summarise(across(c(population, case_counts, death_counts), sum, na.rm = T)) %>% ungroup() %>% 
  dplyr::mutate(adj_case_14day_rate = (case_counts / population) * 100000, 
                adj_death_14day_rate = (death_counts / population) * 100000)



lac_summarytable_dash <- bind_rows(tmp1b, tmp2) %>%
  dplyr::mutate(city = case_when(city == "Covina (Charter Oak)" ~ "Charter Oak", 
                                 city == "Northeast San Gabriel" ~ "East San Gabriel", # need to make sure this is true
                                 city == "Athens-Westmont" ~ "Westmont", 
                                 city == "Florence-Firestone" ~ "Florence-Graham",
                                 TRUE ~ city))

# check again
tmp0 <- data.frame(table(lac_summarytable_dash$city))

# output
write.csv(lac_summarytable_dash, quote = F, file = "data/LA_County_Covid19_CSA_14day_case_death_table_Clean.csv")



rm(tmp0, tmp1, tmp2, tmp3, tmp1b)

# careful - adult means 12+, so it's really ALL (columns changed in data from dashboard)
vac_colnames <- c("dose1_date", "city", 
                  "dose1_all", "dose1_all_c", "dose1_all_c_prcent",
                  "dose1_youth", "dose1_youth_c", "dose1_youth_c_prcent", 
                  "dose1_senior", "dose1_senior_c", "dose1_senior_c_prcent",
                  "pop_all", "pop_youth", "pop_senior")

vaccinetmp <- read.csv("data/LAC_Vaccine_City_Data.csv", col.names = vac_colnames) %>%
  dplyr::filter(pop_all > 1000) %>% 
  dplyr::mutate(dose1_date = mdy(dose1_date), 
                across(.cols = contains("pop"), as.numeric))

# identify cities that have all unreliable data for 'all' vaccinations
# let's filter them if they have > 100 days of reliable data
tmp0 <- vaccinetmp %>% 
  group_by(city) %>% 
  summarise(counts = sum(grepl("Unreliable", dose1_all_c_prcent))) %>% 
  filter(counts > 100)

vaccinetmp <- vaccinetmp %>% 
  filter(!city %in% tmp0$city) %>% 
  dplyr::select(-contains(c('youth', 'senior')))


# todo
# 1) combined los angeles into a single city. for unreliable data, fill in with info from previous day
# 2) then sum dose1 for each age group
# 3) calculate cumulative doses + % vaccinated
# 4) merge back into overall data
# edit - only include 'all'
tmp1 <- vaccinetmp %>% 
  dplyr::filter(!grepl("Los Angeles - ", city)) %>% 
  group_by(city) %>% 
  dplyr::mutate(across(.cols = c("dose1_all"), ~ as.numeric(ifelse(.x == "Unreliable Data", 0, .x))), 
                across(.cols = c("dose1_all_c"), ~ as.numeric(zoo::na.locf(ifelse(.x == "Unreliable Data", NA, .x)))), 
                across(.cols = c("dose1_all_c_prcent"), ~ zoo::na.locf(ifelse(.x == "Unreliable Data", NA, .x)))) %>% 
  ungroup()

tmp2 <- vaccinetmp %>% 
  dplyr::filter(grepl("Los Angeles - ", city)) %>% 
  dplyr::group_by(city) %>% 
  dplyr::mutate(across(.cols = c("dose1_all"), ~ as.numeric(ifelse(.x == "Unreliable Data", 0, .x))), 
                across(.cols = c("dose1_all_c"), ~ as.numeric(zoo::na.locf(ifelse(.x == "Unreliable Data", NA, .x))))) %>% 
  ungroup()

tmp2b <- tmp2 %>% 
  dplyr::select(-contains('prcent')) %>% 
  dplyr::group_by(dose1_date) %>% 
  dplyr::summarise(across(dose1_all:pop_all, sum, na.rm = T)) %>% ungroup() %>% 
  dplyr::mutate(city = "Los Angeles", 
                dose1_all_c_prcent = paste0(round( (dose1_all_c / pop_all)*100, 0), "%"))

# which cities have both city and unincorporated
vaccine <- bind_rows(tmp1, tmp2b) %>% 
  mutate(city = gsub("City of ", "", city), 
         city = gsub("Unincorporated - ", "", city))

tmp0 <- data.frame(table(vaccine$city))


# remove prefixes, combine, recalculate percentages
# takes a while
vaccine <- bind_rows(tmp1, tmp2b) %>% 
  mutate(city = gsub("City of ", "", city), 
         city = gsub("Unincorporated - ", "", city)) %>% 
  # change some names to match almanac, then combine all by date/city
  mutate(city = case_when(city == "Athens-Westmont" ~ "Westmont", 
                          city == "Covina (Charter Oak)" ~ "Charter Oak", 
                          city == "Florence-Firestone" ~ "Florence-Graham", 
                          city == "Northeast San Gabriel" ~ "East San Gabriel", 
                          TRUE ~ city)) %>% 
  dplyr::select(-contains('prcent')) %>% 
  group_by(dose1_date, city) %>% 
  dplyr::summarise(across(c(dose1_all, dose1_all_c, pop_all), sum, na.rm = T)) %>% ungroup() %>%
  dplyr::mutate(dose1_all_c_prcent = round( (dose1_all_c / pop_all)*100, 1))

# check again
tmp0 <- data.frame(table(vaccine$city))

# output
write.csv(vaccine, quote = F, file = "data/LAC_Vaccine_City_Data_Clean.csv")

rm(tmp0, tmp1, tmp2, tmp2b, tmp3, vaccinetmp, vac_colnames)

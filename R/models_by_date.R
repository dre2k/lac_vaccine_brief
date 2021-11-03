library(tidyverse)
library(data.table)
library(glue)
library(lubridate)

# vaccine data
# source("R/lac_city_vaccine.R")
vaccine <- read.csv("data/LAC_Vaccine_City_Data_Clean.csv")

# IR data 
# source("R/lac_city_covid_ir.R")
lac_summarytable_dash <- read.csv("data/LA_County_Covid19_CSA_14day_case_death_table_Clean.csv")


# almanac etc ----

# January surge incidence rate to include in models:
january.date <- "2021-01-07"

# almanac data
d.race <- read.table("data/almanac_race_by_city.csv", header=T, sep=",") %>% 
  mutate(CityHispanic.cat = relevel(as.factor(ifelse(Hispanic > quantile(Hispanic, probs=.75), "High",
                                              ifelse(Hispanic < quantile(Hispanic, probs=.25), "Low",
                                                     "Med"))), ref="Med"), 
         CityBlack.cat = relevel(as.factor(ifelse(Black > 10, "High","Low")), "Low"),
         CityAsian.cat = relevel(as.factor(ifelse(Asian > quantile(Asian, probs=.75), "High",
                                                  ifelse(Asian < quantile(Asian, probs=.5), "Low",
                                                         "Med"))), ref="Low"))


d.age <- read.table("data/almanac_age_group_by_city.csv", header=T, sep=",")
Age <- d.age[,c("Age.Under.15","Age.15.17","Age.18.24","Age.25.34","Age.35.54","Age.55.64","Age.65.")]
Age <- t(apply(Age, 1, FUN=function(v) { v/sum(v) }))
d.age <- d.age %>% 
  mutate(CityAgeYoung = apply(Age[,c("Age.Under.15","Age.15.17","Age.18.24")], 1, sum), 
         CityAgeOld = apply(Age[,c("Age.55.64","Age.65.")], 1, sum), 
         CityAgeOld.cat = relevel(as.factor(ifelse(CityAgeOld > quantile(CityAgeOld, probs=.75), "High","notHigh")), "notHigh"), 
         CityAgeYoung.cat = relevel(as.factor(ifelse(CityAgeYoung > quantile(CityAgeYoung, probs=.75), "High","notHigh")), "notHigh"))



d.income <- read.table("data/city_income.csv", header=T, sep=",")
d.income$city <- unlist(lapply(d.income$city, FUN=function(v) { strsplit(v, "†") }))
d.income <- d.income %>% 
  mutate(HouseholdIncome.cat =  relevel(as.factor(ifelse(Households > quantile(Households, probs=.75), "High",
                                                         ifelse(Households < quantile(Households, probs=.25), "Low",
                                                                "Med"))), ref="Med"))

d.ir.jan <- lac_summarytable_dash[lac_summarytable_dash$Date==january.date,]

d <- inner_join(lac_summarytable_dash, d.ir.jan, by = 'city')

# introduce lag time between vaccine date and IR date
# play around with this
d.vax <- vaccine %>% 
  mutate(Date.x = dose1_date)

d <- inner_join(d, d.vax, by = c('city', 'Date.x'))
d <- inner_join(d, d.race, by = 'city')
d <- inner_join(d, d.age, by = 'city')
d <- inner_join(d, d.income, by = 'city')


# data clean ----

wtf <- filter(d, Date.x == "2021-08-18") %>% 
  mutate(Vac.percent = as.numeric(dose1_all_c_prcent), 
         Vax.cat = relevel(as.factor(ifelse(Vac.percent > quantile(Vac.percent, probs=.75), "High",
                                         ifelse(Vac.percent < quantile(Vac.percent, probs=.25), "Low",
                                                "Med"))), ref="Med"),
         Jan.IR =  adj_case_14day_rate.y, 
         Jan.IR.cat =  relevel(as.factor(ifelse(Jan.IR > quantile(Jan.IR, probs=.75), "High",
                                                ifelse(Jan.IR < quantile(Jan.IR, probs=.25), "Low",
                                                       "Med"))), ref="Med"))
hist(wtf$Vac.percent)
table(wtf$Vax.cat)

model1 <- lm(adj_case_14day_rate.x ~ Vax.cat + Jan.IR.cat + HouseholdIncome.cat+CityHispanic.cat+CityBlack.cat+ CityAsian.cat+ CityAgeOld.cat+ CityAgeYoung.cat, data = wtf)
summary(model1)

dout <- d %>% 
  mutate(Vax.percent = dose1_all_c_prcent, 
         Jan.IR =  adj_case_14day_rate.y, 
         Jan.IR.cat =  relevel(as.factor(ifelse(Jan.IR > quantile(Jan.IR, probs=.75), "High",
                                                ifelse(Jan.IR < quantile(Jan.IR, probs=.25), "Low",
                                                       "Med"))), ref="Med")) %>%
  dplyr::select(Date.x, city, adj_case_14day_rate.x, 
                Vax.percent, Vax.Jan.IR.cat, 
                HouseholdIncome.cat, CityHispanic.cat, CityBlack.cat, 
                CityAsian.cat, CityAgeOld.cat, CityAgeYoung.cat)



# dout <- d %>% 
#   mutate(Vax.percent = as.numeric(dose1_all_c_prcent), 
#          Vax.cat = relevel(as.factor(ifelse(Vac.percent > quantile(Vac.percent, probs=.75), "High",
#                                             ifelse(Vac.percent < quantile(Vac.percent, probs=.25), "Low",
#                                                    "Med"))), ref="Med"),
#          Jan.IR =  adj_case_14day_rate.y, 
#          Jan.IR.cat =  relevel(as.factor(ifelse(Jan.IR > quantile(Jan.IR, probs=.75), "High",
#                                                 ifelse(Jan.IR < quantile(Jan.IR, probs=.25), "Low",
#                                                        "Med"))), ref="Med")) %>% 
#   dplyr::select(Date.x, city, adj_case_14day_rate.x, 
#                 Vax.percent, Jan.IR.cat, 
#                 HouseholdIncome.cat, CityHispanic.cat, CityBlack.cat, 
#                 CityAsian.cat, CityAgeOld.cat, CityAgeYoung.cat)





dout <- d %>% 
  cbind(CityAge) %>% 
  # to create variables based on quantiles, need to group by Date.x
  # need to decide - do i want that as a definition ... or just a universal cutoff
  group_by(Date.x)
  mutate(Vax.percent = as.numeric(gsub("%", "", dose1_adult_c_prcent)),
         # Vax.percent = as.numeric(gsub("%", "", dose1_adult_c_prcent)),
         Vax.cat = case_when(Vax.percent > quantile(Vax.percent, probs = 0.75) ~ "High", 
                             Vax.percent < quantile(Vax.percent, probs = 0.25) ~ "Low", 
                             TRUE ~ "Med")) %>% 
  filter(Date.x == "2021-08-18")

table(dout$Vax.cat)



         Vax.cat = relevel(as.factor(ifelse(Vax.percent > quantile(Vax.percent, probs=.75), "High",
                                            ifelse(Vax.percent < quantile(Vax.percent, probs=.25), "Low",
                                                   "Med"))), ref="Med")) 
         Jan.IR =  adj_case_14day_rate.y, 
         Jan.IR.cat =  relevel(as.factor(ifelse(Jan.IR > quantile(Jan.IR, probs=.75), "High",
                                                ifelse(Jan.IR < quantile(Jan.IR, probs=.25), "Low",
                                                       "Med"))), ref="Med"), 
         HouseholdIncome.cat =  relevel(as.factor(ifelse(Households > quantile(Households, probs=.75), "High",
                                                         ifelse(Households < quantile(Households, probs=.25), "Low",
                                                                "Med"))), ref="Med"),
         CityHispanic.cat = relevel(as.factor(ifelse(Hispanic > quantile(Hispanic, probs=.75), "High",
                                                     ifelse(Hispanic < quantile(Hispanic, probs=.25), "Low",
                                                            "Med"))), ref="Med"),
         CityBlack.cat = relevel(as.factor(ifelse(Black > 10, "High","Low")), "Low"),
         CityAsian.cat = relevel(as.factor(ifelse(Asian > quantile(Asian, probs=.75), "High",
                                                  ifelse(Asian < quantile(Asian, probs=.5), "Low",
                                                         "Med"))), ref="Low"), 
         CityAgeOld.cat = relevel(as.factor(ifelse(CityAgeOld > quantile(CityAgeOld, probs=.75), "High","notHigh")), "notHigh"), 
         CityAgeYoung.cat = relevel(as.factor(ifelse(CityAgeYoung > quantile(CityAgeYoung, probs=.75), "High","notHigh")), "notHigh")) %>% 
  dplyr::select(Date.x, city, adj_case_14day_rate.x, Vax.cat, Vax.percent, Jan.IR.cat, HouseholdIncome.cat, CityHispanic.cat, CityBlack.cat, CityAsian.cat, CityAgeOld.cat, CityAgeYoung.cat)

head(dout)


# fit models ----

# make sure you get exact same answer as david ... 

model1 <- lm(adj_case_14day_rate.x ~ Vax.cat + Jan.IR.cat + HouseholdIncome.cat+CityHispanic.cat+CityBlack.cat+ CityAsian.cat+ CityAgeOld.cat+ CityAgeYoung.cat, data = dout %>% filter(Date.x == "2021-08-18"))
summary(model1)

tmp <- dout %>% 
  filter(Date.x == "2021-08-18")
table(tmp$Vax.cat)

model2 <- lm(adj_case_14day_rate.x ~ Vax.cat, data = dout %>% filter(Date.x == "2021-08-18"))
summary(model2)

test <- dout %>% 
  # dplyr::filter(Date.x > "2021-03-01") %>% 
  tidyr::nest(data = -Date.x) %>% 
  dplyr::mutate(fit = purrr::map(data, ~lm(adj_case_14day_rate.x ~ Vax.percent + Jan.IR.cat + HouseholdIncome.cat+CityHispanic.cat+CityBlack.cat+ CityAsian.cat+ CityAgeOld.cat+ CityAgeYoung.cat, data = .x)),
                tidied = purrr::map(fit, ~broom::tidy(.x), quality = purrr::map(fit, ~glance(.x)))) %>% 
  dplyr::select(-data, -fit) %>% 
  tidyr::unnest(tidied) %>% 
  # dplyr::filter(grepl("Vax", term)) %>%
  arrange(Date.x)


# Vaccination rates ---- 
# for vax, let's prioritize after april .. 

p1 <- ggplot(test %>% filter(grepl("Vax.percent", term), Date.x > "2021-04-01"), aes(Date.x, estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
  geom_hline(yintercept = 0) + theme_bw() + theme(legend.position = c(0.8, 0.8))
  # geom_line(data = lac_summarytable_dash %>% filter(city == "Los Angeles County"), aes(Date, adj_case_14day_rate))

p2 <- ggplot(lac_summarytable_dash %>% filter(city == "Los Angeles County", Date > "2021-04-01"), aes(Date, adj_case_14day_rate)) +
  geom_line()

plot_grid(p1, p2, ncol = 1, align = 'v')
ggsave(plot = last_plot(), width = 10, height = 8, file = "~/Desktop/lac_full_model_vax.png")




p1 <- ggplot(test %>% filter(term %in% c("CityAsian.catHigh", "CityBlack.catHigh", "CityHispanic.catHigh")), aes(Date.x, estimate)) + 
  geom_point(aes(colour = term)) + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error, colour = term)) +
  geom_hline(yintercept = 0) + theme_bw() + theme(legend.position = c(0.8, 0.8))
# geom_line(data = lac_summarytable_dash %>% filter(city == "Los Angeles County"), aes(Date, adj_case_14day_rate))
# ggsave(p1, width = 10, height = 8, file = "~/Desktop/test1.png")

p2 <- ggplot(lac_summarytable_dash %>% filter(city == "Los Angeles County"), aes(Date, adj_case_14day_rate)) +
  geom_line()


ggsave(p1, width = 10, height = 8, file = "~/Desktop/test1.png")


plot_grid(p1, p2, ncol = 1, align = 'v')





p1 <- ggplot(test %>% filter(term %in% c("HouseholdIncome.catHigh", "HouseholdIncome.catLow")), aes(Date.x, estimate)) + 
  geom_point(aes(colour = term)) + 
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error, colour = term)) +
  geom_hline(yintercept = 0) + theme_bw() + theme(legend.position = c(0.8, 0.8))
# geom_line(data = lac_summarytable_dash %>% filter(city == "Los Angeles County"), aes(Date, adj_case_14day_rate))
# ggsave(p1, width = 10, height = 8, file = "~/Desktop/test1.png")





# original model
reg.Aug <- lm(Aug.IR ~ Aug.Vax.cat + Jan.IR.cat + HouseholdIncome.cat+CityRaceEthnicty.m+CityAge.m)
summary(reg.Aug)

reg.Aug2 <- lm(Aug.IR ~ Aug.Vax.cat + HouseholdIncome.cat+CityRaceEthnicty.m+CityAge.m)
summary(reg.Aug2)



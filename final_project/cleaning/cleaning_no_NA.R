library(readr)
library(tidyverse)
library(dplyr)
#library(janitor)
setwd("~/pstat/131/pstat131/final_project_edited")
VOTER_Survey_December16_Release1 <- read_csv("archive/VOTER_Survey_December16_Release1.csv")
# only taking data from 2016 survey respondent 
cleaned_data_2016 <- VOTER_Survey_December16_Release1 %>%
  dplyr::select(ends_with("2016")) %>%
  dplyr::select(starts_with(c("izip_2016","presvote16post_","ft","accurately_counted2","alcohol","smoke100","pid7","ideo","pew_religimp","starttime","endtime"))) %>%
  dplyr::select(-c("presvote16post_t_2016","presvote16post_rnd_2016")) %>%
  #mutate(izip_2016 = as.numeric(izip_2016))  # makes zip code numeric

# Replacing NA values
cleaned_data_2016 <- cleaned_data_2016 %>%
  replace_na(list(ft_white_2016 = '2000')) %>%
  replace_na(list(ft_black_2016 = '2000')) %>%
  replace_na(list(ft_hisp_2016 = '2000')) %>%
  replace_na(list(ft_asian_2016 = '2000')) %>%
  replace_na(list(ft_muslim_2016 = '2000')) %>%
  replace_na(list(ft_jew_2016 = '2000')) %>%
  replace_na(list(ft_christ_2016 = '2000')) %>%
  replace_na(list(ft_fem_2016 = '2000')) %>%
  replace_na(list(ft_immig_2016 = '2000')) %>% 
  replace_na(list(ft_blm_2016 = '2000')) %>%
  replace_na(list(ft_wallst_2016 = '2000')) %>%
  replace_na(list(ft_gays_2016 = '2000')) %>%
  replace_na(list(ft_unions_2016 = '2000')) %>%
  replace_na(list(ft_police_2016 = '2000')) %>%
  replace_na(list(ft_police_2016 = '2000')) %>%
  replace_na(list(ft_altright_2016 = '2000'))

# 2000 is for NA
# 4000 is for "Don't Know" 
#impute values

# Re-coding White
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_white_2016 <- recode(cleaned_data_2016$ft_white_2016, "Don't know" = "4000")

# Re-coding Black
cleaned_data_2016$ft_black_2016 <- recode(cleaned_data_2016$ft_black_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_black_2016 <- recode(cleaned_data_2016$ft_black_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_black_2016 <- recode(cleaned_data_2016$ft_black_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_black_2016 <- recode(cleaned_data_2016$ft_black_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_black_2016<- recode(cleaned_data_2016$ft_black_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_black_2016 <- recode(cleaned_data_2016$ft_black_2016, "Don't know" = "4000")


# Re-coding Hispanic
#cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "100 - Favorable feeling" = "100")
#cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "75 - Favorable feeling" = "75")
#cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "25 -Unfavorable feeling" = "25")
#cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "50 - No feeling at all" = "50")
#cleaned_data_2016$ft_hisp_2016<- recode(cleaned_data_2016$ft_hisp_2016, "0 - Unfavorable feeling" = "0")
#cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "Don't know" = "4000")

# Re-coding Hispanic
cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_hisp_2016<- recode(cleaned_data_2016$ft_hisp_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_hisp_2016 <- recode(cleaned_data_2016$ft_hisp_2016, "Don't know" = "4000")

# Re-coding Asian
cleaned_data_2016$ft_asian_2016 <- recode(cleaned_data_2016$ft_asian_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_asian_2016 <- recode(cleaned_data_2016$ft_asian_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_asian_2016 <- recode(cleaned_data_2016$ft_asian_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_asian_2016 <- recode(cleaned_data_2016$ft_asian_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_asian_2016<- recode(cleaned_data_2016$ft_asian_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_asian_2016 <- recode(cleaned_data_2016$ft_asian_2016, "Don't know" = "4000")

# Re-coding Muslim
cleaned_data_2016$ft_muslim_2016 <- recode(cleaned_data_2016$ft_muslim_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_muslim_2016 <- recode(cleaned_data_2016$ft_muslim_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_muslim_2016 <- recode(cleaned_data_2016$ft_muslim_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_muslim_2016 <- recode(cleaned_data_2016$ft_muslim_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_muslim_2016<- recode(cleaned_data_2016$ft_muslim_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_muslim_2016 <- recode(cleaned_data_2016$ft_muslim_2016, "Don't know" = "4000")

# Re-coding Jew
cleaned_data_2016$ft_jew_2016 <- recode(cleaned_data_2016$ft_jew_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_jew_2016 <- recode(cleaned_data_2016$ft_jew_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_jew_2016 <- recode(cleaned_data_2016$ft_jew_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_jew_2016 <- recode(cleaned_data_2016$ft_jew_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_jew_2016<- recode(cleaned_data_2016$ft_jew_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_jew_2016 <- recode(cleaned_data_2016$ft_jew_2016, "Don't know" = "4000")

# Re-coding Christ
cleaned_data_2016$ft_christ_2016 <- recode(cleaned_data_2016$ft_christ_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_christ_2016 <- recode(cleaned_data_2016$ft_christ_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_christ_2016 <- recode(cleaned_data_2016$ft_christ_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_christ_2016 <- recode(cleaned_data_2016$ft_christ_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_christ_2016<- recode(cleaned_data_2016$ft_christ_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_christ_2016 <- recode(cleaned_data_2016$ft_christ_2016, "Don't know" = "4000")

# Re-coding Fem
cleaned_data_2016$ft_fem_2016 <- recode(cleaned_data_2016$ft_fem_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_fem_2016 <- recode(cleaned_data_2016$ft_fem_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_fem_2016 <- recode(cleaned_data_2016$ft_fem_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_fem_2016 <- recode(cleaned_data_2016$ft_fem_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_fem_2016<- recode(cleaned_data_2016$ft_fem_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_fem_2016 <- recode(cleaned_data_2016$ft_fem_2016, "Don't know" = "4000")

# Re-coding Immigrant 
cleaned_data_2016$ft_immig_2016 <- recode(cleaned_data_2016$ft_immig_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_immig_2016 <- recode(cleaned_data_2016$ft_immig_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_immig_2016 <- recode(cleaned_data_2016$ft_immig_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_immig_2016 <- recode(cleaned_data_2016$ft_immig_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_immig_2016<- recode(cleaned_data_2016$ft_immig_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_immig_2016 <- recode(cleaned_data_2016$ft_immig_2016, "Don't know" = "4000")

# Re-coding BLM 
cleaned_data_2016$ft_blm_2016 <- recode(cleaned_data_2016$ft_blm_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_blm_2016 <- recode(cleaned_data_2016$ft_blm_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_blm_2016 <- recode(cleaned_data_2016$ft_blm_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_blm_2016 <- recode(cleaned_data_2016$ft_blm_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_blm_2016<- recode(cleaned_data_2016$ft_blm_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_blm_2016 <- recode(cleaned_data_2016$ft_blm_2016, "Don't know" = "4000")

# Re-coding Wallstreet 
cleaned_data_2016$ft_wallst_2016 <- recode(cleaned_data_2016$ft_wallst_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_wallst_2016 <- recode(cleaned_data_2016$ft_wallst_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_wallst_2016 <- recode(cleaned_data_2016$ft_wallst_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_wallst_2016 <- recode(cleaned_data_2016$ft_wallst_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_wallst_2016<- recode(cleaned_data_2016$ft_wallst_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_wallst_2016 <- recode(cleaned_data_2016$ft_wallst_2016, "Don't know" = "4000")

# Re-coding Gays
cleaned_data_2016$ft_gays_2016 <- recode(cleaned_data_2016$ft_gays_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_gays_2016 <- recode(cleaned_data_2016$ft_gays_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_gays_2016 <- recode(cleaned_data_2016$ft_gays_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_gays_2016 <- recode(cleaned_data_2016$ft_gays_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_gays_2016<- recode(cleaned_data_2016$ft_gays_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_gays_2016 <- recode(cleaned_data_2016$ft_gays_2016, "Don't know" = "4000")

# Re-coding unions
cleaned_data_2016$ft_unions_2016 <- recode(cleaned_data_2016$ft_unions_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_unions_2016 <- recode(cleaned_data_2016$ft_unions_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_unions_2016 <- recode(cleaned_data_2016$ft_unions_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_unions_2016 <- recode(cleaned_data_2016$ft_unions_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_unions_2016<- recode(cleaned_data_2016$ft_unions_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_unions_2016 <- recode(cleaned_data_2016$ft_unions_2016, "Don't know" = "4000")

# Re-coding Police
cleaned_data_2016$ft_police_2016 <- recode(cleaned_data_2016$ft_police_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_police_2016 <- recode(cleaned_data_2016$ft_police_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_police_2016 <- recode(cleaned_data_2016$ft_police_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_police_2016 <- recode(cleaned_data_2016$ft_police_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_police_2016<- recode(cleaned_data_2016$ft_police_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_police_2016 <- recode(cleaned_data_2016$ft_police_2016, "Don't know" = "4000")

# Re-coding Alt-right
cleaned_data_2016$ft_altright_2016 <- recode(cleaned_data_2016$ft_altright_2016, "100 - Favorable feeling" = "100")
cleaned_data_2016$ft_altright_2016 <- recode(cleaned_data_2016$ft_altright_2016, "75 - Favorable feeling" = "75")
cleaned_data_2016$ft_altright_2016 <- recode(cleaned_data_2016$ft_altright_2016, "25 -Unfavorable feeling" = "25")
cleaned_data_2016$ft_altright_2016 <- recode(cleaned_data_2016$ft_altright_2016, "50 - No feeling at all" = "50")
cleaned_data_2016$ft_altright_2016<- recode(cleaned_data_2016$ft_altright_2016, "0 - Unfavorable feeling" = "0")
cleaned_data_2016$ft_altright_2016 <- recode(cleaned_data_2016$ft_altright_2016, "Don't know" = "4000")

# changing all of the strings to integers
cleaned_data_2016 <- cleaned_data_2016 %>%
  mutate(ft_black_2016 = as.numeric(ft_black_2016)) %>%
  mutate(ft_white_2016 = as.numeric(ft_white_2016)) %>%
  mutate(ft_hisp_2016 = as.numeric(ft_hisp_2016)) %>%
  mutate(ft_asian_2016 = as.numeric(ft_asian_2016)) %>%
  mutate(ft_muslim_2016 = as.numeric(ft_muslim_2016)) %>%
  mutate(ft_jew_2016 = as.numeric(ft_jew_2016)) %>%
  mutate(ft_christ_2016 = as.numeric(ft_christ_2016)) %>%
  mutate(ft_fem_2016 = as.numeric(ft_fem_2016)) %>%
  mutate(ft_immig_2016 = as.numeric(ft_immig_2016)) %>%
  mutate(ft_blm_2016 = as.numeric(ft_blm_2016)) %>%
  mutate(ft_wallst_2016 = as.numeric(ft_wallst_2016)) %>%
  mutate(ft_gays_2016 = as.numeric(ft_gays_2016)) %>%
  mutate(ft_unions_2016 = as.numeric(ft_unions_2016)) %>%
  mutate(ft_police_2016 = as.numeric(ft_police_2016)) %>%
  mutate(ft_altright_2016 = as.numeric(ft_altright_2016))
  
# putting the NA's back in for "NA' and "Don't Know"
cleaned_data_2016 <- cleaned_data_2016 %>%
  mutate(ft_black_2016 = replace(ft_black_2016, ft_black_2016 == 4000, NA)) %>% #black
  mutate(ft_black_2016 = replace(ft_black_2016, ft_black_2016 == 2000, NA)) %>%
  mutate(ft_white_2016 = replace(ft_white_2016, ft_white_2016 == 4000, NA)) %>%# white
  mutate(ft_white_2016 = replace(ft_white_2016, ft_white_2016 == 2000, NA)) %>%
  mutate(ft_hisp_2016 = replace(ft_hisp_2016,ft_hisp_2016 == 4000, NA)) %>% #hisp
  mutate(ft_hisp_2016 = replace(ft_hisp_2016, ft_hisp_2016 == 2000, NA)) %>%
  mutate(ft_asian_2016 = replace(ft_asian_2016, ft_asian_2016 == 4000, NA)) %>% #asian
  mutate(ft_asian_2016 = replace(ft_asian_2016, ft_asian_2016 == 2000, NA)) %>%
  mutate(ft_christ_2016 = replace(ft_christ_2016, ft_christ_2016 == 4000, NA)) %>% #christian
  mutate(ft_christ_2016 = replace(ft_christ_2016, ft_christ_2016 == 2000, NA)) %>%
  mutate(ft_immig_2016 = replace(ft_immig_2016, ft_immig_2016 == 4000, NA)) %>% # immigrant
  mutate(ft_immig_2016 = replace(ft_immig_2016, ft_immig_2016 == 2000, NA)) %>%
  mutate(ft_wallst_2016 = replace(ft_wallst_2016, ft_wallst_2016 == 4000, NA)) %>% # wall street
  mutate(ft_wallst_2016 = replace(ft_wallst_2016, ft_wallst_2016 == 2000, NA)) %>%
  mutate(ft_unions_2016 = replace(ft_unions_2016, ft_unions_2016 == 4000, NA)) %>% # unions
  mutate(ft_unions_2016 = replace(ft_unions_2016, ft_unions_2016 == 2000, NA)) %>%
  mutate(ft_muslim_2016 = replace(ft_muslim_2016, ft_muslim_2016 == 4000, NA)) %>% # muslim 
  mutate(ft_muslim_2016 = replace(ft_muslim_2016, ft_muslim_2016 == 2000, NA)) %>%
  mutate(ft_jew_2016 = replace(ft_jew_2016, ft_jew_2016 == 4000, NA)) %>%  # jews
  mutate(ft_jew_2016 = replace(ft_jew_2016, ft_jew_2016 == 2000, NA)) %>%  
  mutate(ft_fem_2016 = replace(ft_fem_2016, ft_fem_2016 == 4000, NA)) %>%  # feminist
  mutate(ft_fem_2016 = replace(ft_fem_2016, ft_fem_2016 == 2000, NA)) %>%  
  mutate(ft_blm_2016 = replace(ft_blm_2016, ft_blm_2016 == 4000, NA)) %>%  #BLM
  mutate(ft_blm_2016 = replace(ft_blm_2016, ft_blm_2016 == 2000, NA)) %>%
  mutate(ft_gays_2016 = replace(ft_gays_2016, ft_gays_2016 == 4000, NA)) %>% #gays
  mutate(ft_gays_2016 = replace(ft_gays_2016, ft_gays_2016 == 2000, NA)) %>%
  mutate(ft_police_2016 = replace(ft_police_2016, ft_police_2016 == 4000, NA)) %>% #police
  mutate(ft_police_2016 = replace(ft_police_2016, ft_police_2016 == 2000, NA)) %>% 
  mutate(ft_altright_2016 = replace(ft_altright_2016, ft_altright_2016 == 4000, NA)) %>% # alt- right
  mutate(ft_altright_2016 = replace(ft_altright_2016, ft_altright_2016 == 2000, NA)) 
  
#Robust check
#cleaned_data_2016_testing <- VOTER_Survey_December16_Release1 %>%
  #dplyr::select(ends_with("2016")) %>%
  #dplyr::select(starts_with(c("izip_2016","presvote16post_","ft","accurately_counted2","alcohol","smoke100","pid7","ideo","pew_religimp","starttime","endtime"))) %>%
  #dplyr::select(-c("presvote16post_t_2016","presvote16post_rnd_2016")) %>%
  #mutate(izip_2016 = as.numeric(izip_2016))

#cleaned_data_2016 %>%
  #filter(row_number()==45)

#cleaned_data_2016_testing %>%
  #filter(row_number()==45)


cleaned_data_2016 <- cleaned_data_2016 %>%
  mutate(presvote16post_2016 = replace(presvote16post_2016, presvote16post_2016 == "Evan McMullin", "Other")) %>% #black
  mutate(presvote16post_2016 = replace(presvote16post_2016, presvote16post_2016 == "Gary Johnson", "Other")) %>% #black
  mutate(presvote16post_2016 = replace(presvote16post_2016, presvote16post_2016 == "Jill Stein", "Other")) #black
  
  #mutate(ft_black_2016 = replace(ft_black_2016, ft_black_2016 == 2000, NA)) %>%
  #mutate(ft_white_2016 = replace(ft_white_2016, ft_white_2016 == 4000, NA)) %>%# white

# Let's see the unique options for who someone votes for
cleaned_data_2016 %>%
  group_by(presvote16post_2016) %>%
  summarise(n = n())

# Let's remove NA
#cleaned_data_2016_no_NA <- cleaned_data_2016 %>%
  #filter(presvote16post_2016 %in% c("Did not vote for President","Donald Trump","Evan McMullin","Gary Johnson","Hillary Clinton","Jill Stein","Other"))

cleaned_data_2016 <- cleaned_data_2016 %>%
  filter(presvote16post_2016 %in% c("Donald Trump","Hillary Clinton","Other",NA))



no_na_final <- cleaned_data_2016 %>%
  na.omit()

grouping <- no_na_no_evan %>%
  group_by(presvote16post_2016)

write_csv(no_na_final, file = "drafts/data/no_na_final.csv")
# Let's double check that it worked
##cleaned_data_2016_no_NA %>%
  #group_by(presvote16post_2016) %>%
  #summarise(n = n())

# Let's look at our other variables
# accurately counted # has NA
#cleaned_data_2016_no_NA %>%
  #group_by(accurately_counted2_2016) %>%
  #summarise(n = n())

# alcohol_2016 # has NA
#cleaned_data_2016_no_NA %>%
  #group_by(alcohol_2016) %>%
  #summarise(n = n())

# smoke100_2016 #has NA
#cleaned_data_2016_no_NA %>%
  #group_by(smoke100_2016) %>%
  #summarise(n = n())

# pid7_2016 # no NA
#cleaned_data_2016_no_NA %>%
  #group_by(pid7_2016) %>%
  #summarise(n = n())

# "ideo5_2016" # only 6 NA
#cleaned_data_2016_no_NA %>%
  #group_by(ideo5_2016) %>%
  #summarise(n = n())

# "pew_religimp_2016" # no NA
#cleaned_data_2016_no_NA %>%
  #group_by(pew_religimp_2016) %>%
  #summarise(n = n())

# All of our variables are cleaned and factored!!
cd_2016_no_NA_factors <- cleaned_data_2016_no_NA %>%
  mutate(accurately_counted2_2016 = factor(accurately_counted2_2016, levels = c("Not at all confident","Not too confident",
                                                                                "Somewhat confident","Very confident"),exclude = NULL)) %>%
  mutate(alcohol_2016 = factor(alcohol_2016, levels = c("Yes","No"),exclude=NULL)) %>%
  mutate(smoke100_2016 = factor(smoke100_2016, levels = c("Yes","No"),exclude=NULL)) %>%
  mutate(pid7_2016 = factor(pid7_2016, levels = c("Strong Democrat","Lean Democrat","Not very strong Democrat","Not sure
                                                  ","Independent","Not very strong Republican","Lean Republican","Strong Republican"))) %>%
  mutate(ideo5_2016 = factor(ideo5_2016, levels = c("Very liberal","Liberal","Not sure","Moderate","Conservative","Very conservative"), exclude = NULL)) %>%
  mutate(pew_religimp_2016 = factor(pew_religimp_2016, levels = c("Not at all important","Not too important","Somewhat important","Very important"))) %>%
  mutate(presvote16post_2016 = factor(presvote16post_2016))

write_csv(cd_2016_no_NA_factors, file = "drafts/data/cd_2016.csv")
#```

#```{r}
#boasted_tune_res <- read_rds(file = "data/boasted_tune_res.rsd")
  

#cleaned_data_2016 %>%
  #separate(ft_black_2016, into = c('number', 'description'), sep = ' -') %>%
  #mutate(ft_black_2016 = as.numeric((number)))
#cleaned_data_2016 <- cleaned_data_2016 %>%
  #mutate(ft_white_2016 = as.numeric(ft_white_2016))
  
#cleaned_data_2016 <- cleaned_data_2016 %>%
 # mutate(ft_white_2016 = str_to_int(ft_white_2016))


# cleaning feeling thermometer questions 
#str_to_int <- function(x){
 # if (x == "0 - Unfavorable") {return(0)}
 # if (x == "25 -Unfavorable feeling") {return(25)}
 # if (x == "50 - No feeling at all") {return(50)}
 # if (x == "75 - Favorable feeling") {return(75)}
 # if (x == "100 - Favorable feeling") {return(100)}
 # if (x == "Don't Know") {return(2000)} # case where there is a "Don't Know 
 # else return(strtoi(x))
#}



  #select(-starts_with(c("PARTY","SOCIAL","race","sexism","pp","regzip","vote","wished","second","Sanders","Clinton")))

# do I want to work with Imiss data?

#save(pred_df, file = 'data/final-preds.RData')
#%>%
  #select(starts_with("ft")|starts_with("selfdescr_ccap"))
  
  #select(starts_with("ft")) %>%  
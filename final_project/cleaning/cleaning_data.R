library(readr)
library(tidyverse)
VOTER_Survey_December16_Release1 <- read_csv("pstat/131/pstat131-testing/final_project/archive/VOTER_Survey_December16_Release1.csv")
View(VOTER_Survey_December16_Release1)
#labels(VOTER_Survey_December16_Release1)
# selecting feeling thermometer data
cleaned_data <- VOTER_Survey_December16_Release1 %>%
  select("case_identifier","weight","presvote16post_2016",
         "ft_black_2016","ft_white_2016","ft_hisp_2016","ft_asian_2016","ft_muslim_2016",
         "ft_jew_2016","ft_christ_2016","ft_fem_2016","ft_immig_2016","ft_blm_2016","ft_wallst_2016","ft_gays_2016",
         "ft_unions_2016","ft_police_2016","ft_altright_2016")

# remove anyone who didn't vote


# remove anyone who didn't fill out parts of the poll


str_to_int <- function(x){
  if (x == "0 - Unfavorable") {return(0)}
  if (x == "25 -Unfavorable feeling") {return(25)}
  if (x == "50 - No feeling at all") {return(50)}
  if (x == "75 - Favorable feeling") {return(75)}
  if (x == "100 - Favorable feeling") {return(100)}
  if (is.na(x) == TRUE) {return(2000)} # case where there is NA
  if (x == "Don't Know") {return(2000)} # case where there is a "Don't Know 
  else return(strtoi(x))
}

y <- for (element in cleaned_data$ft_black_2016){
  str_to_int(element)
}

#map_dbl

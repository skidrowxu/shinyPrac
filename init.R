rm(list = ls(all.names = TRUE))

# load the required package
`%notin%` <- Negate(`%in%`)
lapply(c("shiny","ggplot2","tidyverse","plotly","dplyr","shinyWidgets","DT", "haven"), library, character.only = TRUE)

# read the ADaM datasets
adsl <- read_xpt("ADaM/adsl.xpt")
adlbc <- read_xpt("ADaM/adlbc.xpt")

# add the factor
adlbc_subset <- adlbc %>% select(AVISIT, AVISITN) %>% 
                  arrange(AVISITN) %>% distinct()
print(adlbc_subset)
print(adlbc$AVISIT)
adlbc$AVISIT <- factor(adlbc$AVISIT, levels = adlbc_subset$AVISIT)
print(adlbc)
print(adlbc$AVISIT)



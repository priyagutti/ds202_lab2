
data1 <- readxl::read_xls('C:/Data Science/DS202/GSS.xls')
library(ggplot2)
library(dplyr)
library(tidyverse)
View(data1)

##1. Find the average years of education for the respondents with each marital status. Arrange your output in a meaningful order and print. 
class(data1$`Highest year of school completed`)
highest_school_years <- as.numeric(data1$`Highest year of school completed`)
data2 <- data1 %>% 
  group_by(`Marital status`) %>%
  summarize(mean_year = mean(highest_school_years, na.rm = TRUE))
View(data2)
##2. Create a single data frame containing records for the better educated respondents with each marital status.
data3 <- filter(data1, `Marital status` == 'No answer' & `Highest year of school completed` > 12.9 | `Marital status` == 'Never married' & `Highest year of school completed` > 12.9
| `Marital status` == 'Divorced' & `Highest year of school completed` > 12.9 | `Marital status` == 'Married' & `Highest year of school completed` > 12.9 |
  `Marital status` == 'Seperated' & `Highest year of school completed` > 12.9 |
  `Marital status` == 'widowed' & 'Highest year of school completed' > 12.9 )
View(data3)

## 3. How is the happiness of a respondent related to his/her marriage status? Define that a person is happy if the response to question is "Very happy" or "Pretty happy"
data4 <- data1 %>% 
  filter(`General happiness` %in% c('Very happy', 'Pretty happy', 'Not too happy')) %>% 
  mutate(HappyOrNot = (`General happiness` %in% c('Very happy', 'Pretty happy'))) %>%
  group_by(`Marital status`)%>%
  select(`General happiness`, HappyOrNot, `Marital status`, `Political party affiliation` )

ggplot(data4, aes(x = `Marital status`, weight = HappyOrNot, fill = HappyOrNot)) + geom_bar()

View(data4)
##4. Does party affiliation affect the relationship you found in the last question?
View(data4)
ggplot(data4, aes(x = `Political party affiliation`, weight = HappyOrNot, fill = `Political party affiliation`)) + geom_bar()

##5. Explore an interesting question (not covered by the previous questions), and answer the question using the GSS data.
##How does years of completed education affect the labor force status? 
data5 <- data1 %>%
  filter(`Labor force status` %in% c('Working parttime', 'Working fulltime', 'Keeping house','school')) %>%
  mutate(WorkingOrNot = (`Labor force status` %in% c('Working parttime', 'Working fulltime', 'Keeping house','school'))) %>%
  select(`Labor force status`,`Marital status`, `Highest year of school completed`, WorkingOrNot) %>%
  group_by(`Labor force status`,na.rm = TRUE)
  View(data5)
ggplot(data5, aes(x = `Highest year of school completed`, weight = WorkingOrNot, fill = `Labor force status`, na.rm = TRUE)) + geom_bar()
##The bar chart shows the distribution for the number of years completed and labor force status. Based on the results, there is a significant count of people who completed 12 years of school. Around 3,000 people keep home and around 7,500 work full time.
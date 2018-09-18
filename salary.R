library(dplyr)
library(ggplot2)
library(tidyr)

exchange_rate = 0.77
sf_adjustment = 41/100
yvr_adjustment = 5.6/100

job <- c("Application Development Manager","Application Development Project Manager","Application Development System Analyst","Application Architect","Business System Analyst","Cloud Computing Analyst","CRM Business Analyst","CRM Technical Developer","Application Developer/Programmer Analyst","ERP Business Analyst","ERP Technical/Functional Analyst","ERP Technical Developer","Lead Application Developer", "Mobile Application Developer", "Technical Writer", # Application Development
         "Big Data Engineer","Database Manager","Database Developer","Database Administrator","Data Analyst/Report Writer","Data Architect","Data Modeler","Data Scientist","Data Warehouse Analyst","Business Intelligence Analyst","Data Reporting Analyst", #Data/Database Administration
         "QA Engineer - Manual","QA Engineer - Automated","QA/Testing Manager","QA Associate/Analyst", # QA and Testing
         "Sr. Web Developer","Web Developer","Front-End Web Developer","Web Administrator","Web Designer","E-Commerce Analyst","DevOps Engineer", # Web Development
         "Product Manager","Software Engineer","Software Developer") # Software Development

job <- append(job, job)

country <- append(rep("Canada",40), rep("US",40) )
ca_salary<- c(122500,112250,89750,127000,96250,95000,101750,103250,76000,115750,129750,128750,121000,79000,90750, # Application Development
              127500,120000,104000,81750,67750,116750,103000,108250,91750,98750,63750, # Data/Database Administration
              74250,85750,92500,80500, # QA and Testing
              101000,77500,76000,76250,69500,108000,108750, # Web Development
              95500,89000,78750) # Software Development

us_salary<- c(128500,111500,92750,135750,93250,94500,97000,107250,104500,102000,108750,114500,125000,143500,66500, # Application Development
              155500,129500,118000,97250,97500,133500,98500,121500,96250,106000,74000, # Data/Database Administration
              71500,84000,97250,74250, # QA and Testing
              119500,100250,79250,82250,88250,98500,110500, # Web Development
              121750,124500,117500) # Software Development

salary <- append(ca_salary, us_salary)

df <- data_frame(job, country, salary)
df <- df %>% 
  spread(country, salary) %>% 
  mutate(Canada = Canada * exchange_rate) %>% # Convert from Cdn to USD
  mutate(Vancouver = Canada * (1 + yvr_adjustment), `San Francisco` = US * (1+sf_adjustment) ) %>% # Adjustment based on cities 
  select(-Canada, -US) %>%
  gather(Vancouver, `San Francisco`, key=city, value=salary)

salary_plot <- df  %>%
  ggplot(aes(x=job, y=salary, fill=city)) + 
  geom_bar(stat="identity", width=0.5  ,position=position_dodge()) +
  xlab("") +
  ylab("Salary in US Dollars") +
  labs(title="Salary Comparison") +
  coord_flip() 

salary_plot

ggsave(filename="salaries.png", plot = salary_plot)




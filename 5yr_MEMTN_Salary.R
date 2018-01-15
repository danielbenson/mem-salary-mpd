## Examining a CSV generated from City of Memphis Employee Salary Data
## 2013-2017, with specific focus on the Police Services division.

# Load required libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(magrittr)


# Read in data

MPDSalary_5yr <- read.csv("Memphis 2013-2017 Salaries_MPD.csv")

MPDSalary_5yr$Annual_Salary <- as.numeric(gsub('[$,]', '',
                                               MPDSalary_5yr$Annual_Salary))

MPDSalary_5yr <- subset(MPDSalary_5yr, select = -c(Employee_Name))

# Inspect Data

str(MPDSalary_5yr)

levels(MPDSalary_5yr)

summary(MPDSalary_5yr)

head(MPDSalary_5yr)

MPDSalary_5yr[!complete.cases(MPDSalary_5yr),]

unique(MPDSalary_5yr$Job_Title)

# Generate a table to view number of persons in a given role by year.

Title_Count <- MPDSalary_5yr %>% 
        group_by(Job_Title, Year) %>% 
        tally()

# Generate a table to view number of individuals in Police Officer II role by
# year. You could also just create a new object using "filter" on Title_Count.

POII_Count <- MPDSalary_5yr %>% 
        group_by(Job_Title, Year) %>% 
        filter(Job_Title == "Police Officer II") %>%
        tally()

# Generate a table to view number of recruits by year. You could also just 
# create a new object using "filter" on Title_Count.

Recruit_Count <- MPDSalary_5yr %>% 
        group_by(Job_Title, Year) %>% 
        filter(Job_Title == "Police Recruit") %>%
        tally()

# Using Annual CPI values provided by data.bls.gov, apply crude cost-of-living
# adjustments from 2013 - 2017.

CPI_Base <- MPDSalary_5yr %>% 
        group_by(Job_Title) %>% 
        filter(Year == "2013") 

CPI_2014est <- transmute(CPI_Base, Annual_Salary = 
                                 (Annual_Salary + Annual_Salary*.015),
                         Year = 2014) 

CPI_2015est <- transmute(CPI_2014est, Annual_Salary = 
                                 (Annual_Salary + Annual_Salary*.016),
                         Year = 2015) 

CPI_2016est <- transmute(CPI_2015est, Annual_Salary = 
                                 (Annual_Salary + Annual_Salary*.001),
                         Year = 2016)

CPI_2017est <- transmute(CPI_2016est, Annual_Salary = 
                                 (Annual_Salary + Annual_Salary*.013),
                         Year = 2017)

CPISalary_5yrEst <- rbind(CPI_Base, CPI_2014est, CPI_2015est, CPI_2016est,
                          CPI_2017est)

# Create tables of averages for both the hypothetical CPI salaries, and the 
# actual salaries. Append "Basis" columns to each in preparation for facet-wrap
# plot.

CPISalary_5yrAVG <- CPISalary_5yrEst %>%
        group_by(Year) %>%
        summarize(Average_Salary = mean(Annual_Salary)) %>%
        mutate(Basis = "Hypothetical CPI Adjusted")

MPDSalary_5yrAVG <- MPDSalary_5yr %>%
        group_by(Year) %>%
        summarize(Average_Salary = mean(Annual_Salary))%>%
        mutate(Basis = "Actual")

# Combine tables of averages in preparation for facet-wrap plot.

Comparative_AVGs <- rbind(CPISalary_5yrAVG, MPDSalary_5yrAVG)

# Generate facet-wrap bar chart.

P1 <- ggplot(Comparative_AVGs, aes(y = Average_Salary, x = Year)) + 
        labs(y = "Average Salary") + 
        geom_bar(stat = "identity", fill = "blue") + 
        facet_wrap(~Basis) +
        scale_y_continuous(labels = scales::dollar_format("$"),
                           breaks= seq(0,60000, 5000))+
        ggtitle("Average Memphis, TN Police Services Salary by Year")

P1

ggsave("Average Memphis, TN Police Services Salary by Year.png",
       dpi=300, dev='png', height=7, width=11, units="in")

# Compare the actual increase in average salary to the hypothetical 
# cost-of-living adjustments made earlier based on CPI. We're basically 
# seeing if real adjustments/increases outpaced inflation. The code below
# throws a warning message about vector multiplicity but works just fine.

Real_Percent_Increase <- MPDSalary_5yrAVG %>%
        select(Year, Average_Salary) %>%
        mutate(Percent_Increase =
                       round(lag(100*(diff(MPDSalary_5yrAVG$Average_Salary)/
                                        MPDSalary_5yrAVG$Average_Salary)),2))%>%
        mutate(Applied_CPI = c("NA",1.5,1.6,0.1,1.3)) %>%
        replace(is.na(.), "NA") %>%
        mutate(Hypothetical_CPI_AvgSalaries = CPISalary_5yrAVG$Average_Salary)


## End

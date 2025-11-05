library(tidyverse)

p <- ggplot(mtcars, aes(x=factor(cyl)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal() 


p + annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -1, ymax = 12,
             alpha = 0, color= "green") +
  theme(axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(y = -0.5, x = factor(cyl), 
                label = cyl)) +
  labs(title="Rectangle over x axis!",
       x ="cyl", y = "count")

##################################

library(shiny)
library(tidyverse)
library(vroom)
library(DT)
library(wesanderson)
library(formattable)

dashboard <- vroom("data/ca_dashboard.csv") 

dashboard <- 
  dashboard %>% 
  mutate(statuslevel = as_factor(statuslevel),
         reportingyear = fct_relevel(as_factor(reportingyear), "2019"),
         indicator = fct_relevel(as_factor(indicator), 
                                 "ELA", "Math", "ELPI", 
                                 "absenteeism", "graduation", 
                                 "suspension", "college/career"),
         studentgroup = fct_relevel(as_factor(studentgroup),
                                    "ALL", "AA", "AI", "AS", "FI",
                                    "HI", "MR", "PI", "WH", 
                                    "EL", "ELO", "FRP",
                                    "HOM", "SED", "FOS", "SWD"),
         priority_eligible = as_factor(priority_eligible),
         charter_flag = if_else(is.na(charter_flag), "N", charter_flag),
         color = case_when(
           reportingyear == "2022" ~ statuslevel,
           .default = as_factor(color)))

dashboard_wide <- 
  dashboard %>% 
  filter(schoolname == "Benicia High") %>% 
  select(reportingyear, countyname, schoolname, charter_flag, student_group_long, 
       priority, indicator, color, 
       currstatus, priorstatus, change, priority_eligible) %>% 
  pivot_wider(names_from = c(priority, indicator), 
              values_from = c(currstatus, change, color),
              names_glue = "priority_{priority}_{indicator}_{.value}") %>% 
  mutate(priority_4_ELA = paste(priority_4_ELA_currstatus, priority_4_ELA_color),
         priority_4_Math = paste(priority_4_Math_currstatus, priority_4_Math_color),
         priority_4_ELPI = paste(priority_4_ELPI_currstatus, priority_4_ELPI_color),
         priority_4_ELPI = paste(priority_4_ELA_currstatus, priority_4_ELA_color),
         priority_5_graduation = paste(priority_5_graduation_currstatus, priority_5_graduation_color),
         priority_6_suspension = paste(priority_6_suspension_currstatus, priority_6_suspension_color),
         priority_8_college_career = paste(`priority_8_college/career_currstatus`, `priority_8_college/career_color`),) %>% 
  select(reportingyear:student_group_long, 
         starts_with("priority_4"), starts_with("priority_5"),
         starts_with("priority_6"), starts_with("priority_8"))

dashboard_color <- formatter('span',
                             style = x ~ style(

                             ))

formattable(dashboard_wide)


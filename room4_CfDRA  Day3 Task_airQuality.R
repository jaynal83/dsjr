


#####remove all list
rm(list=ls())

##### Load library
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readxl)

#####Load data
Air_Data <- read_excel("Air Quality Data.xlsx", sheet = "Dhaka", 
                       col_types = c("date", "text", "numeric", "numeric"))

#####Explore dataset
str(Air_Data)
names(Air_Data)
summary(Air_Data)

#####Remove missing values
Air_Data_nm <- Air_Data %>%
  drop_na()


#####Extract year and month
Air_Data_nm$Year <- format(Air_Data_nm$Date, "%Y")
Air_Data_nm$Month <- format(Air_Data_nm$Date, "%m")

#####Checking for completeness 
summary(Air_Data_nm)

#####Summarizing dataset by year and month
Air_Data_nmsum <- Air_Data_nm %>%
  group_by(Year, Month) %>%
  summarise(num_mean = mean(PM2.5), .groups = 'drop')

#####Converting month to numeric 
Air_Data_nmsum <- Air_Data_nmsum %>%
  mutate(Month = as.numeric(Month)) 

#####Pattern or trend graph for pollution
Air_Data_nmsum %>%
  ggplot() + 
  geom_line(aes(Month, num_mean, colour = Year), linewidth = 3) +
  labs(x = "", y = expression("Mean concentration of PM2.5 measured in  "*mu*g/m^3),
       title = "Trend of air polution") +
  theme_hc() +
  labs(color = "") +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("January", "February", "March", "April", "May", "June", 
                                "July", "August", "September", "October", "November", "December")) +
  scale_y_continuous(limits = c(0, 260), breaks = seq(0, 250, 50)) +
  theme(axis.text=element_text(size = 14, colour="black"), 
        axis.title=element_text(size = 14, face="bold"), 
        legend.text=element_text(size = 14, face="bold"),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        legend.position="top",
        axis.text.y = element_text( hjust = 1 )) 







library(tidyverse)
library(ggplot2)
library(ggrepel)

setwd("C:/Users/ranas/OneDrive/28 Aug 2019/Desktop/R/Smart City")

##Download data on no. of healthcare facilities in each ward of SMART CITY AGRA
read.csv("hosp.csv") ##Source: https://smartcities.gov.in/
##Download data on no. of households in each ward of SMART CITY AGRA
pop<-read.csv("D03-Households (1).csv") ##Source: https://smartcities.gov.in/

str(hosp)
str(pop)

##rename the variable Ward.Name
pop<-pop%>%
  rename(ward=Ward.Name)

##count number of healthcare facilities in each ward and save in hosp_ward

hosp_ward<-hosp%>%count(ward)

##Arrange the ward in descending order of number of healthcare facilities

hosp_ward<-arrange(hosp_ward,desc(n))

##merge the dataset hosp_ward and pop by ward

agra<-merge(hosp_ward,pop,by="ward")

##Plot No. of Healthcare facilities against the No. of Households

agra_p<-agra%>%
  ggplot(aes(Total.no..of.Households,n,color=ward))+
  geom_point()+
  geom_text_repel(aes(label=ward),max.overlaps = Inf)+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Households vs Healthcare Facility (Smart City Agra)")+
  xlab("No. of Households in the Ward")+
  ylab("No. of Healthcare Facilities in the Ward")+
  scale_x_continuous(breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000))+
  geom_smooth(method="lm", se=FALSE)

agra_p

##INFERENCE

##The chart shows that there are several wards where the healthcare facilities 
#are relatively fewer compared to the number of households in these 
#wards (Smart City Agra) while converse is also true. For example, 
#Noori Darwaza has nearly 8 healthcare facilities for around 2300 HHs 
#while Awas Vikas West only has 4 healthcare facilities for 7500 HHs. 
#Such insights from KI can help city infrastructure planning, resource 
#allocation and betterment of public services.


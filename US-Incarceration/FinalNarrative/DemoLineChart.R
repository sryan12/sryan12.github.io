require(tidyverse)
options(scipen = 999)

imprisonmentRate <- read_csv("IMPRISONMENTRATES.csv")%>%
  slice(1:11)%>%
  select(Year,Total,White:Asian)%>%
  slice(1,11)
imprisonmentRate

chartData <- imprisonmentRate %>%
  pivot_longer(
    cols=Total:Asian,
    names_to = "Category",
    values_to = "Rate"
  )
chartData

imprisonmentLinechart <- ggplot(chartData, aes(x=Year,y=Rate,color=Category,group=Category))+
  geom_line()+
  labs(title="While the Rate of Black Imprisonment Drops, Other Demographics Stay Stable",
       subtitle = "Rate of Imprisonment per 100,000 People by Race from 2012-2022")+
  scale_y_continuous(limits = c(0,1500),breaks=c(250,500,750,1000,1250,1500))+
  theme_minimal()
imprisonmentLinechart

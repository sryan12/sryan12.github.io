require(tidyverse)

prisonPopulation <- read_csv("stock.csv")%>%
  slice(1:11)%>%
  select(Year,Total)
prisonPopulation

prisonPopulationChart <- ggplot(prisonPopulation, aes(x = Year, y = Total, group=1)) +
  geom_line() +
  scale_y_continuous(limits = c(0,2000000))+
  labs(title = "Prison Population Rising Again Following Sharp Drop Caused by COVID",
       subtitle = "Total prison population between 2012-2022")+
  theme_minimal()
prisonPopulationChart

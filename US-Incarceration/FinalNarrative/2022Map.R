require(tidyverse)
require(sf)


imprisonmentRate2022 <- read_csv("incarceration_rate.csv")%>%
  select("Juristiction","Total...6")%>%
  slice(4:53)%>%
  mutate(
    bucket = cut(
      `Total...6`, 
      breaks = c(0,99, 199, 299, 399, 499,599,699,799,899),
      right = FALSE
    ))%>%
  arrange(desc(`Total...6`))
imprisonmentRate2022

states <- st_read("states")%>%
  filter(
    NAME != "Alaska" &
      NAME != "Hawaii" &
      NAME != "Puerto Rico" &
      NAME != "American Samoa" &
      NAME != "Guam" &
      NAME != "United States Virgin Islands" &
      NAME != "Commonwealth of the Northern Mariana Islands"
  )%>%
  arrange(NAME)
states

mapData2022 <- states %>%
  left_join(imprisonmentRate2022, 
            by = join_by(NAME == Juristiction))
mapData2022

prisonMap2022 <- ggplot(mapData2022, aes(fill = bucket),color=NA) +
  geom_sf() +
  coord_sf(crs = st_crs("EPSG:5070"))+
  geom_sf_text(data = mapData2022, aes(label = STUSPS), size = 2) +
  scale_fill_manual(values=c(
    "#fff7ec",
    "#fee8c8",
    "#fdd49e",
    "#fdbb84",
    "#fc8d59",
    "#ef6548",
    "#d7301f",
    "#b30000",
    "#7f0000",
    "#630000"
  ))+
  
  # "#fef0d9", "#fdd49e","#fdbb84","#fc8d59", "#ef6548","#d7301f", "#990000"))+
  labs(title = 'Incarceation Rate 2022',
       subtitle = "Incarceration Rate per 100 Thousand People",
       caption="Source: Bureau of Justice Statistics, Prisoners in 2022 – Statistical Tables") +
  theme_void()
prisonMap2022

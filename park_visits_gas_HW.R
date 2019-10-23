library(tidyverse)
library(dplyr)
library(ggthemes)
library(viridis)
library(colorRamps)
library(RColorBrewer)
library(ggrepel)


###Load in data

park_data<-read_csv('data/2019/2019-09-17/national_parks.csv')
head(park_data)

gas_price<-read_csv('data/2019/2019-09-17/gas_price.csv')
head(gas_price)

population<-read_csv('data/2019/2019-09-17/state_pop.csv')
View(population)

### US population

population%>%
  group_by(year)%>%
  summarize(pop=sum(pop,na.rm=TRUE))->US_pop
View(US_pop)

### calculate regional totals by year

park_data[c("year","region","visitors")]%>%
  group_by(region,year)%>%
  summarize(visitors=sum(visitors))%>%
  subset(year!="Total")->regional_total
regional_total

#spread regional totals

regional_total%>%spread(key=region, value=visitors)->regional_spread

#calculate US totals

regional_total%>%
  group_by(year)%>%
  summarize(visitors=sum(visitors))%>%
  rename(US = visitors)->US_total

###join US totals to US regions

left_join(US_total, regional_spread, by="year")%>%
  gather("US","AK","IM","MW","NC","NE","NT","PW","SE", key="region", value="visitors")%>%
  transform(year=as.numeric(year))->tidy_region

###join above to gas prices

full_join(tidy_region,gas_price,by="year",-gas_current)->"gas_and_visits"

### join above to US population

full_join(gas_and_visits,US_pop,by="year")%>%
  mutate("visit_per_cap"=visitors/pop)->"gas_visits_pop"

###add decade column

gas_visits_pop%>%
mutate("decade"=paste(floor(year/10)*10,'s',sep=""))->"visits_by_decade"

###subset US data

subset(visits_by_decade,region=="US"& year>1930)->subset_US
View(subset_US)

mycolors<-colorRampPalette(brewer.pal(9,"Set1"))(9)

###draw scatterplot

ggplot(data=subset_US,(aes(gas_constant,visit_per_cap, size=10)))+
         geom_point(aes(color=decade))+
  scale_color_brewer(palette="Set1")+
  guides(size="none")+
  xlab("gas price - inflation adjusted dollars")+
  ylab("US park visits per cap")+
  ggtitle("Impact of gas prices on US park visits per capita")+
  theme_clean()
   



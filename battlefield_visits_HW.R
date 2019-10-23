library(tidyverse)
library(dplyr)
library(ggthemes)
library(viridis)
library(colorRamps)
library(RColorBrewer)
library(ggrepel)

###Read in data

park_data<-read_csv('data/2019/2019-09-17/national_parks.csv')
head(park_data)

military_sites <- c("National Battlefield", "National Battlefield Park", "National Military Park")

### data description
#park_data shows visits by year to all US parks, dating back to eary 1900s, 
#broken out by region, year and park type

###Filter by military sites and calc percent visits

filter(park_data,unit_type %in% military_sites, year==2016)%>%
  mutate("share_visits"=visitors/sum(visitors))->percent_visits

###assign a state rank

"state_level"<-select(percent_visits, c("state", "visitors", "share_visits"))
state_level%>%
  group_by(state)%>%
  summarize(share_visits=sum(share_visits))%>%
  mutate("state_rank"=rank(-share_visits))->ranked_states
View(ranked_states)

###create a factor based on ranking

ranked_states$state <- factor(ranked_states$state, levels = ranked_states$state[order(-ranked_states$state_rank)])
ranked_states$state

###create custome color palette

mycolors<-colorRampPalette(brewer.pal(8,"Set1"))(14)

### plot bar chart

ggplot(ranked_states, aes(x="", y=share_visits, fill=state))+
  geom_bar(width = 1, stat ="identity")+
  geom_text(data = filter(ranked_states, share_visits*100 > 2), aes(x = 1.6, label = paste0(round(share_visits*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=mycolors,guide = guide_legend(reverse=TRUE))+
  coord_polar("y")+
  ggtitle("Share of visits, by state, of battlefields managed by NPS")+
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(axis.text=element_blank())+
  labs(caption = 
         "percentages not depicted for states comprising less than 2% of total")




library(tidyverse)
library(lingtypology)
library(wesanderson)
survey <- read_tsv("attitude.csv")

sex <- survey %>%
  select(sex)%>%
  group_by(sex)%>%
  summarize(sex_total = n())

ggplot(sex, aes(x=sex, y=total, fill=sex)) +
  geom_bar(stat="identity")+
  scale_y_continuous(breaks =c(0:10))+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="none")+
  scale_x_discrete(labels=c("Ж", "М"))

age <- survey %>%
  select(age_group)%>%
  group_by(age_group)%>%
  summarize(age_total = n())

ggplot(age, aes(x=age_group, y=age_total, fill=age_group)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="none")


birth_place <- survey %>%
  select(birth_place) %>%
  group_by(birth_place) %>%
  summarize(total = n()) %>%
  rename(place = birth_place)
birth_place$type <- "место рождения"

residence <- survey %>%
  select(residence) %>%
  group_by(residence) %>%
  summarize(total = n())%>%
  rename(place = residence)
residence$type <- "место проживания"

places <- rbind(birth_place, residence)

places_ru <- c("Ашино", "Ботлих", "Махачкала",
               "Ботлих", "Хасавюрт", "Кизляр",
               "Махачкала", "Москва")

places$place <- places_ru

places <- places %>%
mutate(place = fct_reorder(place, total))

ggplot(places, aes(x=place, y=total, fill=type)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.title=element_blank())+
  coord_flip()



important <- survey %>%
  select(`14. Считаете ли вы важным знать свой язык?`) %>%
  group_by(`14. Считаете ли вы важным знать свой язык?`) %>%
  summarize(total = n())%>%
  rename(q14 = `14. Считаете ли вы важным знать свой язык?`)

important$pct <- "100"
no <- c("нет", NA, "0")
important <- rbind(important, no)

ggplot(important, aes(x=q14, y=total, fill=q14)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("lightgoldenrod", "white"))+
  coord_polar("y", start=0)+
  theme_void()+
  theme(legend.title=element_blank())

ggplot(important, aes(x = "", y = total, fill = q14)) +
  geom_col() +
  coord_polar(theta = "y")

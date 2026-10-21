
library(tidyverse)
library(wesanderson)
library(ggplot2)

survey <- read_tsv("attitude.csv")

age <- survey %>%
  select(age_group)%>%
  group_by(age_group)%>%
  summarize(age_total = n())

ggplot(age, aes(x=age_group, y=age_total, fill=age_group)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank())

birth_place <- survey %>%
  select(birth_place) %>%
  group_by(birth_place) %>%
  summarize(total = n()) %>%
  rename(place = birth_place)
birth_place$type <- "birth place"

residence <- survey %>%
  select(residence) %>%
  group_by(residence) %>%
  summarize(total = n())%>%
  rename(place = residence)
residence$type <- "residence"

places <- rbind(birth_place, residence)%>%
  mutate(place = fct_reorder(place, total))

ggplot(places, aes(x=place, y=total, fill=type)) +
  geom_bar(stat="identity", position="fill")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
        coord_flip()

nation_general <- survey %>%
  select(nationality) %>%
  group_by(nationality) %>%
  summarize(total = n())
nation_general$context <- "general"

nation_botlikh <- survey %>%
  select(nationality_botlikh) %>%
  group_by(nationality_botlikh) %>%
  summarize(total = n())
nation_botlikh$context <- "botlikh"
colnames(nation_botlikh)[1] <- "nationality"

nation_mcx <- survey %>%
  select(nationality_makhachkala) %>%
  group_by(nationality_makhachkala) %>%
  summarize(total = n())
nation_mcx$context <- "makhachkala"
colnames(nation_mcx)[1] <- "nationality"

nation_msk <- survey %>%
  select(nationality_moscow) %>%
  group_by(nationality_moscow) %>%
  summarize(total = n())
nation_msk$context <- "moscow"
colnames(nation_msk)[1] <- "nationality"

nation_abroad <- survey %>%
  select(nationality_abroad) %>%
  group_by(nationality_abroad) %>%
  summarize(total = n())
nation_abroad$context <- "abroad"
colnames(nation_abroad)[1] <- "nationality"

nationality <- bind_rows(nation_general, nation_botlikh, nation_mcx,
                       nation_msk, nation_abroad)
  nationality$context <- factor(nationality$context, 
                                levels = c("general", "botlikh",
                                           "makhachkala", "moscow",
                                           "abroad"))

ggplot(nationality, aes(nationality, total, group = context, color = context)) +
  geom_line()+
  scale_color_manual(values = wes_palette("Moonrise3"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x = element_text(angle = 50, vjust = 1, hjust=1))

botlikh_ethnos <- survey %>%
  select(botlikh_ethnos) %>%
  group_by(botlikh_ethnos) %>%
  summarize(total = n())

ggplot(botlikh_ethnos, aes(x=botlikh_ethnos, y=total, fill=botlikh_ethnos)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank())

family_botlikh <- survey %>%
  select(family_botlikh) %>%
  group_by(family_botlikh) %>%
  summarize(total = n())
colnames(family_botlikh)[1] <- "language"

family_avar <- survey %>%  
  select(family_avar) %>%
  group_by(family_avar) %>%
  summarize(total = n())
colnames(family_avar)[1] <- "language"

family_russian <- survey %>%  
  select(family_russian) %>%
  group_by(family_russian) %>%
  summarize(total = n())
colnames(family_russian)[1] <- "language"

language_family <- bind_rows(family_botlikh, family_avar, family_russian)%>%
  filter(!is.na(language))%>%
  mutate(language = fct_reorder(language, total))

ggplot(language_family, aes(x=language, y=total, fill=language)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank())+
  coord_flip()

mixedmarr <- survey %>%
  select(mixed_languages) %>%
  group_by(mixed_languages) %>%
  summarize(total = n()) %>%
  mutate(language = fct_reorder(mixed_languages, total))

ggplot(mixedmarr, aes(x=mixed_languages, y=total, fill=mixed_languages)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = wes_palette("Royal2"))+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="none")+
  coord_flip()


s1 <- survey %>%
  rename(s25 = `25. Важно уметь говорить на своем языке.`)%>%
  select(s25)%>%
  group_by(s25)%>%
  summarise(score = n())

s25 <- 1:4
score <- c(0, 0, 0, 0)

empties <- as.data.frame(cbind(s25, score))

s1 <- rbind(s1, empties)

ggplot(s1, aes(x=s25, y=score)) +
  geom_bar(stat="identity")+
  theme_classic()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        legend.position="none")


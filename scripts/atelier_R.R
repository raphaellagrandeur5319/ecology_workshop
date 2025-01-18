library(tidyverse)
library(ratdat)

### exploration de donnée
?complete_old
summary(complete_old)
head(complete_old)
str(complete_old)

###ggplot
library(ggplot2)

       
ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length))+geom_point()   

complete_old <- filter(complete_old, !is.na(weight) & !is.na(hindfoot_length))
### ! exclue ce l'argument indiqueé ci-après

ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length))+geom_point(alpha=0.1, color ="blue")   

ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length, color=plot_type))+geom_point()   
  #color=plot_type commw une 3e variable, mes couleurs différente pour chaque traitement

ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length, shape=sex, ))+geom_point() 
  #2 conditions shape= ET color=

ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length, color=year))+geom_point() 

transgenre<-filter(complete_old, !sex=="M", !sex=="F")

ggplot(complete_old, mapping=aes(x=weight, y=hindfoot_length, color=year))+
geom_point()+
scale_color_viridis_d() +
scale_x_log10()

ggplot(complete_old, mapping=aes(x=plot_type, hindfoot_length, color=plot_type))+
  geom_boxplot() +
  geom_jitter(alpha=0.5) +
scale_x_discrete(labels=label_wrap_gen(width=10))

ggplot(complete_old, mapping=aes(x=plot_type, hindfoot_length))+
  geom_boxplot() +
  geom_jitter(alpha=0.5, aes(color=plot_type)) +
  scale_x_discrete(labels=label_wrap_gen(width=10))

ggplot(complete_old, mapping=aes(x=plot_type, hindfoot_length))+
  geom_jitter(alpha=0.2, aes(color=plot_type)) + geom_violin(fill=NA)+
  scale_x_discrete(labels=label_wrap_gen(width=10))
#Les premiers arguments sont les premiers excécuté: pour mettre les boxplot en avant plnats, il faut les mettre en dernier

ggplot(complete_old, mapping=aes(x=plot_type, hindfoot_length))+
  geom_boxplot() +
  geom_jitter(alpha=0.5, aes(color=plot_type)) +
  scale_x_discrete(labels=label_wrap_gen(width=10)) + 
  theme_bw() + theme(legend.position = "none")+
  labs(x= "Plot type", y = "hindfootlenght (mm)")
#theme_bw = thème par défgautl ; theme=changer des choses spécifiques dans le thèmes

ggplot(complete_old, mapping=aes(x=plot_type, hindfoot_length))+
  geom_boxplot() +
  facet_wrap(vars(sex, ncol=1))+
  geom_jitter(alpha=0.5, aes(color=plot_type)) +
  scale_x_discrete(labels=label_wrap_gen(width=10)) + 
  theme_bw() + theme(legend.position = "none")+
  labs(x= "Plot type", y = "hindfootlenght (mm)")
#facet_wrap = créer de multiple graphique avec la vraible sex

ggsave(filename = "figures/plot_final.png", height=6, width =8)
  #sauvegarde le graphique


########Tidyverse!!!

read_csv(data/raw/"data/raw/surveys_complete_77_89.csv")
  ###La touche tab termine tout seul ne nomdu fichier à importer
 #select() : prendre colonnes ; filter() sélectionn des ligne ; mutate() créer des colone ; group by() ; summuryse()

select(surveys,c(3,4))
select(surveys, -plot_id)
### "-" exclue la colone indiqué
select(surveys, where(is.numeric))
select(surveys, where(anyNA))    

filter(surveys, year==1988)
filter(surveys, species %in% c("RM", "DO"))
filter(surveys, year==1988 & species %in% c("RM", "DO"))

object<-filter(surveys, year >= 1980 & year<=1985)
select(object, year, month, species_id, plot_id)
 
select(filter(object, year, month, species_id, plot_id),year, month, species_id, plot_id)

surveys %>% 
  filter(year == 1980:1985) %>% 
  select(year, month, species_id, plot_id)
  #controle+shift+M donne "%>%" : emboite les arguments précédents (filter s'applique sur surveys, select sur filter et surveys), remplace l'Emboitement de parenthèse

surveys %>% 
  filter(year == 1988) %>% 
  select(record_id, month, species_id)

surveys %>% 
  mutate(weight_kg=weight/1000) %>% 
  relocate(weight_kg, .after=record_id)

surveys %>% 
  mutate(weight_kg=weight/1000,
         weight_lbs=weight_kg) %>% 
  relocate(weight_kg,.after=record_id) %>% 
  relocate(weight_lbs, after=record_id )

library(lubridate)
surveys %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  relocate(date, .after =year)

surveys %>% 
  mutate(date=ymd(paste(year, month, day, sep="-"))) %>% 
  group_by(sex,date)%>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=date, y=count, color=sex))+geom_line()

